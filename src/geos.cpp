#include <ogr_geometry.h>

#define GEOS_USE_ONLY_R_API // avoid using non-thread-safe GEOSxx functions without _r extension.
#include <geos_c.h>

#include <Rcpp.h>

#include "gdal.h"

std::vector<GEOSGeom> geometries_from_sfc(GEOSContextHandle_t hGEOSCtxt, Rcpp::List sfc) {
	std::vector<OGRGeometry *> ogr = ogr_from_sfc(sfc, NULL);
	std::vector<GEOSGeom> g(ogr.size());
	for (int i = 0; i < ogr.size(); i++) {
		g[i] = ogr[i]->exportToGEOS(hGEOSCtxt);
		delete ogr[i];
	}
	return g;
}

Rcpp::List sfc_from_geometry(GEOSContextHandle_t hGEOSCtxt, std::vector<GEOSGeom> geom) {
	std::vector<OGRGeometry *> ogr(geom.size());
	for (int i = 0; i < geom.size(); i++) {
		ogr[i] = OGRGeometryFactory::createFromGEOS(hGEOSCtxt, geom[i]);
		GEOSGeom_destroy_r(hGEOSCtxt, geom[i]);
	}
	return sfc_from_ogr(ogr, true); // destroy
}

Rcpp::NumericVector get_dim(double dim0, double dim1) {
	Rcpp::NumericVector dim(2);
	dim(0) = dim0;
	dim(1) = dim1;
	return dim;
}

Rcpp::IntegerVector get_which(Rcpp::LogicalVector row) {
	int j = 0;
	for (int i = 0; i < row.length(); i++)
		if (row(i))
			j++;
	Rcpp::IntegerVector ret(j);
	for (int i = 0, j = 0; i < row.length(); i++)
		if (row(i))
			ret(j++) = i + 1; // R is 1-based
	return ret;
}

bool chk_(char value) {
	if (value == 2)
		throw std::range_error("GEOS exception");
	return value == 1;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_binop(Rcpp::List sfc0, Rcpp::List sfc1, std::string op, double par = 0.0, 
		bool sparse = true) {

	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();

	std::vector<GEOSGeom> gmv0 = geometries_from_sfc(hGEOSCtxt, sfc0);
	std::vector<GEOSGeom> gmv1 = geometries_from_sfc(hGEOSCtxt, sfc1);

	Rcpp::List ret_list;

	using namespace Rcpp; // so that later on the (i,_) works
	if (op == "relate") { // character return matrix:
		Rcpp::CharacterVector out(sfc0.length() * sfc1.length());
		for (int i = 0; i < sfc0.length(); i++)
			for (int j = 0; j < sfc1.length(); j++) {
				char *cp = GEOSRelate_r(hGEOSCtxt, gmv0[i], gmv1[j]);
				if (cp == NULL)
					throw std::range_error("GEOS error in GEOSRelate_r");
				out[j * sfc0.length() + i] = cp;
				GEOSFree_r(hGEOSCtxt, cp);
			}
		out.attr("dim") = get_dim(sfc0.length(), sfc1.length());
		ret_list = Rcpp::List::create(out);
	} else if (op == "distance") { // return double matrix:
		Rcpp::NumericMatrix out(sfc0.length(), sfc1.length());
		for (int i = 0; i < gmv0.size(); i++)
			for (int j = 0; j < gmv1.size(); j++) {
				double dist = -1.0;
				if (GEOSDistance_r(hGEOSCtxt, gmv0[i], gmv1[j], &dist) == 0)
					throw std::range_error("GEOS error in GEOSDistance_r");
				out(i,j) = dist;
			}
		ret_list = Rcpp::List::create(out);
	} else {
		// other cases: boolean return matrix, either dense or sparse
		Rcpp::LogicalMatrix densemat;
		if (! sparse)  // allocate:
			densemat = Rcpp::LogicalMatrix(sfc0.length(), sfc1.length());
		Rcpp::List sparsemat(sfc0.length());
		for (int i = 0; i < sfc0.length(); i++) { // row
		// TODO: speed up contains, containsproperly, covers, and intersects with prepared geometry i
			Rcpp::LogicalVector rowi(sfc1.length()); 
			if (op == "intersects")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSIntersects_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "disjoint")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSDisjoint_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "touches")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSTouches_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "crosses")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSCrosses_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "within")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSWithin_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "contains")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSContains_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "overlaps")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSOverlaps_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "equals")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSEquals_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "covers")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSCovers_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "covered_by")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSCoveredBy_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "equals_exact")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSEqualsExact_r(hGEOSCtxt, gmv0[i], gmv1[j], par));
			/* else if (op == "is_within_distance") ==>> no C interface??
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = gmv0[i]->isWithinDistance(gmv1[j].get(), par);
			*/
			else
				throw std::range_error("wrong value for op"); // unlikely to happen unless user wants to
			if (! sparse)
				densemat(i,_) = rowi;
			else
				sparsemat[i] = get_which(rowi);
		}
		if (sparse)
			ret_list = sparsemat;
		else
			ret_list = Rcpp::List::create(densemat);
	}
	for (int i = 0; i < gmv0.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, gmv0[i]);
	for (int i = 0; i < gmv1.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, gmv1[i]);
	OGRGeometry::freeGEOSContext(hGEOSCtxt);
	return ret_list;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_valid(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();
	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc);
	Rcpp::LogicalVector out(gmv.size());
	for (int i = 0; i < out.length(); i++) {
		out[i] = chk_(GEOSisValid_r(hGEOSCtxt, gmv[i]));
		GEOSGeom_destroy_r(hGEOSCtxt, gmv[i]);
	}
	OGRGeometry::freeGEOSContext(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_simple(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();
	Rcpp::LogicalVector out(sfc.length());
	std::vector<GEOSGeom> g = geometries_from_sfc(hGEOSCtxt, sfc);
	for (size_t i = 0; i < g.size(); i++) {
		out[i] = chk_(GEOSisSimple_r(hGEOSCtxt, g[i]));
		GEOSGeom_destroy_r(hGEOSCtxt, g[i]);
	}
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_union(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();
	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc);
	GEOSGeom gc = GEOSGeom_createCollection_r(hGEOSCtxt, GEOS_GEOMETRYCOLLECTION, gmv.data(), gmv.size());
	std::vector<GEOSGeom> gmv_out(1);
#if GEOS_VERSION_MAJOR >= 3 && GEOS_VERSION_MINOR >= 3
	gmv_out[0] = GEOSUnaryUnion_r(hGEOSCtxt, gc);
#else
	gmv_out[0] = GEOSUnionCascaded_r(hGEOSCtxt, gc);
#endif
	GEOSGeom_destroy_r(hGEOSCtxt, gc);
	Rcpp::List out(sfc_from_geometry(hGEOSCtxt, gmv_out)); // destroys gmv_out
	OGRGeometry::freeGEOSContext(hGEOSCtxt);
	return out;
}


GEOSGeometry *chkNULL(GEOSGeometry *value) {
	if (value == NULL)
		throw std::range_error("GEOS exception");
	return value;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_op(std::string op, Rcpp::List sfc, 
		double bufferDist = 0.0, int nQuadSegs = 30,
		double dTolerance = 0.0, bool preserveTopology = false, 
		int bOnlyEdges = 1, double dfMaxLength = 0.0) {

	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();
	std::vector<GEOSGeom> g = geometries_from_sfc(hGEOSCtxt, sfc);
	std::vector<GEOSGeom> out(sfc.length());

	// std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	// std::vector<OGRGeometry *> out(g.size());

	if (op == "buffer") {
		for (size_t i = 0; i < g.size(); i++)
			// out[i] = g[i]->Buffer(bufferDist, nQuadSegs);
			out[i] = chkNULL(GEOSBuffer_r(hGEOSCtxt, g[i], bufferDist, nQuadSegs));
	} else if (op == "boundary") {
		for (size_t i = 0; i < g.size(); i++)
			// out[i] = g[i]->Boundary();
			out[i] = chkNULL(GEOSBoundary_r(hGEOSCtxt, g[i]));
	} else if (op == "convex_hull") {
		for (size_t i = 0; i < g.size(); i++)
			// out[i] = g[i]->ConvexHull();
			out[i] = chkNULL(GEOSConvexHull_r(hGEOSCtxt, g[i]));
	} else if (op == "union_cascaded") {
		for (size_t i = 0; i < g.size(); i++)
			// out[i] = g[i]->UnionCascaded();
			out[i] = chkNULL(GEOSUnionCascaded_r(hGEOSCtxt, g[i]));
	} else if (op == "simplify") {
		for (size_t i = 0; i < g.size(); i++)
			// out[i] = preserveTopology ?  g[i]->SimplifyPreserveTopology(dTolerance) : 
			//		g[i]->Simplify(dTolerance);
			out[i] = preserveTopology ? chkNULL(GEOSTopologyPreserveSimplify_r(hGEOSCtxt, g[i], dTolerance)) :
					chkNULL(GEOSSimplify_r(hGEOSCtxt, g[i], dTolerance));
	} else if (op == "polygonize") {
		for (size_t i = 0; i < g.size(); i++)
			// out[i] = g[i]->Polygonize();
			out[i] = chkNULL(GEOSPolygonize_r(hGEOSCtxt, &(g[i]), 1)); // xxx
	} else if (op == "centroid") {
		for (size_t i = 0; i < g.size(); i++) {
			// OGRPoint *gm = new OGRPoint;
			// g[i]->Centroid(gm);
			// out[i] = gm;
			out[i] = chkNULL(GEOSGetCentroid_r(hGEOSCtxt, g[i]));
		}
	} else
#if GEOS_VERSION_MAJOR >= 3 && GEOS_VERSION_MINOR >= 4
	if (op == "triangulate") {
		for (size_t i = 0; i < g.size(); i++)
			// out[i] = g[i]->DelaunayTriangulation(dTolerance, bOnlyEdges);
			out[i] = chkNULL(GEOSDelaunayTriangulation_r(hGEOSCtxt, g[i], dTolerance, bOnlyEdges));
	} else
#endif
		throw std::invalid_argument("invalid operation"); // would leak g and out

	for (size_t i = 0; i < g.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, g[i]);

	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out)); // destroys out
	OGRGeometry::freeGEOSContext(hGEOSCtxt);
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_op2(std::string op, Rcpp::List sfc, Rcpp::List sf0) {

	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();
	std::vector<GEOSGeom> g = geometries_from_sfc(hGEOSCtxt, sfc);
	std::vector<GEOSGeom> g0 = geometries_from_sfc(hGEOSCtxt, sf0);
	std::vector<GEOSGeom> out(sfc.length());

	if (op == "intersection") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSIntersection_r(hGEOSCtxt, g[i], g0[0]));
	} else if (op == "union") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSUnion_r(hGEOSCtxt, g[i], g0[0]));
	} else if (op == "difference") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSDifference_r(hGEOSCtxt, g[i], g0[0]));
	} else if (op == "sym_difference") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSSymDifference_r(hGEOSCtxt, g[i], g0[0]));
	} else 
		throw std::invalid_argument("invalid operation"); // would leak g, g0 and out
	// clean up:
	for (size_t i = 0; i < g.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, g[i]);
	for (size_t i = 0; i < g0.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, g0[i]);

	/* old GDAL impl:
	OGRGeometryFactory f;
	for (size_t i = 0; i < out.size(); i++)
		if (out[i] == NULL)
			out[i] = f.createGeometry(wkbGeometryCollection);
	*/
	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out)); // destroys out
	OGRGeometry::freeGEOSContext(hGEOSCtxt);
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}

// [[Rcpp::export]]
std::string CPL_geos_version(bool b = false) {
	return GEOS_VERSION;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix CPL_geos_dist(Rcpp::List sfc0, Rcpp::List sfc1) {
	Rcpp::NumericMatrix out = CPL_geos_binop(sfc0, sfc1, "distance", 0.0, false)[0];
	return out;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_geos_relate(Rcpp::List sfc0, Rcpp::List sfc1) {
	Rcpp::CharacterVector out = CPL_geos_binop(sfc0, sfc1, "relate", 0.0, false)[0];
	return out;	
}

static void __errorHandler(const char *fmt, ...) {

    char buf[BUFSIZ], *p;
    va_list(ap);
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

	Rcpp::Function error("error");
    error(buf);

    return;
}

static void __warningHandler(const char *fmt, ...) {

    char buf[BUFSIZ], *p;
    va_list(ap);
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

	Rcpp::Function warning("warning");
	warning(buf);
    
    return;
}

GEOSContextHandle_t geos_ctxt_ptr;  // global variable -- can it do any harm?

// [[Rcpp::export]]
void CPL_geos_init() {
	// geos_ctxt_ptr = GEOS_init_r();
	geos_ctxt_ptr =
    	initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
}

// [[Rcpp::export]]
void CPL_geos_finish() {
	// GEOS_finish_r(geos_ctxt_ptr); // needs context handler, we don't have one
	finishGEOS_r(geos_ctxt_ptr);
}
