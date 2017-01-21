#define GEOS_USE_ONLY_R_API // avoid using non-thread-safe GEOSxx functions without _r extension.
#include <geos_c.h>

#include <Rcpp.h>

#include "wkb.h"

static void __errorHandler(const char *fmt, ...) { // #nocov start

    char buf[BUFSIZ], *p;
    va_list(ap);
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

	Rcpp::Function error("stop");
    error(buf);

    return; // #nocov end
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

GEOSContextHandle_t CPL_geos_init(void) {
    return initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
}

void CPL_geos_finish(GEOSContextHandle_t ctxt) {
	finishGEOS_r(ctxt);
}

std::vector<GEOSGeom> geometries_from_sfc(GEOSContextHandle_t hGEOSCtxt, Rcpp::List sfc) {
	double precision = sfc.attr("precision");
	Rcpp::List wkblst = CPL_write_wkb(sfc, false, native_endian(), "XY", precision);
	std::vector<GEOSGeom> g(sfc.size());
	for (int i = 0; i < sfc.size(); i++) {
		Rcpp::RawVector r = wkblst[i];
		g[i] = GEOSGeomFromWKB_buf_r(hGEOSCtxt, &(r[0]), r.size());
	}
	return g;
}

Rcpp::List sfc_from_geometry(GEOSContextHandle_t hGEOSCtxt, std::vector<GEOSGeom> geom) {
	Rcpp::List out(geom.size());
	for (size_t i = 0; i < geom.size(); i++) {
		size_t size;
		unsigned char *buf = GEOSGeomToWKB_buf_r(hGEOSCtxt, geom[i], &size);
		Rcpp::RawVector raw(size);
		memcpy(&(raw[0]), buf, size);
		free(buf);
		out[i] = raw;
		GEOSGeom_destroy_r(hGEOSCtxt, geom[i]);
	}
	return CPL_read_wkb(out, false, native_endian());
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
		throw std::range_error("GEOS exception"); // #nocov
	return value; // 1: true, 0: false
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_binop(Rcpp::List sfc0, Rcpp::List sfc1, std::string op, double par = 0.0, 
		bool sparse = true) {

	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

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
					throw std::range_error("GEOS error in GEOSRelate_r"); // #nocov
				out[j * sfc0.length() + i] = cp;
				GEOSFree_r(hGEOSCtxt, cp);
			}
		out.attr("dim") = get_dim(sfc0.length(), sfc1.length());
		ret_list = Rcpp::List::create(out);
	} else if (op == "distance") { // return double matrix:
		Rcpp::NumericMatrix out(sfc0.length(), sfc1.length());
		for (size_t i = 0; i < gmv0.size(); i++)
			for (size_t j = 0; j < gmv1.size(); j++) {
				double dist = -1.0;
				if (GEOSDistance_r(hGEOSCtxt, gmv0[i], gmv1[j], &dist) == 0)
					throw std::range_error("GEOS error in GEOSDistance_r"); // #nocov
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
	for (size_t i = 0; i < gmv0.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, gmv0[i]);
	for (size_t i = 0; i < gmv1.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, gmv1[i]);
	CPL_geos_finish(hGEOSCtxt);
	return ret_list;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_valid(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc);
	Rcpp::LogicalVector out(gmv.size());
	for (int i = 0; i < out.length(); i++) {
		out[i] = chk_(GEOSisValid_r(hGEOSCtxt, gmv[i]));
		GEOSGeom_destroy_r(hGEOSCtxt, gmv[i]);
	}
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_simple(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	Rcpp::LogicalVector out(sfc.length());
	std::vector<GEOSGeom> g = geometries_from_sfc(hGEOSCtxt, sfc);
	for (size_t i = 0; i < g.size(); i++) {
		out[i] = chk_(GEOSisSimple_r(hGEOSCtxt, g[i]));
		GEOSGeom_destroy_r(hGEOSCtxt, g[i]);
	}
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_union(Rcpp::List sfc, bool by_feature = false) { 
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc);
	std::vector<GEOSGeom> gmv_out(by_feature ? sfc.size() : 1);
	if (by_feature) {
		for (int i = 0; i < sfc.size(); i++)
#if GEOS_VERSION_MAJOR >= 3 && GEOS_VERSION_MINOR >= 3
			gmv_out[i] = GEOSUnaryUnion_r(hGEOSCtxt, gmv[i]);
#else
			gmv_out[i] = GEOSUnionCascaded_r(hGEOSCtxt, gmv[i]);
#endif
	} else {
		GEOSGeom gc = GEOSGeom_createCollection_r(hGEOSCtxt, GEOS_GEOMETRYCOLLECTION, gmv.data(), gmv.size());
#if GEOS_VERSION_MAJOR >= 3 && GEOS_VERSION_MINOR >= 3
		gmv_out[0] = GEOSUnaryUnion_r(hGEOSCtxt, gc);
#else
		gmv_out[0] = GEOSUnionCascaded_r(hGEOSCtxt, gc);
#endif
		GEOSGeom_destroy_r(hGEOSCtxt, gc);
	}
	Rcpp::List out(sfc_from_geometry(hGEOSCtxt, gmv_out)); // destroys gmv_out
	CPL_geos_finish(hGEOSCtxt);
	out.attr("precision") = sfc.attr("precision");
	out.attr("crs") = sfc.attr("crs");
	return out;
}


GEOSGeometry *chkNULL(GEOSGeometry *value) {
	if (value == NULL)
		throw std::range_error("GEOS exception"); // #nocov
	return value;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_op(std::string op, Rcpp::List sfc, 
		double bufferDist = 0.0, int nQuadSegs = 30,
		double dTolerance = 0.0, bool preserveTopology = false, 
		int bOnlyEdges = 1, double dfMaxLength = 0.0) {

	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init(); 

	std::vector<GEOSGeom> g = geometries_from_sfc(hGEOSCtxt, sfc);
	std::vector<GEOSGeom> out(sfc.length());

	if (op == "buffer") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSBuffer_r(hGEOSCtxt, g[i], bufferDist, nQuadSegs));
	} else if (op == "boundary") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSBoundary_r(hGEOSCtxt, g[i]));
	} else if (op == "convex_hull") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSConvexHull_r(hGEOSCtxt, g[i]));
	} else if (op == "union_cascaded") {
		for (size_t i = 0; i < g.size(); i++) // #nocov
			out[i] = chkNULL(GEOSUnionCascaded_r(hGEOSCtxt, g[i])); // #nocov
	} else if (op == "simplify") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = preserveTopology ? chkNULL(GEOSTopologyPreserveSimplify_r(hGEOSCtxt, g[i], dTolerance)) :
					chkNULL(GEOSSimplify_r(hGEOSCtxt, g[i], dTolerance));
	} else if (op == "linemerge") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSLineMerge_r(hGEOSCtxt, g[i]));
	} else if (op == "polygonize") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSPolygonize_r(hGEOSCtxt, &(g[i]), 1)); // xxx
	} else if (op == "centroid") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = chkNULL(GEOSGetCentroid_r(hGEOSCtxt, g[i]));
		}
	} else
#if GEOS_VERSION_MAJOR >= 3 && GEOS_VERSION_MINOR >= 4
	if (op == "triangulate") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSDelaunayTriangulation_r(hGEOSCtxt, g[i], dTolerance, bOnlyEdges));
	} else
#endif
		throw std::invalid_argument("invalid operation"); // would leak g and out // #nocov

	for (size_t i = 0; i < g.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, g[i]);

	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out)); // destroys out
	CPL_geos_finish(hGEOSCtxt);
	ret.attr("precision") = sfc.attr("precision");
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}

GEOSGeometry *chkNULLcnt(GEOSContextHandle_t hGEOSCtxt, GEOSGeometry *value, size_t *n) {
	if (value == NULL)
		throw std::range_error("GEOS exception"); // #nocov
	if (!chk_(GEOSisEmpty_r(hGEOSCtxt, value)))
		*n = *n + 1;
	return value;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_op2(std::string op, Rcpp::List sfcx, Rcpp::List sfcy) {

	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GEOSGeom> x = geometries_from_sfc(hGEOSCtxt, sfcx);
	std::vector<GEOSGeom> y = geometries_from_sfc(hGEOSCtxt, sfcy);
	std::vector<GEOSGeom> out(x.size() * y.size());

	size_t n = 0;
	if (op == "intersection") {
		for (size_t i = 0; i < y.size(); i++)
			for (size_t j = 0; j < x.size(); j++)
				out[i * x.size() + j] = chkNULLcnt(hGEOSCtxt, GEOSIntersection_r(hGEOSCtxt, x[j], y[i]), &n);
	} else if (op == "union") {
		for (size_t i = 0; i < y.size(); i++)
			for (size_t j = 0; j < x.size(); j++)
				out[i * x.size() + j] = chkNULLcnt(hGEOSCtxt, GEOSUnion_r(hGEOSCtxt, x[j], y[i]), &n);
	} else if (op == "difference") {
		for (size_t i = 0; i < y.size(); i++)
			for (size_t j = 0; j < x.size(); j++)
				out[i * x.size() + j] = chkNULLcnt(hGEOSCtxt, GEOSDifference_r(hGEOSCtxt, x[j], y[i]), &n);
	} else if (op == "sym_difference") {
		for (size_t i = 0; i < y.size(); i++)
			for (size_t j = 0; j < x.size(); j++)
				out[i * x.size() + j] = chkNULLcnt(hGEOSCtxt, GEOSSymDifference_r(hGEOSCtxt, x[j], y[i]), &n);
	} else 
		throw std::invalid_argument("invalid operation"); // would leak g, g0 and out // #nocov
	// clean up x and y:
	for (size_t i = 0; i < x.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, x[i]);
	for (size_t i = 0; i < y.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, y[i]);
	// trim results back to non-empty geometries:
	std::vector<GEOSGeom> out2(n);
	Rcpp::NumericMatrix m(n, 2); // and a set of 1-based indices to x and y
	size_t k = 0, l = 0;
	for (size_t i = 0; i < y.size(); i++) {
		for (size_t j = 0; j < x.size(); j++) {
			l = i * x.size() + j;
			if (!chk_(GEOSisEmpty_r(hGEOSCtxt, out[l]))) { // keep:
				out2[k] = out[l];
				m(k, 0) = j + 1;
				m(k, 1) = i + 1;
				k++;
				if (k > n)
					throw std::range_error("invalid k");
			} else // discard:
				GEOSGeom_destroy_r(hGEOSCtxt, out[l]);
		}
	}
	if (k != n)
		throw std::range_error("invalid k, check 2"); // #nocov

	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out2)); // destroys out2
	CPL_geos_finish(hGEOSCtxt);
	ret.attr("crs") = sfcx.attr("crs");
	ret.attr("idx") = m;
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
