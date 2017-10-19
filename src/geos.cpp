#define GEOS_USE_ONLY_R_API // prevents using non-thread-safe GEOSxx functions without _r extension.
#include <geos_c.h>

#if GEOS_VERSION_MAJOR == 3 && GEOS_VERSION_MINOR >= 5
#  define HAVE350
#else
# if GEOS_VERSION_MAJOR > 3
#  define HAVE350
# endif
#endif

#include <Rcpp.h>

#include "wkb.h"

typedef char (* log_fn)(GEOSContextHandle_t, const GEOSGeometry *, const GEOSGeometry *);
typedef char (* log_prfn)(GEOSContextHandle_t, const GEOSPreparedGeometry *, 
	const GEOSGeometry *);
typedef GEOSGeom (* geom_fn)(GEOSContextHandle_t, const GEOSGeom, const GEOSGeom);

void cb(void *item, void *userdata) { // callback function for tree selection
	std::vector<size_t> *ret = (std::vector<size_t> *) userdata;
	ret->push_back(*((size_t *) item));
}

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

// #nocov start
static void __countErrorHandler(const char *fmt, void *userdata) {
	int *i = (int *) userdata;
	*i = *i + 1;
}

static void __emptyNoticeHandler(const char *fmt, void *userdata) { }
// #nocov end

GEOSContextHandle_t CPL_geos_init(void) {
#ifdef HAVE350
	GEOSContextHandle_t ctxt = GEOS_init_r();
	GEOSContext_setNoticeHandler_r(ctxt, __warningHandler);
	GEOSContext_setErrorHandler_r(ctxt, __errorHandler);
	return ctxt;
#else
	return initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);
#endif
}

void CPL_geos_finish(GEOSContextHandle_t ctxt) {
#ifdef HAVE350
	GEOS_finish_r(ctxt);
#else
	finishGEOS_r(ctxt);
#endif
}

std::vector<GEOSGeom> geometries_from_sfc(GEOSContextHandle_t hGEOSCtxt, Rcpp::List sfc, int *dim = NULL) {

	double precision = sfc.attr("precision");

	Rcpp::CharacterVector cls = get_dim_sfc(sfc, dim);
	if (cls[0] == "XYM" || cls[0] == "XYZM")
		Rcpp::stop("GEOS does not support XYM or XYZM geometries; use st_zm() to drop M\n"); // #nocov

	Rcpp::List wkblst = CPL_write_wkb(sfc, true, native_endian(), cls, precision);
	std::vector<GEOSGeom> g(sfc.size());
	GEOSWKBReader *wkb_reader = GEOSWKBReader_create_r(hGEOSCtxt);
	for (int i = 0; i < sfc.size(); i++) {
		Rcpp::RawVector r = wkblst[i];
		g[i] = GEOSWKBReader_read_r(hGEOSCtxt, wkb_reader, &(r[0]), r.size());
	}
	GEOSWKBReader_destroy_r(hGEOSCtxt, wkb_reader);
	return g;
}

Rcpp::List sfc_from_geometry(GEOSContextHandle_t hGEOSCtxt, std::vector<GEOSGeom> geom, int dim = 2) {

	Rcpp::List out(geom.size());
	GEOSWKBWriter *wkb_writer = GEOSWKBWriter_create_r(hGEOSCtxt);
	GEOSWKBWriter_setOutputDimension_r(hGEOSCtxt, wkb_writer, dim);
	for (size_t i = 0; i < geom.size(); i++) {
		size_t size;
		unsigned char *buf = GEOSWKBWriter_write_r(hGEOSCtxt, wkb_writer, geom[i], &size);
		Rcpp::RawVector raw(size);
		memcpy(&(raw[0]), buf, size);
		GEOSFree_r(hGEOSCtxt, buf);
		out[i] = raw;
		GEOSGeom_destroy_r(hGEOSCtxt, geom[i]);
	}
	GEOSWKBWriter_destroy_r(hGEOSCtxt, wkb_writer);
	return CPL_read_wkb(out, true, false, native_endian());
}

Rcpp::NumericVector get_dim(double dim0, double dim1) {
	Rcpp::NumericVector dim(2);
	dim(0) = dim0;
	dim(1) = dim1;
	return dim;
}

Rcpp::IntegerVector get_which(Rcpp::LogicalVector row) {
	std::vector<int32_t> v;
	for (int i = 0; i < row.length(); i++)
		if (row(i))
			v.push_back(i + 1);
	return Rcpp::wrap(v);
}

bool chk_(char value) {
	if (value == 2)
		Rcpp::stop("GEOS exception"); // #nocov
	return value; // 1: true, 0: false
}

log_fn which_geom_fn(const std::string op) {
	if (op == "intersects")
		return GEOSIntersects_r;
//	else if (op == "disjoint")
//		return GEOSDisjoint_r;
	else if (op == "touches")
		return GEOSTouches_r;
	else if (op == "crosses")
		return GEOSCrosses_r;
	else if (op == "within")
		return GEOSWithin_r;
	else if (op == "contains")
		return GEOSContains_r;
	else if (op == "overlaps")
		return GEOSOverlaps_r;
	else if (op == "equals")
		return GEOSEquals_r;
	else if (op == "covers")
		return GEOSCovers_r;
	else if (op == "covered_by")
		return GEOSCoveredBy_r;
	Rcpp::stop("wrong value for op"); // unlikely to happen unless user wants to #nocov
}

log_prfn which_prep_geom_fn(const std::string op) {
	if (op == "intersects")
		return GEOSPreparedIntersects_r;
//	else if (op == "disjoint")
//		return GEOSPreparedDisjoint_r;
	else if (op == "touches")
		return GEOSPreparedTouches_r;
	else if (op == "crosses")
		return GEOSPreparedCrosses_r;
	else if (op == "within")
		return GEOSPreparedWithin_r;
	else if (op == "contains")
		return GEOSPreparedContains_r;
	else if (op == "contains_properly")
		return GEOSPreparedContainsProperly_r;
	else if (op == "overlaps")
		return GEOSPreparedOverlaps_r;
	//else if (op == "equals")
	//	return GEOSPreparedEquals_r;
	else if (op == "covers")
		return GEOSPreparedCovers_r;
	else if (op == "covered_by")
		return GEOSPreparedCoveredBy_r;
	Rcpp::stop("wrong value for op"); // unlikely to happen unless user wants to #nocov
}

Rcpp::LogicalVector get_dense(std::vector<size_t> items, int length) {
	Rcpp::LogicalVector rowi(length);
	for (int j = 0; j < length; j++)
		rowi(j) = false;
	for (size_t j = 0; j < items.size(); j++)
		rowi(items[j] - 1) = true; // items is 1-based
	return rowi;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_binop(Rcpp::List sfc0, Rcpp::List sfc1, std::string op, double par = 0.0, 
		std::string pattern = "", bool sparse = true, bool prepared = false) {

	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	std::vector<GEOSGeom> gmv0 = geometries_from_sfc(hGEOSCtxt, sfc0, NULL);
	std::vector<GEOSGeom> gmv1 = geometries_from_sfc(hGEOSCtxt, sfc1, NULL);

	Rcpp::List ret_list;

	using namespace Rcpp; // so that later on the (i,_) works
	if (op == "relate") { // character return matrix:
		Rcpp::CharacterVector out(sfc0.length() * sfc1.length());
		for (int i = 0; i < sfc0.length(); i++) {
			for (int j = 0; j < sfc1.length(); j++) {
				char *cp = GEOSRelate_r(hGEOSCtxt, gmv0[i], gmv1[j]);
				if (cp == NULL)
					Rcpp::stop("GEOS error in GEOSRelate_r"); // #nocov
				out[j * sfc0.length() + i] = cp;
				GEOSFree_r(hGEOSCtxt, cp);
			}
			R_CheckUserInterrupt();
		}
		out.attr("dim") = get_dim(sfc0.length(), sfc1.length());
		ret_list = Rcpp::List::create(out);
	} else if (op == "distance") { // return double matrix:
		Rcpp::NumericMatrix out(sfc0.length(), sfc1.length());
		for (size_t i = 0; i < gmv0.size(); i++) {
			for (size_t j = 0; j < gmv1.size(); j++) {
				double dist = -1.0;
				if (GEOSDistance_r(hGEOSCtxt, gmv0[i], gmv1[j], &dist) == 0)
					Rcpp::stop("GEOS error in GEOSDistance_r"); // #nocov
				out(i, j) = dist;
			}
			R_CheckUserInterrupt();
		}
		ret_list = Rcpp::List::create(out);
	} else if (op == "is_within_distance") { // return sparse matrix:
		if (! sparse)
			Rcpp::stop("for a dense matrix, use st_distance(x,y) <= dist");
		Rcpp::List sparsemat(sfc0.length());
		for (size_t i = 0; i < gmv0.size(); i++) {
			std::vector<size_t> sel;
			for (size_t j = 0; j < gmv1.size(); j++) {
				double dist = -1.0;
				if (GEOSDistance_r(hGEOSCtxt, gmv0[i], gmv1[j], &dist) == 0)
					Rcpp::stop("GEOS error in GEOSDistance_r"); // #nocov
				if (dist <= par)
					sel.push_back(j + 1); // 1-based
			}
			sparsemat[i] = Rcpp::IntegerVector(sel.begin(), sel.end());
			R_CheckUserInterrupt();
		}
		ret_list = sparsemat;
	} else if (gmv1.size()) {
		// other cases: boolean return matrix, either dense or sparse
		Rcpp::LogicalMatrix densemat;
		if (! sparse)  // allocate:
			densemat = Rcpp::LogicalMatrix(sfc0.length(), sfc1.length());
		Rcpp::List sparsemat(sfc0.length());

		std::vector<size_t> items(gmv1.size());
		GEOSSTRtree *tree1 = GEOSSTRtree_create_r(hGEOSCtxt, 10);
		for (size_t i = 0; i < gmv1.size(); i++) {
			items[i] = i;
			if (! GEOSisEmpty_r(hGEOSCtxt, gmv1[i]))
				GEOSSTRtree_insert_r(hGEOSCtxt, tree1, gmv1[i], &(items[i]));
		}

		if (op == "equals_exact") { // has it's own signature, needing `par':
			for (int i = 0; i < sfc0.length(); i++) { // row
				Rcpp::LogicalVector rowi(sfc1.length()); 
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSEqualsExact_r(hGEOSCtxt, gmv0[i], gmv1[j], par));
				if (! sparse)
					densemat(i,_) = rowi;
				else
					sparsemat[i] = get_which(rowi);
				R_CheckUserInterrupt();
			}
		} else if (op == "relate_pattern") { // needing pattern
			if (GEOSRelatePatternMatch_r(hGEOSCtxt, pattern.c_str(), "FF*FF****"))
				Rcpp::stop("use st_disjoint for this pattern");
			// all remaining can use tree:
			for (int i = 0; i < sfc0.length(); i++) { // row
				// pre-select sfc1's using tree:
				std::vector<size_t> tree_sel, sel;
				if (! GEOSisEmpty_r(hGEOSCtxt, gmv0[i]))
					GEOSSTRtree_query_r(hGEOSCtxt, tree1, gmv0[i], cb, &tree_sel);
				for (size_t j = 0; j < tree_sel.size(); j++)
					if (chk_(GEOSRelatePattern_r(hGEOSCtxt, gmv0[i], gmv1[tree_sel[j]], pattern.c_str())))
						sel.push_back(tree_sel[j] + 1); // 1-based
				if (sparse) {
					std::sort(sel.begin(), sel.end());
					sparsemat[i] = Rcpp::IntegerVector(sel.begin(), sel.end());
				} else // dense
					densemat(i,_) = get_dense(sel, sfc1.length());
				R_CheckUserInterrupt();
			}
		} else if (op == "disjoint")
			Rcpp::stop("disjoint should have been handled in R"); // #nocov
		else { // anything else:
			if (prepared) {
				log_prfn logical_fn = which_prep_geom_fn(op);
				for (int i = 0; i < sfc0.length(); i++) { // row
					const GEOSPreparedGeometry *pr = GEOSPrepare_r(hGEOSCtxt, gmv0[i]);
					// pre-select sfc1's using tree:
					std::vector<size_t> tree_sel, sel;
					if (! GEOSisEmpty_r(hGEOSCtxt, gmv0[i]))
						GEOSSTRtree_query_r(hGEOSCtxt, tree1, gmv0[i], cb, &tree_sel);
					for (size_t j = 0; j < tree_sel.size(); j++)
						if (chk_(logical_fn(hGEOSCtxt, pr, gmv1[tree_sel[j]])))
							sel.push_back(tree_sel[j] + 1); // 1-based
					if (sparse) {
						std::sort(sel.begin(), sel.end());
						sparsemat[i] = Rcpp::IntegerVector(sel.begin(), sel.end());
					} else // dense
						densemat(i,_) = get_dense(sel, sfc1.length());
					GEOSPreparedGeom_destroy_r(hGEOSCtxt, pr);
					R_CheckUserInterrupt();
				}
			} else {
				log_fn logical_fn = which_geom_fn(op);
				for (int i = 0; i < sfc0.length(); i++) { // row
					// pre-select sfc1's using tree:
					std::vector<size_t> tree_sel, sel;
					if (! GEOSisEmpty_r(hGEOSCtxt, gmv0[i]))
						GEOSSTRtree_query_r(hGEOSCtxt, tree1, gmv0[i], cb, &tree_sel);
					for (size_t j = 0; j < tree_sel.size(); j++)
						if (chk_(logical_fn(hGEOSCtxt, gmv0[i], gmv1[tree_sel[j]])))
							sel.push_back(tree_sel[j] + 1); // 1-based
					if (sparse) {
						std::sort(sel.begin(), sel.end());
						sparsemat[i] = Rcpp::IntegerVector(sel.begin(), sel.end());
					} else // dense
						densemat(i,_) = get_dense(sel, sfc1.length());
					R_CheckUserInterrupt();
				}
			}
		}
		GEOSSTRtree_destroy_r(hGEOSCtxt, tree1);
		if (sparse)
			ret_list = sparsemat;
		else
			ret_list = Rcpp::List::create(densemat);
	} else { // gmv1.size() == 0:
		if (! sparse) { // allocate:
			Rcpp::LogicalMatrix densemat;
			densemat = Rcpp::LogicalMatrix(sfc0.length(), sfc1.length());
			ret_list = Rcpp::List::create(densemat);
		} else {
			Rcpp::List sparsemat(sfc0.length());
			for (size_t i = 0; i < gmv0.size(); i++)
				sparsemat[i] = Rcpp::IntegerVector();
			ret_list = sparsemat;
		}
	}
	for (size_t i = 0; i < gmv0.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, gmv0[i]);
	for (size_t i = 0; i < gmv1.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, gmv1[i]);
	CPL_geos_finish(hGEOSCtxt);
	return ret_list;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_geos_is_valid_reason(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc, NULL);
	Rcpp::CharacterVector out(gmv.size());
	for (int i = 0; i < out.length(); i++) {
		char *buf = GEOSisValidReason_r(hGEOSCtxt, gmv[i]);
		if (buf == NULL)
			out[i] = NA_STRING; // #nocov
		else {
			out[i] = buf;
			GEOSFree_r(hGEOSCtxt, buf);
		}
		GEOSGeom_destroy_r(hGEOSCtxt, gmv[i]);
	}
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_valid(Rcpp::List sfc, bool NA_on_exception = true) { 
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	int notice = 0;
	if (NA_on_exception) {
		if (sfc.size() > 1)
			Rcpp::stop("NA_on_exception will only work reliably with length 1 sfc objects"); // #nocov
#ifdef HAVE350
		GEOSContext_setNoticeMessageHandler_r(hGEOSCtxt, 
			(GEOSMessageHandler_r) __emptyNoticeHandler, (void *) &notice);
		GEOSContext_setErrorMessageHandler_r(hGEOSCtxt, 
			(GEOSMessageHandler_r) __countErrorHandler, (void *) &notice); 
#endif
	}

	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc, NULL); // where notice might be set!
#ifdef HAVE350
	GEOSContext_setNoticeHandler_r(hGEOSCtxt, __warningHandler);
	GEOSContext_setErrorHandler_r(hGEOSCtxt, __errorHandler);
#endif
	Rcpp::LogicalVector out(gmv.size());
	for (int i = 0; i < out.length(); i++) {
		int ret = GEOSisValid_r(hGEOSCtxt, gmv[i]);
		if (NA_on_exception && (ret == 2 || notice != 0))
			out[i] = NA_LOGICAL; // no need to set notice back here, as we only consider 1 geometry #nocov
		else
			out[i] = chk_(ret);
		GEOSGeom_destroy_r(hGEOSCtxt, gmv[i]);
	}
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_simple(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	Rcpp::LogicalVector out(sfc.length());
	std::vector<GEOSGeom> g = geometries_from_sfc(hGEOSCtxt, sfc, NULL);
	for (size_t i = 0; i < g.size(); i++) {
		out[i] = chk_(GEOSisSimple_r(hGEOSCtxt, g[i]));
		GEOSGeom_destroy_r(hGEOSCtxt, g[i]);
	}
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_union(Rcpp::List sfc, bool by_feature = false) { 
	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	std::vector<GEOSGeom> gmv_out(by_feature ? sfc.size() : 1);
	if (by_feature) {
		for (int i = 0; i < sfc.size(); i++) {
			gmv_out[i] = GEOSUnaryUnion_r(hGEOSCtxt, gmv[i]);
			GEOSGeom_destroy_r(hGEOSCtxt, gmv[i]);
		}
	} else {
		GEOSGeom gc = GEOSGeom_createCollection_r(hGEOSCtxt, GEOS_GEOMETRYCOLLECTION, gmv.data(), gmv.size());
		gmv_out[0] = GEOSUnaryUnion_r(hGEOSCtxt, gc);
		GEOSGeom_destroy_r(hGEOSCtxt, gc);
	}

	Rcpp::List out(sfc_from_geometry(hGEOSCtxt, gmv_out, dim)); // destroys gmv_out
	CPL_geos_finish(hGEOSCtxt);
	out.attr("precision") = sfc.attr("precision");
	out.attr("crs") = sfc.attr("crs");
	return out;
}

GEOSGeometry *chkNULL(GEOSGeometry *value) {
	if (value == NULL)
		Rcpp::stop("GEOS exception"); // #nocov
	R_CheckUserInterrupt();
	return value;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_op(std::string op, Rcpp::List sfc, 
		Rcpp::NumericVector bufferDist, int nQuadSegs = 30,
		double dTolerance = 0.0, bool preserveTopology = false, 
		int bOnlyEdges = 1, double dfMaxLength = 0.0) {

	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init(); 

	std::vector<GEOSGeom> g = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	std::vector<GEOSGeom> out(sfc.length());

	if (op == "buffer") {
		if (bufferDist.size() != (int) g.size())
			Rcpp::stop("invalid dist argument"); // #nocov
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSBuffer_r(hGEOSCtxt, g[i], bufferDist[i], nQuadSegs));
	} else if (op == "boundary") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSBoundary_r(hGEOSCtxt, g[i]));
	} else if (op == "convex_hull") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSConvexHull_r(hGEOSCtxt, g[i]));
//	} else if (op == "unary_union") { // -> done by CPL_geos_union()
//		for (size_t i = 0; i < g.size(); i++)
//			out[i] = chkNULL(GEOSUnaryUnion_r(hGEOSCtxt, g[i]));
	} else if (op == "simplify") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = preserveTopology ? chkNULL(GEOSTopologyPreserveSimplify_r(hGEOSCtxt, g[i], dTolerance)) :
					chkNULL(GEOSSimplify_r(hGEOSCtxt, g[i], dTolerance));
	} else if (op == "linemerge") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSLineMerge_r(hGEOSCtxt, g[i]));
	} else if (op == "polygonize") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSPolygonize_r(hGEOSCtxt, &(g[i]), 1));
	} else if (op == "centroid") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = chkNULL(GEOSGetCentroid_r(hGEOSCtxt, g[i]));
		}
	} else if (op == "node") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = chkNULL(GEOSNode_r(hGEOSCtxt, g[i]));
		}
	} else if (op == "point_on_surface") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = chkNULL(GEOSPointOnSurface_r(hGEOSCtxt, g[i]));
		}
	} else
#if GEOS_VERSION_MAJOR >= 3 && GEOS_VERSION_MINOR >= 4
	if (op == "triangulate") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = chkNULL(GEOSDelaunayTriangulation_r(hGEOSCtxt, g[i], dTolerance, bOnlyEdges));
	} else
#endif
		Rcpp::stop("invalid operation"); // would leak g and out // #nocov

	for (size_t i = 0; i < g.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, g[i]);

	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out, dim)); // destroys out
	CPL_geos_finish(hGEOSCtxt);
	ret.attr("precision") = sfc.attr("precision");
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_voronoi(Rcpp::List sfc, Rcpp::List env, double dTolerance = 0.0, int bOnlyEdges = 1) {

	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init(); 

	std::vector<GEOSGeom> g = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	std::vector<GEOSGeom> out(sfc.length());

#ifdef HAVE350
	switch (env.size()) {
		case 0: ;
		case 1: {
			std::vector<GEOSGeom> g_env = geometries_from_sfc(hGEOSCtxt, env);
			for (size_t i = 0; i < g.size(); i++) {
				out[i] = chkNULL(GEOSVoronoiDiagram_r(hGEOSCtxt, g[i], 
					g_env.size() ? g_env[0] : NULL, dTolerance, bOnlyEdges));
				GEOSGeom_destroy_r(hGEOSCtxt, g[i]);
			}
			if (g_env.size())
				GEOSGeom_destroy_r(hGEOSCtxt, g_env[0]);
			break;
		}
		default:
			Rcpp::stop("env should have length 0 or 1"); // #nocov
	}
#else
	Rcpp::stop("voronoi diagrams require a GEOS version >= 3.5.0"); // #nocov
#endif

	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out, dim)); // destroys out
	CPL_geos_finish(hGEOSCtxt);
	ret.attr("precision") = sfc.attr("precision");
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_op2(std::string op, Rcpp::List sfcx, Rcpp::List sfcy) {

	using namespace Rcpp; // so that later on the (_,1) works

	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GEOSGeom> x = geometries_from_sfc(hGEOSCtxt, sfcx, &dim);
	std::vector<GEOSGeom> y = geometries_from_sfc(hGEOSCtxt, sfcy, &dim);
	std::vector<GEOSGeom> out;
	std::vector<double> index_x, index_y;
	std::vector<size_t> items(x.size());

	if (op == "intersection") {

		bool tree_empty = true;
		GEOSSTRtree *tree = GEOSSTRtree_create_r(hGEOSCtxt, 10);
		for (size_t i = 0; i < x.size(); i++) {
			items[i] = i;
			if (! GEOSisEmpty_r(hGEOSCtxt, x[i])) {
				GEOSSTRtree_insert_r(hGEOSCtxt, tree, x[i], &(items[i]));
				tree_empty = false;
			}
		}

		for (size_t i = 0; i < y.size(); i++) {
			// select x's using tree:
			std::vector<size_t> sel;
			sel.reserve(x.size());
			if (! GEOSisEmpty_r(hGEOSCtxt, y[i]) && ! tree_empty)
				GEOSSTRtree_query_r(hGEOSCtxt, tree, y[i], cb, &sel);
			std::sort(sel.begin(), sel.end());
			for (size_t item = 0; item < sel.size(); item++) {
				size_t j = sel[item];
				GEOSGeom geom = GEOSIntersection_r(hGEOSCtxt, x[j], y[i]);
				if (geom == NULL)
					Rcpp::stop("GEOS exception"); // #nocov
				if (! chk_(GEOSisEmpty_r(hGEOSCtxt, geom))) {
					index_x.push_back(j + 1);
					index_y.push_back(i + 1);
					out.push_back(geom); // keep
				} else
					GEOSGeom_destroy_r(hGEOSCtxt, geom); // discard
			}
			R_CheckUserInterrupt();
		}
		GEOSSTRtree_destroy_r(hGEOSCtxt, tree);

	} else {
		geom_fn geom_function;
		if (op == "union")
			geom_function = (geom_fn) GEOSUnion_r;
		else if (op == "difference")
			geom_function = (geom_fn) GEOSDifference_r;
		else if (op == "sym_difference")
			geom_function = (geom_fn) GEOSSymDifference_r;
		else
			Rcpp::stop("invalid operation"); // #nocov

		for (size_t i = 0; i < y.size(); i++) {
			for (size_t j = 0; j < x.size(); j++) {
				GEOSGeom geom = geom_function(hGEOSCtxt, x[j], y[i]);
				if (geom == NULL)
					Rcpp::stop("GEOS exception"); // #nocov
				if (! chk_(GEOSisEmpty_r(hGEOSCtxt, geom))) {
					index_x.push_back(j + 1);
					index_y.push_back(i + 1);
					out.push_back(geom); // keep
				} else
					GEOSGeom_destroy_r(hGEOSCtxt, geom); // discard
			}
			R_CheckUserInterrupt();
		}
	}

	// clean up x and y:
	for (size_t i = 0; i < x.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, x[i]);
	for (size_t i = 0; i < y.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, y[i]);

	Rcpp::NumericMatrix m(index_x.size(), 2); // and a set of 1-based indices to x and y
	m(_, 0) = Rcpp::NumericVector(index_x.begin(), index_x.end());
	m(_, 1) = Rcpp::NumericVector(index_y.begin(), index_y.end());

	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out, dim)); // destroys out2
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
	Rcpp::NumericMatrix out = CPL_geos_binop(sfc0, sfc1, "distance", 0.0, "", false)[0];
	return out;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_geos_relate(Rcpp::List sfc0, Rcpp::List sfc1) {
	Rcpp::CharacterVector out = CPL_geos_binop(sfc0, sfc1, "relate", 0.0, "", false)[0];
	return out;	
}

// [[Rcpp::export]]
Rcpp::List CPL_transpose_sparse_incidence(Rcpp::List m, int n) {
// transpose a sparse incidence matrix list m that has n columns
	std::vector<size_t> sizes(n);
	for (int i = 0; i < n; i++)
		sizes[i] = 0; // init
	for (int i = 0; i < m.size(); i++) {
		Rcpp::IntegerVector v = m[i];
		for (int j = 0; j < v.size(); j++) {
			if (v[j] > n || v[j] < 0)
				Rcpp::stop("CPL_transpose_sparse_incidence: index out of bounds"); // #nocov
			sizes[v[j] - 1] += 1; // count
		}
	}
	Rcpp::List out(n);
	for (int i = 0; i < n; i++)
		out[i] = Rcpp::IntegerVector(sizes[i]);
	for (int i = 0; i < m.size(); i++) {
		Rcpp::IntegerVector v = m[i];
		for (int j = 0; j < v.size(); j++) {
			size_t new_i = v[j] - 1;
			Rcpp::IntegerVector w = out[new_i];
			w[w.size() - sizes[new_i]] = i + 1; // 1-based
			sizes[new_i] -= 1;
		}
	}
	return out;
}
