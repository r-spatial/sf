#define GEOS_USE_ONLY_R_API // prevents using non-thread-safe GEOSxx functions without _r extension.
#include <geos_c.h>

#if GEOS_VERSION_MAJOR == 3
# if GEOS_VERSION_MINOR >= 4
#  define HAVE340
# endif
# if GEOS_VERSION_MINOR >= 5
#  define HAVE350
# endif
# if GEOS_VERSION_MINOR >= 9
#  define HAVE390
# endif
# if GEOS_VERSION_MINOR >= 8
#  define HAVE380
# endif
# if GEOS_VERSION_MINOR == 6
#  if GEOS_VERSION_PATCH >= 1
#   define HAVE361
#  endif
# endif
# if GEOS_VERSION_MINOR >= 7
#  define HAVE361
#  define HAVE370
# endif
#else
# if GEOS_VERSION_MAJOR > 3
#  define HAVE340
#  define HAVE350
#  define HAVE370
#  define HAVE361
#  define HAVE380
#  define HAVE390
# endif
#endif

#include <Rcpp.h>
#include <memory>
#include <vector>

#include "wkb.h"
#include "hex.h"

typedef int (* dist_fn)(GEOSContextHandle_t, const GEOSGeometry *, const GEOSGeometry *, double *);
typedef int (* dist_parfn)(GEOSContextHandle_t, const GEOSGeometry *, const GEOSGeometry *, double, double *);
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
	va_list ap;
	va_start(ap, fmt);
	vsprintf(buf, fmt, ap);
	va_end(ap);
	p = buf + strlen(buf) - 1;
	if(strlen(buf) > 0 && *p == '\n') *p = '\0';

	Rcpp::Function error(".stop_geos", Rcpp::Environment::namespace_env("sf"));
	error(buf);

	return; // #nocov end
}

static void __warningHandler(const char *fmt, ...) {

	char buf[BUFSIZ], *p;
	va_list ap;
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


using PrepGeomPtr= std::unique_ptr<const GEOSPreparedGeometry, std::function<void(const GEOSPreparedGeometry*)> >;
using GeomPtr= std::unique_ptr<GEOSGeometry, std::function<void(GEOSGeometry*)> >;
using TreePtr= std::unique_ptr<GEOSSTRtree, std::function<void(GEOSSTRtree*)> >;

static GeomPtr geos_ptr(GEOSGeometry* g, GEOSContextHandle_t hGEOSctxt) {
	auto deleter = std::bind(GEOSGeom_destroy_r, hGEOSctxt, std::placeholders::_1);
	return GeomPtr(g, deleter);
}

static PrepGeomPtr geos_ptr(const GEOSPreparedGeometry* pg, GEOSContextHandle_t hGEOSctxt) {
	auto deleter = std::bind(GEOSPreparedGeom_destroy_r, hGEOSctxt, std::placeholders::_1);
	return PrepGeomPtr(pg, deleter);
}

static TreePtr geos_ptr(GEOSSTRtree* t, GEOSContextHandle_t hGEOSctxt) {
	auto deleter = std::bind(GEOSSTRtree_destroy_r, hGEOSctxt, std::placeholders::_1);
	return TreePtr(t, deleter);
}

static std::vector<GEOSGeometry*> to_raw(std::vector<GeomPtr> & g) {
	std::vector<GEOSGeometry*> raw(g.size());
	std::transform(g.begin(), g.end(), raw.begin(), [](GeomPtr & g) { return g.release(); });
	return raw;
}

std::vector<GeomPtr> geometries_from_sfc(GEOSContextHandle_t hGEOSCtxt, Rcpp::List sfc, int *dim = NULL) {

	Rcpp::List sfc_cls = get_dim_sfc(sfc);
	Rcpp::CharacterVector cls = sfc_cls["_cls"];
	if (dim != NULL) {
		Rcpp::IntegerVector sfc_dim = sfc_cls["_dim"];
		if (sfc_dim.size() == 0)
			Rcpp::stop("sfc_dim size 0: should not happen"); // #nocov
		*dim = sfc_dim[0];
	}

	if (cls[0] == "XYM" || cls[0] == "XYZM")
		Rcpp::stop("GEOS does not support XYM or XYZM geometries; use st_zm() to drop M\n"); // #nocov

	Rcpp::List wkblst = CPL_write_wkb(sfc, true);
	std::vector<GeomPtr> g(sfc.size());
	GEOSWKBReader *wkb_reader = GEOSWKBReader_create_r(hGEOSCtxt);
	for (int i = 0; i < sfc.size(); i++) {
		Rcpp::RawVector r = wkblst[i];
		g[i] = geos_ptr(GEOSWKBReader_read_r(hGEOSCtxt, wkb_reader, &(r[0]), r.size()), hGEOSCtxt);
	}
	GEOSWKBReader_destroy_r(hGEOSCtxt, wkb_reader);
	return g;
}

Rcpp::List sfc_from_geometry(GEOSContextHandle_t hGEOSCtxt, std::vector<GeomPtr> & geom, int dim = 2, bool free = true) {

	Rcpp::List out(geom.size());
	GEOSWKBWriter *wkb_writer = GEOSWKBWriter_create_r(hGEOSCtxt);
	GEOSWKBWriter_setOutputDimension_r(hGEOSCtxt, wkb_writer, dim);
	// empty point, binary, with R NA's (not NaN's); GEOS can't WKB empty points,
	// so we need to work around; see also https://trac.osgeo.org/postgis/ticket/3031
	// > sf:::CPL_raw_to_hex(st_as_binary(st_point()))
	// [1] "0101000000a20700000000f07fa20700000000f07f"
	Rcpp::RawVector empty_point(CPL_hex_to_raw("0101000000a20700000000f07fa20700000000f07f")[0]);
	for (size_t i = 0; i < geom.size(); i++) {
		bool is_empty_point = false;
		bool is_empty = GEOSisEmpty_r(hGEOSCtxt, geom[i].get()) == 1;
		if (is_empty) {
			char *geom_type = GEOSGeomType_r(hGEOSCtxt, geom[i].get());
			is_empty_point = strcmp("Point", geom_type) == 0;
			GEOSFree_r(hGEOSCtxt, geom_type);
		}
		if (is_empty_point)
			out[i] = empty_point;
		else {
			size_t size;
			unsigned char *buf = GEOSWKBWriter_write_r(hGEOSCtxt, wkb_writer, geom[i].get(), &size);
			Rcpp::RawVector raw(size);
			memcpy(&(raw[0]), buf, size);
			GEOSFree_r(hGEOSCtxt, buf);
			out[i] = raw;
		}
	}
	GEOSWKBWriter_destroy_r(hGEOSCtxt, wkb_writer);
	return CPL_read_wkb(out, true, false);
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

/*
Rcpp::LogicalVector get_dense(std::vector<size_t> items, int length) {
	Rcpp::LogicalVector rowi(length);
	for (int j = 0; j < length; j++)
		rowi(j) = false;
	for (size_t j = 0; j < items.size(); j++)
		rowi(items[j] - 1) = true; // items is 1-based
	return rowi;
}
*/

// [[Rcpp::export]]
Rcpp::List CPL_geos_binop(Rcpp::List sfc0, Rcpp::List sfc1, std::string op, double par = 0.0,
		std::string pattern = "", bool prepared = false) {

	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	std::vector<GeomPtr> gmv0 = geometries_from_sfc(hGEOSCtxt, sfc0, NULL);
	std::vector<GeomPtr> gmv1 = geometries_from_sfc(hGEOSCtxt, sfc1, NULL);

	Rcpp::List ret_list;

	using namespace Rcpp; // so that later on the (i,_) works
	if (op == "relate") { // return character matrix:
		Rcpp::CharacterVector out(sfc0.length() * sfc1.length());
		for (int i = 0; i < sfc0.length(); i++) {
			for (int j = 0; j < sfc1.length(); j++) {
				char *cp = GEOSRelate_r(hGEOSCtxt, gmv0[i].get(), gmv1[j].get());
				if (cp == NULL) {
					GEOSFree_r(hGEOSCtxt, cp); // #nocov
					CPL_geos_finish(hGEOSCtxt); // #nocov
					Rcpp::stop("GEOS error in GEOSRelate_r"); // #nocov
				}
				out[j * sfc0.length() + i] = cp;
				GEOSFree_r(hGEOSCtxt, cp);
			}
			Rcpp::checkUserInterrupt();
		}
		out.attr("dim") = get_dim(sfc0.length(), sfc1.length());
		ret_list = Rcpp::List::create(out);
	} else if (op == "Euclidean" || op == "distance" || op == "Hausdorff" || op == "Frechet") { // return double matrix:
		// dist_fn, dist_parfn
		Rcpp::NumericMatrix out(sfc0.length(), sfc1.length());
		if (par <= 0.0) {
			dist_fn dist_function;
			if (op == "Euclidean" || op == "distance")
				dist_function = GEOSDistance_r;
			else if (op == "Hausdorff")
				dist_function = GEOSHausdorffDistance_r;
#ifdef HAVE370
			else if (op == "Frechet")
				dist_function = GEOSFrechetDistance_r;
#endif
			else {
				CPL_geos_finish(hGEOSCtxt); // #nocov
				Rcpp::stop("distance function not supported"); // #nocov
			}

			for (size_t i = 0; i < gmv0.size(); i++) {
				if (GEOSisEmpty_r(hGEOSCtxt, gmv0[i].get())) {
					for (size_t j = 0; j < gmv1.size(); j++) // #nocov
						out(i, j) = NA_REAL;                 // #nocov
				} else for (size_t j = 0; j < gmv1.size(); j++) {
					if (GEOSisEmpty_r(hGEOSCtxt, gmv1[j].get()))
						out(i, j) = NA_REAL;
					else {
						double dist = -1.0;
						if (dist_function(hGEOSCtxt, gmv0[i].get(), gmv1[j].get(), &dist) == 0) {
							CPL_geos_finish(hGEOSCtxt); // #nocov
							Rcpp::stop("GEOS error in GEOS_xx_Distance_r"); // #nocov
						}
						out(i, j) = dist;
					}
				}
				Rcpp::checkUserInterrupt();
			}
		} else {
			dist_parfn dist_function = NULL;
			if (op == "Hausdorff")
				dist_function = GEOSHausdorffDistanceDensify_r;
#ifdef HAVE370
			else if (op == "Frechet")
				dist_function = GEOSFrechetDistanceDensify_r;
#endif
			else {
				CPL_geos_finish(hGEOSCtxt); // #nocov
				Rcpp::stop("distance function not supported"); // #nocov
			}

			for (size_t i = 0; i < gmv0.size(); i++) {
				if (GEOSisEmpty_r(hGEOSCtxt, gmv0[i].get())) {
					for (size_t j = 0; j < gmv1.size(); j++)
						out(i, j) = NA_REAL;
				} else for (size_t j = 0; j < gmv1.size(); j++) {
					if (GEOSisEmpty_r(hGEOSCtxt, gmv1[j].get()))
						out(i, j) = NA_REAL;
					else {
						double dist = -1.0;
						if (dist_function(hGEOSCtxt, gmv0[i].get(), gmv1[j].get(), par, &dist) == 0) {
							CPL_geos_finish(hGEOSCtxt); // #nocov
							Rcpp::stop("GEOS error in GEOS_xx_Distance_r"); // #nocov
						}
						out(i, j) = dist;
					}
				}
				Rcpp::checkUserInterrupt();
			}
		}
		ret_list = Rcpp::List::create(out);
	} else if (op == "is_within_distance") {
		Rcpp::List sparsemat(sfc0.length());
		for (size_t i = 0; i < gmv0.size(); i++) {
			std::vector<size_t> sel;
			for (size_t j = 0; j < gmv1.size(); j++) {
				double dist = -1.0;
				if (GEOSDistance_r(hGEOSCtxt, gmv0[i].get(), gmv1[j].get(), &dist) == 0) {
					CPL_geos_finish(hGEOSCtxt); // #nocov
					Rcpp::stop("GEOS error in GEOSDistance_r"); // #nocov
				}
				if (dist <= par)
					sel.push_back(j + 1); // 1-based
			}
			sparsemat[i] = Rcpp::IntegerVector(sel.begin(), sel.end());
			Rcpp::checkUserInterrupt();
		}
		ret_list = sparsemat;
	} else if (gmv1.size()) {
		// other cases: sparse matrix
		Rcpp::List sparsemat(sfc0.length());

		std::vector<size_t> items(gmv1.size());
		TreePtr tree1 = geos_ptr(GEOSSTRtree_create_r(hGEOSCtxt, 10), hGEOSCtxt);
		for (size_t i = 0; i < gmv1.size(); i++) {
			items[i] = i;
			if (! GEOSisEmpty_r(hGEOSCtxt, gmv1[i].get()))
				GEOSSTRtree_insert_r(hGEOSCtxt, tree1.get(), gmv1[i].get(), &(items[i]));
		}

		if (op == "equals_exact") { // has it's own signature, needing `par':
			for (int i = 0; i < sfc0.length(); i++) { // row
				Rcpp::LogicalVector rowi(sfc1.length());
				for (int j = 0; j < sfc1.length(); j++)
					rowi(j) = chk_(GEOSEqualsExact_r(hGEOSCtxt, gmv0[i].get(), gmv1[j].get(), par));
				sparsemat[i] = get_which(rowi);
				Rcpp::checkUserInterrupt();
			}
		} else if (op == "relate_pattern") { // needing pattern
			if (GEOSRelatePatternMatch_r(hGEOSCtxt, pattern.c_str(), "FF*FF****")) {
				CPL_geos_finish(hGEOSCtxt);
				Rcpp::stop("use st_disjoint for this pattern");
			}
			// all remaining can use tree:
			for (int i = 0; i < sfc0.length(); i++) { // row
				// pre-select sfc1's using tree:
				std::vector<size_t> tree_sel, sel;
				if (! GEOSisEmpty_r(hGEOSCtxt, gmv0[i].get()))
					GEOSSTRtree_query_r(hGEOSCtxt, tree1.get(), gmv0[i].get(), cb, &tree_sel);
				for (size_t j = 0; j < tree_sel.size(); j++)
					if (chk_(GEOSRelatePattern_r(hGEOSCtxt, gmv0[i].get(), gmv1[tree_sel[j]].get(), pattern.c_str())))
						sel.push_back(tree_sel[j] + 1); // 1-based
				std::sort(sel.begin(), sel.end());
				sparsemat[i] = Rcpp::IntegerVector(sel.begin(), sel.end());
				Rcpp::checkUserInterrupt();
			}
		} else if (op == "disjoint") {
			CPL_geos_finish(hGEOSCtxt); // #nocov
			Rcpp::stop("disjoint should have been handled in R"); // #nocov
		}
		else { // anything else:
			if (prepared) {
				log_prfn logical_fn = which_prep_geom_fn(op);
				for (int i = 0; i < sfc0.length(); i++) { // row
					// pre-select sfc1's using tree:
					std::vector<size_t> tree_sel, sel;
					if (! GEOSisEmpty_r(hGEOSCtxt, gmv0[i].get()))
						GEOSSTRtree_query_r(hGEOSCtxt, tree1.get(), gmv0[i].get(), cb, &tree_sel);

					if (! tree_sel.empty()) {
						PrepGeomPtr pr = geos_ptr(GEOSPrepare_r(hGEOSCtxt, gmv0[i].get()), hGEOSCtxt);
						for (size_t j = 0; j < tree_sel.size(); j++)
							if (chk_(logical_fn(hGEOSCtxt, pr.get(), gmv1[tree_sel[j]].get())))
								sel.push_back(tree_sel[j] + 1); // 1-based
						std::sort(sel.begin(), sel.end());
					}

					sparsemat[i] = Rcpp::IntegerVector(sel.begin(), sel.end());
					Rcpp::checkUserInterrupt();
				}
			} else {
				log_fn logical_fn = which_geom_fn(op);
				for (int i = 0; i < sfc0.length(); i++) { // row
					// pre-select sfc1's using tree:
					std::vector<size_t> tree_sel, sel;
					if (! GEOSisEmpty_r(hGEOSCtxt, gmv0[i].get()))
						GEOSSTRtree_query_r(hGEOSCtxt, tree1.get(), gmv0[i].get(), cb, &tree_sel);
					for (size_t j = 0; j < tree_sel.size(); j++)
						if (chk_(logical_fn(hGEOSCtxt, gmv0[i].get(), gmv1[tree_sel[j]].get())))
							sel.push_back(tree_sel[j] + 1); // 1-based
					std::sort(sel.begin(), sel.end());
					sparsemat[i] = Rcpp::IntegerVector(sel.begin(), sel.end());
					Rcpp::checkUserInterrupt();
				}
			}
		}
		ret_list = sparsemat;
	} else { // gmv1.size() == 0:
		Rcpp::List sparsemat(sfc0.length());
		for (size_t i = 0; i < gmv0.size(); i++)
			sparsemat[i] = Rcpp::IntegerVector();
		ret_list = sparsemat;
	}
	// clean up:
	CPL_geos_finish(hGEOSCtxt);
	return ret_list;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_geos_is_valid_reason(Rcpp::List sfc) {
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	std::vector<GeomPtr> gmv = geometries_from_sfc(hGEOSCtxt, sfc, NULL);
	Rcpp::CharacterVector out(gmv.size());
	for (int i = 0; i < out.length(); i++) {
		char *buf = GEOSisValidReason_r(hGEOSCtxt, gmv[i].get());
		if (buf == NULL)
			out[i] = NA_STRING; // #nocov
		else {
			out[i] = buf;
			GEOSFree_r(hGEOSCtxt, buf);
		}
	}
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// #nocov start - no GEOS 3.8.0 on travis yet
// [[Rcpp::export]]
Rcpp::List CPL_geos_make_valid(Rcpp::List sfc) {
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	std::vector<GeomPtr> gmv = geometries_from_sfc(hGEOSCtxt, sfc, NULL);
	std::vector<GeomPtr> out(gmv.size());
#ifdef HAVE380
	for (size_t i = 0; i < gmv.size(); i++)
		gmv[i] = geos_ptr(GEOSMakeValid_r(hGEOSCtxt, gmv[i].get()), hGEOSCtxt);
#else
	Rcpp::stop("this shouldn't happen: st_make_valid should use lwgeom");
#endif
	Rcpp::List ret = sfc_from_geometry(hGEOSCtxt, gmv);
	CPL_geos_finish(hGEOSCtxt);
	return ret;
}
// #nocov end

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_valid(Rcpp::List sfc, bool NA_on_exception = true) {
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	int notice = 0;
	if (NA_on_exception) {
/*
		if (sfc.size() > 1)
			Rcpp::stop("NA_on_exception will only work reliably with length 1 sfc objects"); // #nocov
*/
#ifdef HAVE350
		GEOSContext_setNoticeMessageHandler_r(hGEOSCtxt,
			(GEOSMessageHandler_r) __emptyNoticeHandler, (void *) &notice);
		GEOSContext_setErrorMessageHandler_r(hGEOSCtxt,
			(GEOSMessageHandler_r) __countErrorHandler, (void *) &notice);
#endif
	}
	std::vector<GeomPtr> gmv = geometries_from_sfc(hGEOSCtxt, sfc, NULL); // where notice might be set!
	Rcpp::LogicalVector out(gmv.size());
	for (int i = 0; i < out.length(); i++) {
		int ret = GEOSisValid_r(hGEOSCtxt, gmv[i].get());
		if (NA_on_exception && (ret == 2 || notice != 0))
			out[i] = NA_LOGICAL; // no need to set notice back here, as we only consider 1 geometry #nocov
		else
			out[i] = chk_(ret);
	}
#ifdef HAVE350
	GEOSContext_setNoticeHandler_r(hGEOSCtxt, __warningHandler);
	GEOSContext_setErrorHandler_r(hGEOSCtxt, __errorHandler);
#endif
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_simple(Rcpp::List sfc) {
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	Rcpp::LogicalVector out(sfc.length());
	std::vector<GeomPtr> g = geometries_from_sfc(hGEOSCtxt, sfc, NULL);
	for (size_t i = 0; i < g.size(); i++) {
		out[i] = chk_(GEOSisSimple_r(hGEOSCtxt, g[i].get()));
	}
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_empty(Rcpp::List sfc) {
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	Rcpp::LogicalVector out(sfc.length());
	std::vector<GeomPtr> g = geometries_from_sfc(hGEOSCtxt, sfc, NULL);
	for (size_t i = 0; i < g.size(); i++) {
		out[i] = chk_(GEOSisEmpty_r(hGEOSCtxt, g[i].get()));
	}
	CPL_geos_finish(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_normalize(Rcpp::List sfc) { // #nocov start
	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GeomPtr> gmv = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	for (int i = 0; i < sfc.size(); i++) {
		if (GEOSNormalize_r(hGEOSCtxt, gmv[i].get()) == -1)
			Rcpp::stop("normalize: GEOS exception");
	}
	Rcpp::List out(sfc_from_geometry(hGEOSCtxt, gmv, dim));
	CPL_geos_finish(hGEOSCtxt);
	out.attr("precision") = sfc.attr("precision");
	out.attr("crs") = sfc.attr("crs");
	return out;
} // #nocov end

// [[Rcpp::export]]
Rcpp::List CPL_geos_union(Rcpp::List sfc, bool by_feature = false, bool is_coverage = false) {

#ifndef HAVE380
	if (is_coverage) {
		Rcpp::warning("ignoring 'is_coverage = TRUE' which requires GEOS version 3.8 or greater");
		is_coverage = false;
	}
#endif

	if (sfc.size() == 0)
		return sfc; // #nocov

	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GeomPtr> gmv = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	std::vector<GeomPtr> gmv_out(by_feature ? sfc.size() : 1);

	if (by_feature) {
		for (int i = 0; i < sfc.size(); i++) {
			gmv_out[i] = geos_ptr(GEOSUnaryUnion_r(hGEOSCtxt, gmv[i].get()), hGEOSCtxt);
		}
	} else {
		bool all_inputs_same = true;

		// check to see if all geometries are identical, as in a call to summarize(..., do_union=TRUE)
		for (size_t i = 1; i < gmv.size(); i++) {
			if (!GEOSEqualsExact_r(hGEOSCtxt, gmv[0].get(), gmv[i].get(), 0.0)) {
				all_inputs_same = false;
				break;
			}
		}

		if (all_inputs_same) {
			gmv_out[0] = std::move(gmv[0]);
		} else {
			GeomPtr gc = geos_ptr(GEOSGeom_createCollection_r(hGEOSCtxt, GEOS_GEOMETRYCOLLECTION, to_raw(gmv).data(), gmv.size()), hGEOSCtxt);

#ifdef HAVE380
			if (is_coverage)
				gmv_out[0] = geos_ptr(GEOSCoverageUnion_r(hGEOSCtxt, gc.get()), hGEOSCtxt);
			else
#endif
			gmv_out[0] = geos_ptr(GEOSUnaryUnion_r(hGEOSCtxt, gc.get()), hGEOSCtxt);
		}
	}

	Rcpp::List out(sfc_from_geometry(hGEOSCtxt, gmv_out, dim));
	CPL_geos_finish(hGEOSCtxt);
	out.attr("precision") = sfc.attr("precision");
	out.attr("crs") = sfc.attr("crs");
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_snap(Rcpp::List sfc0, Rcpp::List sfc1, Rcpp::NumericVector tolerance) {
	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GeomPtr> gmv0 = geometries_from_sfc(hGEOSCtxt, sfc0, &dim);
	std::vector<GeomPtr> gmv1 = geometries_from_sfc(hGEOSCtxt, sfc1, &dim);
	GeomPtr gc;
	if (gmv1.size() > 1)
		gc = geos_ptr(GEOSGeom_createCollection_r(hGEOSCtxt, GEOS_GEOMETRYCOLLECTION,
			to_raw(gmv1).data(), gmv1.size()), hGEOSCtxt);
	else
		gc = std::move(gmv1[0]);

	std::vector<GeomPtr> gmv_out(sfc0.size());
	for (int i = 0; i < sfc0.size(); i++) {
		gmv_out[i] = geos_ptr(GEOSSnap_r(hGEOSCtxt, gmv0[i].get(), gc.get(), tolerance[i]), hGEOSCtxt);
		if (gmv_out[i] == NULL)
			Rcpp::stop("snap: GEOS exception"); // #nocov
	}
	Rcpp::List out(sfc_from_geometry(hGEOSCtxt, gmv_out, dim));
	CPL_geos_finish(hGEOSCtxt);
	out.attr("precision") = sfc0.attr("precision");
	out.attr("crs") = sfc0.attr("crs");
	return out;
}

GEOSGeometry *chkNULL(GEOSGeometry *value) {
	if (value == NULL)
		Rcpp::stop("GEOS exception"); // #nocov
	Rcpp::checkUserInterrupt();
	return value;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_op(std::string op, Rcpp::List sfc,
                       Rcpp::NumericVector bufferDist, Rcpp::IntegerVector nQuadSegs,
                       Rcpp::NumericVector dTolerance, Rcpp::LogicalVector preserveTopology,
                       int bOnlyEdges = 1,
                       Rcpp::IntegerVector endCapStyle = 0, Rcpp::IntegerVector joinStyle = 0,
					   Rcpp::NumericVector mitreLimit = 1, Rcpp::LogicalVector singleside = 0)
{
	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	std::vector<GeomPtr> g = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	std::vector<GeomPtr> out(sfc.length());

	if (op == "buffer") {
		if (bufferDist.size() != (int) g.size())
			Rcpp::stop("invalid dist argument"); // #nocov
		for (size_t i = 0; i < g.size(); i++)
			out[i] = geos_ptr(chkNULL(GEOSBuffer_r(hGEOSCtxt, g[i].get(), bufferDist[i], nQuadSegs[i])), hGEOSCtxt);
	} else if (op == "buffer_with_style") {
		GEOSBufferParams *bufferparams = GEOSBufferParams_create_r(hGEOSCtxt);
		for (size_t i = 0; i < g.size(); i++) {
			if (GEOSBufferParams_setEndCapStyle_r(hGEOSCtxt, bufferparams, endCapStyle[i]) &&
					GEOSBufferParams_setJoinStyle_r(hGEOSCtxt, bufferparams, joinStyle[i]) &&
					GEOSBufferParams_setMitreLimit_r(hGEOSCtxt, bufferparams, mitreLimit[i]) &&
					GEOSBufferParams_setQuadrantSegments_r(hGEOSCtxt, bufferparams, nQuadSegs[i]) &&
					GEOSBufferParams_setSingleSided_r(hGEOSCtxt, bufferparams, singleside[i]))
				// out[i] = geos_ptr(chkNULL(GEOSBufferWithStyle_r(hGEOSCtxt, g[i].get(), bufferDist[i], nQuadSegs[i], endCapStyle[i], joinStyle[i], mitreLimit[i])), hGEOSCtxt);
				out[i] = geos_ptr(chkNULL(GEOSBufferWithParams_r(hGEOSCtxt, g[i].get(),
					bufferparams, bufferDist[i])), hGEOSCtxt);
			else
				Rcpp::stop("invalid buffer parameters"); // #nocov
		}
		GEOSBufferParams_destroy_r(hGEOSCtxt, bufferparams);
	} else if (op == "boundary") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = geos_ptr(chkNULL(GEOSBoundary_r(hGEOSCtxt, g[i].get())), hGEOSCtxt);
	} else if (op == "convex_hull") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = geos_ptr(chkNULL(GEOSConvexHull_r(hGEOSCtxt, g[i].get())), hGEOSCtxt);
	} else if (op == "simplify") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = geos_ptr(
					preserveTopology[i] ?
						chkNULL(GEOSTopologyPreserveSimplify_r(hGEOSCtxt, g[i].get(),
							dTolerance[i])) :
						chkNULL(GEOSSimplify_r(hGEOSCtxt, g[i].get(), dTolerance[i])), hGEOSCtxt);
	} else if (op == "linemerge") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = geos_ptr(chkNULL(GEOSLineMerge_r(hGEOSCtxt, g[i].get())), hGEOSCtxt);
	} else if (op == "polygonize") {
		for (size_t i = 0; i < g.size(); i++) {
			const GEOSGeometry* gi = g[i].get();
			out[i] = geos_ptr(chkNULL(GEOSPolygonize_r(hGEOSCtxt, &gi, 1)), hGEOSCtxt);
		}
	} else if (op == "centroid") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = geos_ptr(chkNULL(GEOSGetCentroid_r(hGEOSCtxt, g[i].get())), hGEOSCtxt);
		}
	} else
	if (op == "node") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = geos_ptr(chkNULL(GEOSNode_r(hGEOSCtxt, g[i].get())), hGEOSCtxt);
		}
	} else if (op == "point_on_surface") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = geos_ptr(chkNULL(GEOSPointOnSurface_r(hGEOSCtxt, g[i].get())), hGEOSCtxt);
		}
	} else
#ifdef HAVE340
	if (op == "triangulate") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = geos_ptr(chkNULL(GEOSDelaunayTriangulation_r(hGEOSCtxt, g[i].get(),
				dTolerance[i], bOnlyEdges)), hGEOSCtxt);
	} else
#endif
#ifdef HAVE370
	if (op == "reverse") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = geos_ptr(chkNULL(GEOSReverse_r(hGEOSCtxt, g[i].get())), hGEOSCtxt);
		}
	} else
#endif
#ifdef HAVE390
	if (op == "inscribed_circle") {
		for (size_t i = 0; i < g.size(); i++) {
			out[i] = geos_ptr(chkNULL(GEOSMaximumInscribedCircle_r(hGEOSCtxt, g[i].get(),
				dTolerance[i])), hGEOSCtxt);
		}
	} else
#endif
		Rcpp::stop("invalid operation"); // #nocov

	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out, dim));
	CPL_geos_finish(hGEOSCtxt);
	ret.attr("precision") = sfc.attr("precision");
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}


// [[Rcpp::export]]
Rcpp::List CPL_geos_voronoi(Rcpp::List sfc, Rcpp::List env, double dTolerance = 0.0, int bOnlyEdges = 1) {

	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	std::vector<GeomPtr> g = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	std::vector<GeomPtr> out(sfc.length());

#ifdef HAVE350
	switch (env.size()) {
		case 0: ;
		case 1: {
			std::vector<GeomPtr> g_env = geometries_from_sfc(hGEOSCtxt, env);
			for (size_t i = 0; i < g.size(); i++) {
				out[i] = geos_ptr(chkNULL(GEOSVoronoiDiagram_r(hGEOSCtxt, g[i].get(),
					g_env.size() ? g_env[0].get() : NULL, dTolerance, bOnlyEdges)), hGEOSCtxt);
			}
			break;
		}
		default:
			Rcpp::stop("env should have length 0 or 1"); // #nocov
	}
#else
	Rcpp::stop("voronoi diagrams require a GEOS version >= 3.5.0"); // #nocov
#endif

	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out, dim));
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
	std::vector<GeomPtr> x = geometries_from_sfc(hGEOSCtxt, sfcx, &dim);
	std::vector<GeomPtr> y = geometries_from_sfc(hGEOSCtxt, sfcy, &dim);
	std::vector<GeomPtr> out;
	std::vector<double> index_x, index_y;
	std::vector<size_t> items(x.size());

	if (op == "intersection") {

		bool tree_empty = true;
		TreePtr tree = geos_ptr(GEOSSTRtree_create_r(hGEOSCtxt, 10), hGEOSCtxt);
		for (size_t i = 0; i < x.size(); i++) {
			items[i] = i;
			if (! GEOSisEmpty_r(hGEOSCtxt, x[i].get())) {
				GEOSSTRtree_insert_r(hGEOSCtxt, tree.get(), x[i].get(), &(items[i]));
				tree_empty = false;
			}
		}

		for (size_t i = 0; i < y.size(); i++) {
			// select x's using tree:
			std::vector<size_t> sel;
			sel.reserve(x.size());
			if (! GEOSisEmpty_r(hGEOSCtxt, y[i].get()) && ! tree_empty)
				GEOSSTRtree_query_r(hGEOSCtxt, tree.get(), y[i].get(), cb, &sel);
			std::sort(sel.begin(), sel.end());
			for (size_t item = 0; item < sel.size(); item++) {
				size_t j = sel[item];
				GeomPtr geom = geos_ptr(GEOSIntersection_r(hGEOSCtxt, x[j].get(), y[i].get()), hGEOSCtxt);
				if (geom == nullptr)
					Rcpp::stop("GEOS exception"); // #nocov
				if (! chk_(GEOSisEmpty_r(hGEOSCtxt, geom.get()))) {
					index_x.push_back(j + 1);
					index_y.push_back(i + 1);
					out.push_back(std::move(geom)); // keep
				}
				Rcpp::checkUserInterrupt();
			}
		}

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
				GeomPtr geom = geos_ptr(geom_function(hGEOSCtxt, x[j].get(), y[i].get()), hGEOSCtxt);
				if (geom == nullptr)
					Rcpp::stop("GEOS exception"); // #nocov
				if (! chk_(GEOSisEmpty_r(hGEOSCtxt, geom.get()))) {
					index_x.push_back(j + 1);
					index_y.push_back(i + 1);
					out.push_back(std::move(geom)); // keep
				}
				Rcpp::checkUserInterrupt();
			}
		}
	}

	Rcpp::NumericMatrix m(index_x.size(), 2); // and a set of 1-based indices to x and y
	m(_, 0) = Rcpp::NumericVector(index_x.begin(), index_x.end());
	m(_, 1) = Rcpp::NumericVector(index_y.begin(), index_y.end());

	Rcpp::List ret;
	if ((x.size() == 0 || y.size() == 0) && op != "intersection") {
		if (op == "union" || op == "sym_difference") { // return "the other"
			if (y.size() == 0)
				ret = sfcx;
			else
				ret = sfcy;
		} else // "difference" is asymmetric: x - 0 -> return x
			ret = sfcx;
	} else {
		ret = sfc_from_geometry(hGEOSCtxt, out, dim);
		ret.attr("crs") = sfcx.attr("crs");
		ret.attr("idx") = m;
	}
	CPL_geos_finish(hGEOSCtxt);
	return ret;
}

// [[Rcpp::export]]
std::string CPL_geos_version(bool runtime = false, bool capi = false) {
	if (runtime)
		return GEOSversion();
	else {
		if (capi)
			return GEOS_CAPI_VERSION;
		else
			return GEOS_VERSION;
	}
}

// [[Rcpp::export]]
Rcpp::NumericMatrix CPL_geos_dist(Rcpp::List sfc0, Rcpp::List sfc1,
		Rcpp::CharacterVector which, double par) {
	Rcpp::NumericMatrix out = CPL_geos_binop(sfc0, sfc1, Rcpp::as<std::string>(which), par, "", false)[0];
	return out;
}

// requires 3.6.1: https://trac.osgeo.org/geos/browser/git/NEWS?rev=3.6.2
#ifdef HAVE361
// helper struct & distance function for STRtree:
typedef struct { GEOSGeom g; size_t id; } item_g;

int distance_fn(const void *item1, const void *item2, double *distance, void *userdata) {
	return GEOSDistance_r( (GEOSContextHandle_t) userdata, ((item_g *)item1)->g, ((item_g *)item2)->g, distance);
}

// [[Rcpp::export]]
Rcpp::IntegerVector CPL_geos_nearest_feature(Rcpp::List sfc0, Rcpp::List sfc1) {
	// for every feature in sf0, find the index (1-based) of the nearest feature in sfc1
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();

	int dim = 2;
	std::vector<GeomPtr> gmv0 = geometries_from_sfc(hGEOSCtxt, sfc0, &dim);
	std::vector<GeomPtr> gmv1 = geometries_from_sfc(hGEOSCtxt, sfc1, &dim);
	TreePtr tree = geos_ptr(GEOSSTRtree_create_r(hGEOSCtxt, 10), hGEOSCtxt);
	std::vector<item_g> items(gmv1.size());
	bool tree_is_empty = true;
	for (size_t i = 0; i < gmv1.size(); i++) {
		items[i].id = i + 1; // 1-based
		items[i].g = gmv1[i].get();
		if (!GEOSisEmpty_r(hGEOSCtxt, gmv1[i].get())) {
			GEOSSTRtree_insert_r(hGEOSCtxt, tree.get(), gmv1[i].get(), &(items[i]));
			tree_is_empty = false;
		}
	}
	Rcpp::IntegerVector out(gmv0.size());
	for (size_t i = 0; i < gmv0.size(); i++) {
		if (!GEOSisEmpty_r(hGEOSCtxt, gmv0[i].get()) && !tree_is_empty) {
			item_g item, *ret_item;
			item.id = 0; // is irrelevant
			item.g = gmv0[i].get();
			// now query tree for nearest GEOM at item:
			ret_item = (item_g *) GEOSSTRtree_nearest_generic_r(hGEOSCtxt, tree.get(), &item,
					gmv0[i].get(), distance_fn, hGEOSCtxt);
			if (ret_item != NULL)
				out[i] = ret_item->id; // the index (1-based) of nearest GEOM
			else
				Rcpp::stop("st_nearest_feature: GEOS exception");
		} else
			out[i] = NA_INTEGER;
	}
	CPL_geos_finish(hGEOSCtxt);

	return out;
}
#else
Rcpp::IntegerVector CPL_geos_nearest_feature(Rcpp::List sfc0, Rcpp::List sfc1) {
	Rcpp::stop("GEOS version 3.6.1 required for selecting nearest features");
}
#endif // HAVE_361

// [[Rcpp::export]]
Rcpp::List CPL_geos_nearest_points(Rcpp::List sfc0, Rcpp::List sfc1, bool pairwise) {
	int dim = 2;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GeomPtr> gmv0 = geometries_from_sfc(hGEOSCtxt, sfc0, &dim);
	std::vector<GeomPtr> gmv1 = geometries_from_sfc(hGEOSCtxt, sfc1, &dim);
	Rcpp::List out;
	if (pairwise) {
		if (gmv0.size() != gmv1.size())
			Rcpp::stop("for pairwise nearest points, both arguments need to have the same number of geometries"); // #nocov
		std::vector<GeomPtr> ls(sfc0.size());
		for (size_t i = 0; i < gmv0.size(); i++)
			ls[i] = geos_ptr(GEOSGeom_createLineString_r(hGEOSCtxt, GEOSNearestPoints_r(hGEOSCtxt, gmv0[i].get(), gmv1[i].get())), hGEOSCtxt); // converts as LINESTRING
		out = sfc_from_geometry(hGEOSCtxt, ls, dim);
	} else {
		std::vector<GeomPtr> ls(sfc0.size() * sfc1.size());
		for (size_t i = 0; i < gmv0.size(); i++)
			for (size_t j = 0; j < gmv1.size(); j++)
				ls[(i * gmv1.size()) + j] =
					geos_ptr(GEOSGeom_createLineString_r(hGEOSCtxt, GEOSNearestPoints_r(hGEOSCtxt, gmv0[i].get(), gmv1[j].get())), hGEOSCtxt); // converts as LINESTRING
		out = sfc_from_geometry(hGEOSCtxt, ls, dim);
	}

	CPL_geos_finish(hGEOSCtxt);
	out.attr("precision") = sfc0.attr("precision");
	out.attr("crs") = sfc0.attr("crs");
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

// [[Rcpp::export]]
Rcpp::List CPL_nary_difference(Rcpp::List sfc) {
	// initialize objects
	int dim = 2;
	std::vector<size_t> index;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GeomPtr> x = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	std::vector<GeomPtr> out;
	// initialize trees to find overlapping areas quickly
	for (size_t i = 0; i < x.size(); i++) {
		// if i'th geometry in x is empty then skip it
		if (! GEOSisEmpty_r(hGEOSCtxt, x[i].get())) {
			bool contained = false;
			TreePtr tree = geos_ptr(GEOSSTRtree_create_r(hGEOSCtxt, 10), hGEOSCtxt);
			GeomPtr geom = std::move(x[i]);
			// if out contains geometries than remove overlaps from geom
			if (out.size() > 0) {
				// generate tree for all items in out
				std::vector<size_t> items(out.size());
				for (size_t j = 0; j < out.size(); j++) {
					items[j] = j;
					if (! GEOSisEmpty_r(hGEOSCtxt, out[j].get())) {
						GEOSSTRtree_insert_r(hGEOSCtxt, tree.get(), out[j].get(), &(items[j]));
					}
				}
				// query which geometries in out overlap with geom
				std::vector<size_t> tree_sel;
				GEOSSTRtree_query_r(hGEOSCtxt, tree.get(), geom.get(), cb, &tree_sel);
				// iterate over items in query and erase overlapping areas in geom
				for (size_t j = 0; j < tree_sel.size(); j++) {
					// test if the items are fully contained
					contained = chk_(GEOSContains_r(hGEOSCtxt, out[tree_sel[j]].get(), geom.get()));
					if (contained)
						break;
					// test if the items intersect with geom
					if (chk_(GEOSIntersects_r(hGEOSCtxt, geom.get(), out[tree_sel[j]].get()))) {
						// if they do then erase overlapping parts from geom
						geom = geos_ptr(GEOSDifference_r(hGEOSCtxt, geom.get(), out[tree_sel[j]].get()), hGEOSCtxt);
						if (geom == nullptr)
							Rcpp::stop("GEOS exception"); // #nocov
						// ensure that geom is valid
					}
				}
			}
			// add geom to out if not empty
			if (!contained) {
				index.push_back(i + 1);
				out.push_back(std::move(geom)); // keep
			}
			// check for user interrupt
			Rcpp::checkUserInterrupt();
		}
	}
	// prepare output
	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out, dim)); // destroys out
	ret.attr("crs") = sfc.attr("crs");
	Rcpp::IntegerVector out_index = Rcpp::IntegerVector(index.begin(), index.end());
	ret.attr("idx") = out_index;
	// cleanup
	CPL_geos_finish(hGEOSCtxt);
	// return result
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_nary_intersection(Rcpp::List sfc) {
	// initialize objects
	int dim = 2;
	std::vector< std::vector<size_t> > index;
	GEOSContextHandle_t hGEOSCtxt = CPL_geos_init();
	std::vector<GeomPtr> x = geometries_from_sfc(hGEOSCtxt, sfc, &dim);
	std::vector<GeomPtr> out;
	int errors = 0;
#ifdef HAVE350
	int notice = 0;
	GEOSContext_setNoticeMessageHandler_r(hGEOSCtxt,
		(GEOSMessageHandler_r) __emptyNoticeHandler, (void *) &notice);
	GEOSContext_setErrorMessageHandler_r(hGEOSCtxt,
		(GEOSMessageHandler_r) __countErrorHandler, (void *) &notice);
#endif
	// initialize trees to find overlapping areas quickly
	for (size_t i = 0; i < x.size(); i++) {
		// if i'th geometry in x is empty then skip it
		if (! GEOSisEmpty_r(hGEOSCtxt, x[i].get())) {
			TreePtr tree = geos_ptr(GEOSSTRtree_create_r(hGEOSCtxt, 10), hGEOSCtxt);
			GeomPtr geom = std::move(x[i]);
			// if out contains geometries than remove overlaps from geom
			if (out.size() > 0) {
				// generate tree for all items in out
				std::vector<size_t> items(out.size());
				for (size_t j = 0; j < out.size(); j++) {
					items[j] = j;
					if (! GEOSisEmpty_r(hGEOSCtxt, out[j].get()))
						GEOSSTRtree_insert_r(hGEOSCtxt, tree.get(), out[j].get(), &(items[j]));
				}
				// query which geometries in out overlap with geom
				std::vector<size_t> tree_sel;
				GEOSSTRtree_query_r(hGEOSCtxt, tree.get(), geom.get(), cb, &tree_sel);
				// iterate over items in query and erase overlapping areas in geom
				for (size_t j = 0; j < tree_sel.size(); j++) {
					size_t k = tree_sel[j];
					GeomPtr inters = geos_ptr(GEOSIntersection_r(hGEOSCtxt, out[k].get(), geom.get()), hGEOSCtxt);
					if (geom.get() != nullptr) {
						if (inters == nullptr)
							errors++;
						else if (!chk_(GEOSisEmpty_r(hGEOSCtxt, inters.get()))) { // i and k intersection
							// cut out inters from geom:
							geom = geos_ptr(GEOSDifference_r(hGEOSCtxt, geom.get(), inters.get()), hGEOSCtxt); 
							if (geom == nullptr)
								Rcpp::stop("GEOS exception"); // #nocov
							// cut out inters from out[k]:
							GeomPtr g = geos_ptr(GEOSDifference_r(hGEOSCtxt, out[k].get(), inters.get()), hGEOSCtxt); 
							if (g == nullptr)
								Rcpp::warning("GEOS difference returns NULL"); // #nocov
							else {
								out[k] = std::move(g);
								out.push_back(std::move(inters)); // keep
								std::vector<size_t> idx = index[k]; // k < i, and k might already be an intersection
								idx.push_back(i + 1);
								index.push_back(idx);
							}
						}
					} else
						errors++;
				}
			}
			if (geom != nullptr && ! chk_(GEOSisEmpty_r(hGEOSCtxt, geom.get()))) {
				out.push_back(std::move(geom));
				std::vector<size_t> idx;
				idx.push_back(i + 1);
				index.push_back(idx);
			}
			// check for user interrupt
			Rcpp::checkUserInterrupt();
		}
	} // for i
	if (errors > 0)
		Rcpp::Rcout << "geometry errors: " << errors << std::endl;
#ifdef HAVE350
	if (notice > 0)
		Rcpp::warning("one or more notices ignored");
	GEOSContext_setNoticeHandler_r(hGEOSCtxt, __warningHandler);
	GEOSContext_setErrorHandler_r(hGEOSCtxt, __errorHandler);
#endif
	size_t j = 0;
	for (size_t i = 0; i < out.size(); i++) {
		if (! GEOSisEmpty_r(hGEOSCtxt, out[i].get())) {
			if (i != j) {
				out[j] = std::move(out[i]);
				index[j] = index[i];
			}
			std::sort(index[j].begin(), index[j].end());
			j++;
		}
	}
	out.resize(j);
	index.resize(j);
	// prepare output
	Rcpp::List ret(sfc_from_geometry(hGEOSCtxt, out, dim));
	ret.attr("crs") = sfc.attr("crs");
	Rcpp::List index_list(index.size());
	for (size_t i = 0; i < index.size(); i++) {
		Rcpp::IntegerVector out_index = Rcpp::IntegerVector(index[i].begin(), index[i].end());
		index_list[i] = out_index;
	}
	ret.attr("idx") = index_list;
	// cleanup
	CPL_geos_finish(hGEOSCtxt);
	// return result
	return ret;
}
