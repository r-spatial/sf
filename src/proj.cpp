#include <iostream>

#include <ogr_srs_api.h>
#include <cpl_string.h>

#include "Rcpp.h"

#define NO_GDAL_CPP_HEADERS
#include "gdal_sf_pkg.h"

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_proj_h(bool b = false) {
#if defined(HAVE_PROJ_H) && !defined(ACCEPT_USE_OF_DEPRECATED_PROJ_API_H)
	return true;
#else
	return false;
#endif
}

#if defined(HAVE_PROJ_H) && !defined(ACCEPT_USE_OF_DEPRECATED_PROJ_API_H) // new api
# include <proj.h>

#if PROJ_VERSION_MAJOR > 7
# define HAVE_71
#else
# if PROJ_VERSION_MAJOR == 7
#  if PROJ_VERSION_MINOR >= 1
#   define HAVE_71
#  endif
# endif
#endif

// [[Rcpp::export]]
Rcpp::DataFrame CPL_get_pipelines(Rcpp::CharacterVector crs, Rcpp::CharacterVector authority, 
		Rcpp::NumericVector AOI, Rcpp::CharacterVector Use, 
		Rcpp::CharacterVector grid_availability,
		double accuracy = -1.0,
		bool strict_containment = false,
		bool axis_order_auth_compl = false) {
#ifdef HAVE_71
	if (crs.size() != 2)
		Rcpp::stop("length 2 character vector expected");
	const char *auth = NULL;
	if (authority.size())
		auth = authority[0];
	PJ_OPERATION_FACTORY_CONTEXT *factory_ctx = 
		proj_create_operation_factory_context(PJ_DEFAULT_CTX, auth);
	if (accuracy >= 0.0)
		proj_operation_factory_context_set_desired_accuracy(PJ_DEFAULT_CTX, factory_ctx, accuracy);
	if (AOI.size() == 4)
		proj_operation_factory_context_set_area_of_interest(PJ_DEFAULT_CTX, factory_ctx, 
			AOI[0], AOI[1], AOI[2], AOI[3]);
	else if (Use.size() == 1) {
		if (Use[0] == "NONE")
			proj_operation_factory_context_set_crs_extent_use(PJ_DEFAULT_CTX, factory_ctx, 
				PJ_CRS_EXTENT_NONE);
		else if (Use[0] == "BOTH")
			proj_operation_factory_context_set_crs_extent_use(PJ_DEFAULT_CTX, factory_ctx, 
				PJ_CRS_EXTENT_BOTH);
		else if (Use[0] == "INTERSECTION")
			proj_operation_factory_context_set_crs_extent_use(PJ_DEFAULT_CTX, factory_ctx,
				PJ_CRS_EXTENT_INTERSECTION);
		else if (Use[0] == "SMALLEST")
			proj_operation_factory_context_set_crs_extent_use(PJ_DEFAULT_CTX, factory_ctx, 
				PJ_CRS_EXTENT_SMALLEST);
		else
			Rcpp::stop("unknown value for Use");
	}
	// FIXME:
	// handle many more constraining options
	if (strict_containment)
		proj_operation_factory_context_set_spatial_criterion(PJ_DEFAULT_CTX, factory_ctx,
			PROJ_SPATIAL_CRITERION_STRICT_CONTAINMENT);
	else
		proj_operation_factory_context_set_spatial_criterion(PJ_DEFAULT_CTX, factory_ctx,
			PROJ_SPATIAL_CRITERION_PARTIAL_INTERSECTION);

	// PROJ_GRID_AVAILABILITY_USE
	if (grid_availability.size() == 1) {
		if (grid_availability[0] == "USED")
			proj_operation_factory_context_set_grid_availability_use(PJ_DEFAULT_CTX, factory_ctx, 
				PROJ_GRID_AVAILABILITY_USED_FOR_SORTING); // Grid availability is only used for sorting results. 
														  // Operations where some grids are missing will be sorted last.
		else if (grid_availability[0] == "DISCARD")
			proj_operation_factory_context_set_grid_availability_use(PJ_DEFAULT_CTX, factory_ctx, 
							PROJ_GRID_AVAILABILITY_DISCARD_OPERATION_IF_MISSING_GRID); // Completely discard an operation if a required grid is missing.
		else if (grid_availability[0] == "IGNORED")
			proj_operation_factory_context_set_grid_availability_use(PJ_DEFAULT_CTX, factory_ctx, 
							PROJ_GRID_AVAILABILITY_IGNORED); // Ignore grid availability at all. Results will be presented as if all grids were available.
		else if (grid_availability[0] == "AVAILABLE")
			proj_operation_factory_context_set_grid_availability_use(PJ_DEFAULT_CTX, factory_ctx, 
							PROJ_GRID_AVAILABILITY_KNOWN_AVAILABLE); // Results will be presented as if grids known to PROJ 
																	 // (that is registered in the grid_alternatives table of its database) 
																	 // were available. Used typically when networking is enabled.
		else
			Rcpp::stop("Unknown value for grid_availability");
	}

	PJ *source_crs = proj_create(PJ_DEFAULT_CTX, crs[0]);
	if (source_crs == NULL)
		Rcpp::stop(proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX)));
	PJ *target_crs = proj_create(PJ_DEFAULT_CTX, crs[1]);
	if (target_crs == NULL)
		Rcpp::stop(proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX)));

	PJ_OBJ_LIST *obj_list = proj_create_operations(PJ_DEFAULT_CTX, source_crs, target_crs, 
		factory_ctx);
	int n = proj_list_get_count(obj_list);
	Rcpp::CharacterVector id(n);
	Rcpp::CharacterVector description(n);
	Rcpp::CharacterVector definition(n);
	Rcpp::LogicalVector   has_inverse(n);
	Rcpp::LogicalVector   axis_order(n);
	Rcpp::NumericVector   acc(n);
	Rcpp::IntegerVector   grid_count(n);
	Rcpp::LogicalVector   instantiable(n);
	Rcpp::List            grids(n);
	for (int i = 0; i < n; i++) {
		PJ *pj = proj_list_get(PJ_DEFAULT_CTX, obj_list, i);
		if (! axis_order_auth_compl) {
			PJ* orig = pj;
			pj = proj_normalize_for_visualization(PJ_DEFAULT_CTX, orig);
			proj_destroy(orig);
		}
		axis_order(i) = axis_order_auth_compl;
		grid_count(i) = proj_coordoperation_get_grid_used_count(PJ_DEFAULT_CTX, pj);
		instantiable(i) = (bool) proj_coordoperation_is_instantiable(PJ_DEFAULT_CTX, pj);
		PJ_PROJ_INFO info = proj_pj_info(pj);
		description(i) = info.description;
		definition(i) = info.definition;
		if (info.id != NULL)
			id(i) = info.id;
		has_inverse(i) = info.has_inverse != 0;
		if (info.accuracy == -1.0)
			acc(i) = NA_REAL;
		else
			acc(i) = info.accuracy;
		if (grid_count(i) > 0) {
			Rcpp::List g(grid_count(i));
			for (int j = 0; j < grid_count(i); j++) {
				const char *out_short_name, *out_full_name, *out_package_name, *out_url;
				int grid_OK, out_direct_download, out_open_license, out_available;
				grid_OK = proj_coordoperation_get_grid_used(PJ_DEFAULT_CTX, pj,
					j, &out_short_name, &out_full_name, &out_package_name,
					&out_url, &out_direct_download, &out_open_license,
					&out_available);
				if (grid_OK) {
					g(j) = Rcpp::List::create(
						Rcpp::Named("out_short_name") = out_short_name,
						Rcpp::Named("out_full_name") = out_full_name,
						Rcpp::Named("out_package_name") = out_package_name,
						Rcpp::Named("out_url") = out_url,
						Rcpp::Named("out_direct_download") = out_direct_download,
						Rcpp::Named("out_open_license") = out_open_license,
						Rcpp::Named("out_available") = out_available
					);
				}
			}
			grids(i) = g;
		}
		proj_destroy(pj);
	}
	// int sug = proj_get_suggested_operation(PJ_DEFAULT_CTX, *obj_list, PJ_DIRECTION direction, PJ_COORD coord)

	Rcpp::DataFrame df = Rcpp::DataFrame::create( 
		Rcpp::Named("id") = id,
		Rcpp::Named("description") = description,
		Rcpp::Named("definition") = definition,
		Rcpp::Named("has_inverse") = has_inverse,
		Rcpp::Named("accuracy") = acc,
		Rcpp::Named("axis_order") = axis_order,
		Rcpp::Named("grid_count") = grid_count,
		Rcpp::Named("instantiable") = instantiable
	);
	df.attr("grids") = grids;

	proj_destroy(source_crs);
	proj_destroy(target_crs);
	proj_operation_factory_context_destroy(factory_ctx);
	return df;
#else
	Rcpp::warning("PROJ >= 7.1 required");
	return Rcpp::DataFrame::create();
#endif
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_get_data_dir(bool b = false) {
	Rcpp::CharacterVector ret(2);
	ret[0] = proj_info().searchpath;
#if GDAL_VERSION_NUM >= 3000300
	char **ogr_sp = OSRGetPROJSearchPaths();
	Rcpp::CharacterVector ogr_sp_sf = charpp2CV(ogr_sp);
	ret[1] = ogr_sp_sf[0];
	CSLDestroy(ogr_sp);
#else
	ret[1] = "requires GDAL >= 3.0.3";
#endif
	Rcpp::CharacterVector nms(2);
	nms(0) = "PROJ";
	nms(1) = "GDAL";
	ret.attr("names") = nms;
	return ret;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_is_network_enabled(bool b = false) {
#if PROJ_VERSION_MAJOR >= 7
#if GDAL_VERSION_NUM >= 3040000
	if (OSRGetPROJEnableNetwork() != proj_context_is_network_enabled(PJ_DEFAULT_CTX))
		Rcpp::warning("GDAL and PROJ have different settings for network enablement; use sf_use_network() to sync them");
#endif
	return Rcpp::LogicalVector::create(proj_context_is_network_enabled(PJ_DEFAULT_CTX));
#else
	return Rcpp::LogicalVector::create(false);
#endif
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_enable_network(Rcpp::CharacterVector url, bool enable = true) {
#ifdef HAVE_71
	if (enable) {
		proj_context_set_enable_network(PJ_DEFAULT_CTX, 1);
#if GDAL_VERSION_NUM >= 3040000
		OSRSetPROJEnableNetwork(1);
#endif
		if (url.size())
			proj_context_set_url_endpoint(PJ_DEFAULT_CTX, url[0]);
		return Rcpp::CharacterVector::create(proj_context_get_url_endpoint(PJ_DEFAULT_CTX));
	} else { // disable:
		proj_context_set_enable_network(PJ_DEFAULT_CTX, 0);
#if GDAL_VERSION_NUM >= 3040000
		OSRSetPROJEnableNetwork(0);
#endif
		return Rcpp::CharacterVector::create();
	}
#else
	return Rcpp::CharacterVector::create();
#endif
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_set_data_dir(Rcpp::CharacterVector data_dir) {
	if (data_dir.size() != 1)
		Rcpp::stop("data_dir should be size 1 character vector"); // #nocov
	std::string dd = Rcpp::as<std::string>(data_dir);
	const char *cp = dd.c_str();
	proj_context_set_search_paths(PJ_DEFAULT_CTX, 1, &cp);
#if GDAL_VERSION_NUM >= 3000000
	std::vector <char *> dirs = create_options(data_dir, true);
	OSRSetPROJSearchPaths(dirs.data());
#endif
	return true;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_use_proj4_init_rules(Rcpp::IntegerVector v) {
	proj_context_use_proj4_init_rules(PJ_DEFAULT_CTX, v[0]);
	return true;
}

// [[Rcpp::export]]
std::string CPL_proj_version(bool b = false) {

	std::stringstream buffer;
	buffer << PROJ_VERSION_MAJOR << "." << PROJ_VERSION_MINOR << "." << PROJ_VERSION_PATCH;
	return buffer.str();
}

// [[Rcpp::export]]
Rcpp::List CPL_proj_is_valid(std::string proj4string) {
	Rcpp::List out(2);

	proj_context_use_proj4_init_rules(PJ_DEFAULT_CTX, 1);
	PJ *P = proj_create(PJ_DEFAULT_CTX, proj4string.c_str());
	if (P == NULL) {
		out(0) = Rcpp::LogicalVector::create(false);
		out(1) = Rcpp::CharacterVector::create( proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX)));
	} else {
		out(0) = Rcpp::LogicalVector::create(true);
		PJ_PROJ_INFO pi;
		pi = proj_pj_info(P);
		out(1) = Rcpp::CharacterVector::create(pi.description);
		proj_destroy(P);
	}
	return out;
}

// [[Rcpp::export]]
bool CPL_have_datum_files(SEXP foo) {

	// TODO:
	// create a PJ with e.g. conus, check success, if yes destroy, return success
	Rcpp::warning("CPL_have_datum not yet implemented for PROJ6 proj.h interface");
	return true;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix CPL_proj_direct(Rcpp::CharacterVector from_to, Rcpp::NumericMatrix pts, 
		bool keep, bool warn = true, bool authority_compliant = false) {

	using namespace Rcpp;

	if (from_to.size() != 1 && from_to.size() != 2)
		stop("from_to should be size 1 or 2 character vector"); // #nocov
	if (pts.ncol() < 2 || pts.ncol() > 4)
		stop("pts should be 2-, 3- or 4-column numeric vector"); // #nocov
	bool have_z = pts.ncol() > 2; // column 3
	bool have_t = pts.ncol() > 3; // column 4

	proj_context_use_proj4_init_rules(PJ_DEFAULT_CTX, 1); // FIXME: needed?
	PJ *P = NULL;
	if (from_to.size() == 2) // source + target:
		P = proj_create_crs_to_crs(PJ_DEFAULT_CTX, from_to[0], from_to[1], NULL); 
		// PJ_AREA *area);
	else // source to target pipeline:
		P = proj_create(PJ_DEFAULT_CTX, from_to[0]);
	if (P == NULL)
		stop(proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX)));
	if (!authority_compliant && from_to.size() == 2) { // keep lat/lon as lon/lat
		PJ *NewP = proj_normalize_for_visualization(PJ_DEFAULT_CTX, P);
		proj_destroy(P);
		if (NewP == NULL)
			stop(proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX)));
		else
			P = NewP;
	}
	// copy over:
	std::vector<PJ_COORD> x(pts.nrow());
	for (int i = 0; i < pts.nrow(); i++) {
		x.data()[i].lpzt.lam = pts(i, 0);
		x.data()[i].lpzt.phi = pts(i, 1);
		// for default z and t values, see https://proj.org/development/migration.html
		x.data()[i].lpzt.z = have_z ? pts(i, 2) : 0.0;
		x.data()[i].lpzt.t = have_t ? pts(i, 3) : HUGE_VAL; 
	}

	// deg2rad?
	if (proj_angular_output(P, PJ_INV)) {
		for (int i = 0; i < pts.nrow(); i++) {
			x.data()[i].lpzt.lam = proj_torad(x.data()[i].lpzt.lam);
			x.data()[i].lpzt.phi = proj_torad(x.data()[i].lpzt.phi);
		}
	}

	// transform:
	if (keep) {
		// use proj_trans() on individual points, making unprojectable points be NA
		PJ_COORD row = { 0.0, 0.0, 0.0, 0.0 }, projected;
		for (int i = 0; i < pts.nrow(); i++) {
			/*
			row.lpzt.lam = x.data()[i].lpzt.lam;
			row.lpzt.phi = x.data()[i].lpzt.phi;
			row.lpzt.z = x.data()[i].lpzt.z;
			row.lpzt.t = x.data()[i].lpzt.t;
			*/
			row.lpzt = x.data()[i].lpzt;
			projected = proj_trans(P, PJ_FWD, row);
			/*
			x.data()[i].lpzt.lam = projected.lpzt.lam;
			x.data()[i].lpzt.phi = projected.lpzt.phi;
			x.data()[i].lpzt.z = projected.lpzt.z;
			x.data()[i].lpzt.t = projected.lpzt.t;
			*/
			x.data()[i].lpzt = projected.lpzt;
		}
	} else {
		// DEFAULT: use proj_trans_array() on array, returning zero-length if any point is unprojectable
		if (proj_trans_array(P, PJ_FWD, x.size(), x.data())) {
			proj_destroy(P);
			stop(proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX)));
		}
	}

	// rad2deg?
	if (proj_angular_output(P, PJ_FWD)) {
		for (int i = 0; i < pts.nrow(); i++) {
			x.data()[i].lpzt.lam = proj_todeg(x.data()[i].lpzt.lam);
			x.data()[i].lpzt.phi = proj_todeg(x.data()[i].lpzt.phi);
		}
	}
	proj_destroy(P);

	// copy to out matrix:
	NumericMatrix out(pts.nrow(), pts.ncol());
	for (int i = 0; i < out.nrow(); i++) {
		out(i, 0) = x.data()[i].lpzt.lam;
		out(i, 1) = x.data()[i].lpzt.phi;
		if (have_z)
			out(i, 2) = x.data()[i].lpzt.z;
		if (have_t)
			out(i, 3) = x.data()[i].lpzt.t;
	}

	int nwarn = 0;
	for (int i = 0; i < out.nrow(); i++) {
		if (out(i, 0) == HUGE_VAL || out(i, 1) == HUGE_VAL) {
			out(i, 0) = NA_REAL;
			out(i, 1) = NA_REAL;
			if (have_z)
				out(i, 2) = NA_REAL;
			if (have_t)
				out(i, 3) = NA_REAL;
			nwarn++; // #nocov
		}
	}
	if (warn && nwarn > 0)
		warning("one or more projected point(s) not finite"); // #nocov
	return out;
}


#else // if defined(HAVE_PROJ_H) && !defined(ACCEPT_USE_OF_DEPRECATED_PROJ_API_H) i.e., old proj_api:
# include <proj_api.h>

#if PJ_VERSION >= 600
# define PROJ6 1
#endif

Rcpp::DataFrame CPL_get_pipelines(Rcpp::CharacterVector crs, Rcpp::CharacterVector authority, 
		Rcpp::NumericVector AOI, Rcpp::CharacterVector Use, 
		Rcpp::CharacterVector grid_availability,
		double accuracy = -1.0,
		bool strict_containment = false,
		bool axis_order_auth_compl = false) {

	Rcpp::stop("PROJ 7 required");
	return Rcpp::DataFrame::create();
}

Rcpp::LogicalVector CPL_is_network_enabled(bool b = false) {
#if PROJ_VERSION_MAJOR >= 7
	return Rcpp::LogicalVector::create(proj_context_is_network_enabled(PJ_DEFAULT_CTX));
#else
	return Rcpp::LogicalVector::create(false);
#endif
}

Rcpp::CharacterVector CPL_enable_network(Rcpp::CharacterVector url, bool enable = true) {
#if PROJ_VERSION_MAJOR >= 7
	if (enable) {
		proj_context_set_enable_network(PJ_DEFAULT_CTX, 1);
		if (url.size())
			proj_context_set_url_endpoint(PJ_DEFAULT_CTX, url[0]);
		return Rcpp::CharacterVector::create(proj_context_get_url_endpoint(PJ_DEFAULT_CTX));
	} else { // disable:
		proj_context_set_enable_network(PJ_DEFAULT_CTX, 0);
		return Rcpp::CharacterVector::create();
	}
#else
	return Rcpp::CharacterVector::create();
#endif
}

Rcpp::CharacterVector CPL_get_data_dir(bool b = false) {
#if PROJ_VERSION_MAJOR >= 7
	return Rcpp::CharacterVector(proj_info().searchpath);
#else
	return Rcpp::CharacterVector(NA_STRING);
#endif
}

Rcpp::LogicalVector CPL_set_data_dir(std::string data_dir) { // #nocov start
	return false;
}

Rcpp::LogicalVector CPL_use_proj4_init_rules(Rcpp::IntegerVector v) {
	return false;
}  // #nocov end

#if PJ_VERSION == 480
extern "C" {
FILE *pj_open_lib(projCtx, const char *, const char *);
}
#endif

#include "Rcpp.h"

std::string CPL_proj_version(bool b = false) {
	int v = PJ_VERSION;
	std::stringstream buffer;
	buffer << v / 100 << "." << (v / 10) % 10 << "." << v % 10;
	return buffer.str();
}

Rcpp::List CPL_proj_is_valid(std::string proj4string) {
	Rcpp::List out(2);
	projPJ pj = pj_init_plus(proj4string.c_str());
	if (pj == NULL) {
		out(0) = Rcpp::LogicalVector::create(false);
		out(1) = Rcpp::CharacterVector::create(pj_strerrno(*pj_get_errno_ref()));
	} else {
		out(0) = Rcpp::LogicalVector::create(true);
		char *def = pj_get_def(pj, 0);
		out(1) = Rcpp::CharacterVector::create(def);
		pj_free(pj);
		free(def);
	}
	return out;
}

bool CPL_have_datum_files(SEXP foo) {

#if PJ_VERSION <= 480
	FILE *fp;
#else
	PAFile fp;
#endif
	projCtx ctx;
	ctx = pj_get_default_ctx();
	fp = pj_open_lib(ctx, "conus", "rb");
	if (fp != NULL) {
#if PJ_VERSION <= 480
		fclose(fp);
#else
		pj_ctx_fclose(ctx, fp);
#endif
		return true;
	} else
		return false; // #nocov
}

Rcpp::NumericMatrix CPL_proj_direct(Rcpp::CharacterVector from_to, Rcpp::NumericMatrix pts, 
		bool keep, bool warn = true, bool authority_compliant = false) {

	using namespace Rcpp;

	if (authority_compliant)
		stop("authority_compliant = TRUE requires the new PROJ (proj.h) interface");
	if (from_to.size() != 2)
		stop("from_to should be size 2 character vector"); // #nocov
	if (pts.ncol() != 2)
		stop("pts should be 2-column numeric vector"); // #nocov

	projPJ fromPJ, toPJ;

	if (!(fromPJ = pj_init_plus(from_to[0])))
		stop(pj_strerrno(*pj_get_errno_ref()));

	if (!(toPJ = pj_init_plus(from_to[1])))
		stop(pj_strerrno(*pj_get_errno_ref()));

	// copy over:
	std::vector<double> xx(pts.nrow()), yy(pts.nrow());
	for (int i = 0; i < pts.nrow(); i++) {
   		 xx[i] = pts(i, 0);
   		 yy[i] = pts(i, 1);
	}
	if (pj_is_latlong(fromPJ)) {
		for (int i = 0; i < pts.nrow(); i++) {
	   		 xx[i] *= DEG_TO_RAD;
	   		 yy[i] *= DEG_TO_RAD;
		}
	}

//	for (int i = 0; i < pts.nrow(); i++)
//  		 Rcout << xx[i] << " " << yy[i] << std::endl;
	if (keep) {
		// use proj_trans() on individual points, making unprojectable points be NA
		// FIXME: not tested, since author has no access to the old proj API.
		double thisx, thisy;
		for (int i = 0; i < pts.nrow(); i++) {
			thisx = xx[i];
			thisy = yy[i];
			if (pj_transform(fromPJ, toPJ, 1, 0, &thisx, &thisy, NULL) != 0) {
				xx[i] = R_PosInf;
				yy[i] = R_PosInf;
			} else {
				xx[i] = thisx;
				yy[i] = thisy;
			}
		}
	} else {
   		// DEFAULT: use proj_trans_array() on array, returning zero-length if any point is unprojectable
		if (pj_transform(fromPJ, toPJ, pts.nrow(), 0, xx.data(), yy.data(), NULL) != 0) {
			pj_free(fromPJ); pj_free(toPJ); // #nocov start
			Rcout << "error in pj_transform: " << pj_strerrno(*pj_get_errno_ref()) << std::endl;
			stop("error"); // #nocov end
		}
	}
	pj_free(fromPJ);
	if (pj_is_latlong(toPJ)) {
		for (int i = 0; i < pts.nrow(); i++) {
	   			 xx[i] *= RAD_TO_DEG;
	   			 yy[i] *= RAD_TO_DEG;
		}
	}
	// copy to out matrix:
	NumericMatrix out(pts.nrow(), pts.ncol());
	for (int i = 0; i < out.nrow(); i++) {
   		 out(i, 0) = xx[i];
   		 out(i, 1) = yy[i];
	}
	pj_free(toPJ);
	int nwarn = 0;
	for (int i = 0; i < out.nrow(); i++) {
		if (out(i, 0) == HUGE_VAL || out(i, 1) == HUGE_VAL) {
			out(i, 0) = NA_REAL;
			out(i, 1) = NA_REAL;
			nwarn++; // #nocov
		}
	}
	if (warn && nwarn > 0) 
		warning("one or more projected point(s) not finite"); // #nocov
	return out;
}
#endif
