#include <iostream>

#include "Rcpp.h"

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

Rcpp::CharacterVector CPL_get_data_dir(bool b = false) {
	return Rcpp::CharacterVector(proj_info().searchpath);
}

Rcpp::LogicalVector CPL_is_network_enabled(bool b = false) {
#if PROJ_VERSION_MAJOR >= 7
	return Rcpp::LogicalVector::create(proj_context_is_network_enabled(PJ_DEFAULT_CTX));
#else
	return Rcpp::LogicalVector::create(false);
#endif
}

Rcpp::LogicalVector CPL_set_data_dir(std::string data_dir) {
	const char *cp = data_dir.c_str();
	proj_context_set_search_paths(PJ_DEFAULT_CTX, 1, &cp);
	return true;
}

Rcpp::LogicalVector CPL_use_proj4_init_rules(Rcpp::IntegerVector v) {
	proj_context_use_proj4_init_rules(PJ_DEFAULT_CTX, v[0]);
	return true;
}

std::string CPL_proj_version(bool b = false) {

	std::stringstream buffer;
	buffer << PROJ_VERSION_MAJOR << "." << PROJ_VERSION_MINOR << "." << PROJ_VERSION_PATCH;
	return buffer.str();
}

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

bool CPL_have_datum_files(SEXP foo) {

	// TODO:
	// create a PJ with e.g. conus, check success, if yes destroy, return success
	Rcpp::warning("CPL_have_datum not yet implemented for PROJ6 proj.h interface");
	return true;
}

Rcpp::NumericMatrix CPL_proj_direct(Rcpp::CharacterVector from_to, Rcpp::NumericMatrix pts, 
		Rcpp::IntegerVector keep, bool warn = true) {

	using namespace Rcpp;

	if (from_to.size() != 2)
		stop("from_to should be size 2 character vector"); // #nocov
	if (pts.ncol() != 2)
		stop("pts should be 2-column numeric vector"); // #nocov
	if (keep.size() != 1)
		stop("keep should be a single integer"); // #nocov

	proj_context_use_proj4_init_rules(PJ_DEFAULT_CTX, 1);
	PJ *P = proj_create_crs_to_crs(PJ_DEFAULT_CTX, from_to[0], from_to[1], NULL); // PJ_AREA *area);
	if (P == NULL)
		stop(proj_errno_string(proj_context_errno(PJ_DEFAULT_CTX)));
	// copy over:
	std::vector<PJ_COORD> x(pts.nrow());
	for (int i = 0; i < pts.nrow(); i++) {
   		 x.data()[i].lp.lam = pts(i, 0);
   		 x.data()[i].lp.phi = pts(i, 1);
	}

	// deg2rad?
	if (proj_angular_output(P, PJ_INV)) {
		for (int i = 0; i < pts.nrow(); i++) {
			x.data()[i].lp.lam = proj_torad(x.data()[i].lp.lam);
			x.data()[i].lp.phi = proj_torad(x.data()[i].lp.phi);
		}
	}

//	for (int i = 0; i < pts.nrow(); i++)
//  		 Rcout << xx[i] << " " << yy[i] << std::endl;

	// transform:
        if (keep[0] == 1) {
            // use proj_trans() on individual points, making unprojectable points be NA
            PJ_COORD row, projected;
            for (int i = 0; i < pts.nrow(); i++) {
                row.lp.lam = x.data()[i].lp.lam;
                row.lp.phi = x.data()[i].lp.phi;
                projected = proj_trans(P, PJ_FWD, row);
                x.data()[i].lp.lam = projected.lp.lam;
                x.data()[i].lp.phi = projected.lp.phi;
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
			x.data()[i].lp.lam = proj_todeg(x.data()[i].lp.lam);
			x.data()[i].lp.phi = proj_todeg(x.data()[i].lp.phi);
		}
	}
	proj_destroy(P);

	// copy to out matrix:
	NumericMatrix out(pts.nrow(), pts.ncol());
	for (int i = 0; i < out.nrow(); i++) {
   		 out(i, 0) = x.data()[i].lp.lam;
   		 out(i, 1) = x.data()[i].lp.phi;
	}

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


#else // if defined(HAVE_PROJ_H) && !defined(ACCEPT_USE_OF_DEPRECATED_PROJ_API_H) i.e., old proj_api:
# include <proj_api.h>

#if PJ_VERSION >= 600
# define PROJ6 1
#endif

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_is_network_enabled(bool b = false) {
#if PROJ_VERSION_MAJOR >= 7
	return Rcpp::LogicalVector::create(proj_context_is_network_enabled(PJ_DEFAULT_CTX));
#else
	return Rcpp::LogicalVector::create(false);
#endif
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_get_data_dir(bool b = false) {
#if PROJ_VERSION_MAJOR >= 7
	return Rcpp::CharacterVector(proj_info().searchpath);
#else
	return Rcpp::CharacterVector(NA_STRING);
#endif
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_set_data_dir(std::string data_dir) { // #nocov start
	return false;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_use_proj4_init_rules(Rcpp::IntegerVector v) {
	return false;
}                                                            // #nocov end

#if PJ_VERSION == 480
extern "C" {
FILE *pj_open_lib(projCtx, const char *, const char *);
}
#endif

#include "Rcpp.h"

// [[Rcpp::export]]
std::string CPL_proj_version(bool b = false) {
	int v = PJ_VERSION;
	std::stringstream buffer;
	buffer << v / 100 << "." << (v / 10) % 10 << "." << v % 10;
	return buffer.str();
}

// [[Rcpp::export]]
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

// [[Rcpp::export]]
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

// [[Rcpp::export]]
Rcpp::NumericMatrix CPL_proj_direct(Rcpp::CharacterVector from_to, Rcpp::NumericMatrix pts, 
		Rcpp::IntegerVector keep, bool warn = true) {

	using namespace Rcpp;

	if (from_to.size() != 2)
		stop("from_to should be size 2 character vector"); // #nocov
	if (pts.ncol() != 2)
		stop("pts should be 2-column numeric vector"); // #nocov
        if (keep.size() != 1)
                stop("keep should be a single integer"); // #nocov

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
        if (keep[0] == 1) {
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
#endif // defined() etc

