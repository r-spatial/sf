#include <iostream>
#include <proj_api.h>

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
Rcpp::NumericMatrix CPL_proj_direct(Rcpp::CharacterVector from_to, Rcpp::NumericMatrix pts) {

	using namespace Rcpp;

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

	if (pj_transform(fromPJ, toPJ, pts.nrow(), 0, xx.data(), yy.data(), NULL) != 0) {
		pj_free(fromPJ); pj_free(toPJ); // #nocov start
		Rcout << "error in pj_transform: " << pj_strerrno(*pj_get_errno_ref()) << std::endl;
		stop("error"); // #nocov end
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
		if (out(i, 0) == HUGE_VAL || out(i, 1) == HUGE_VAL )
		    // || ISNAN(pts[i,0]) || ISNAN(pts[i,1]))
                	    nwarn++; // #nocov
	}
	if (nwarn > 0) 
		warning("one or more projected point(s) not finite"); // #nocov
	return out;
}


extern "C" {
// modified from: rgdal/pkg/src/projectit.cpp

// copied necessary parts from projects.h; full header conflicts with R headers;
// should these things ever change in the proj API, then we're in trouble.
struct PJconsts;
struct PJ_LIST {
	char	*id;		/* projection keyword */
	struct PJconsts	*(*proj)(struct PJconsts*);/* projection entry point */
	char 	* const *descr;	/* description text */
};
struct PJ_LIST *pj_get_list_ref( void );
struct PJ_ELLPS {
	char	*id;	/* ellipse keyword name */
	char	*major;	/* a= value */
	char	*ell;	/* elliptical parameter */
	char	*name;	/* comments */
};
struct PJ_ELLPS *pj_get_ellps_ref( void );
struct PJ_DATUMS {
	char	*id;	 /* datum keyword */
	char	*defn;	 /* ie. "to_wgs84=..." */
	char	*ellipse_id; /* ie from ellipse table */
	char	*comments; /* EPSG code, etc */
};
struct PJ_DATUMS *pj_get_datums_ref( void ); 
struct PJ_UNITS {
	char	*id;		/* units keyword */
	char	*to_meter;	/* multiply by value to get meters */
	char	*name;		/* comments */
#if PJ_VERSION >= 500
	double   factor;       /* to_meter factor in actual numbers */
#endif
};
struct PJ_UNITS *pj_get_units_ref( void );

} // extern "C"

// [[Rcpp::export]]
Rcpp::List CPL_proj_info(int type) {

	Rcpp::List ret;
	switch (type) {
		case 0: {
			Rcpp::List ans(2);
			ans.attr("names") = Rcpp::CharacterVector::create("name", "description");
			int n = 0;
			struct PJ_LIST *lp;
			for (lp = pj_get_list_ref() ; lp->id ; ++lp)
				n++;
			Rcpp::CharacterVector cv0(n);
			Rcpp::CharacterVector cv1(n);
			n = 0;
			for (lp = pj_get_list_ref() ; lp->id ; ++lp) {
				cv0(n) = lp->id;
				cv1(n) = *lp->descr;
				n++;
			}
			ans(0) = cv0;
			ans(1) = cv1;
			ret = ans;
		} break;
		case 1: {
			Rcpp::List ans(4);
			ans.attr("names") = Rcpp::CharacterVector::create("name", 
				"major", "ell", "description");
			int n = 0;
			struct PJ_ELLPS *le;
			for (le = pj_get_ellps_ref(); le->id ; ++le) 
				n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			Rcpp::CharacterVector ans3(n);
			n = 0;
			for (le = pj_get_ellps_ref(); le->id ; ++le) {
				ans0(n) = le->id;
				ans1(n) = le->major;
				ans2(n) = le->ell;
				ans3(n) = le->name;
				n++;
			}
			ans(0) = ans0;
			ans(1) = ans1;
			ans(2) = ans2;
			ans(3) = ans3;
			ret = ans;
		} break;
		case 2: {
			Rcpp::List ans(4);
			ans.attr("names") = Rcpp::CharacterVector::create("name", "ellipse",
				"definition", "description");
			int n = 0;
			struct PJ_DATUMS *ld;
			for (ld = pj_get_datums_ref(); ld->id ; ++ld) 
				n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			Rcpp::CharacterVector ans3(n);
			n = 0;
			for (ld = pj_get_datums_ref(); ld->id ; ++ld) {
				ans0(n) = ld->id;
				ans1(n) = ld->ellipse_id;
				ans2(n) = ld->defn;
				ans3(n) = ld->comments;
				n++;
			}
			ans(0) = ans0;
			ans(1) = ans1;
			ans(2) = ans2;
			ans(3) = ans3;
			ret = ans;
		} break;
		case 3: {
#if PJ_VERSION >= 500
			Rcpp::List ans(4);
			ans.attr("names") = Rcpp::CharacterVector::create("id", "to_meter",
				"name", "factor");
			int n = 0;
			struct PJ_UNITS *ld;
			for (ld = pj_get_units_ref(); ld->id ; ++ld) 
				n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			Rcpp::NumericVector ans3(n);
			n = 0;
			for (ld = pj_get_units_ref(); ld->id ; ++ld) {
				ans0(n) = ld->id;
				ans1(n) = ld->to_meter;
				ans2(n) = ld->name;
				ans3(n) = ld->factor;
				n++;
			}
			ans(0) = ans0;
			ans(1) = ans1;
			ans(2) = ans2;
			ans(3) = ans3;
			ret = ans;
#else
			Rcpp::List ans(3);
			ans.attr("names") = Rcpp::CharacterVector::create("id", "to_meter",
				"name");
			int n = 0;
			struct PJ_UNITS *ld;
			for (ld = pj_get_units_ref(); ld->id ; ++ld) 
				n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			n = 0;
			for (ld = pj_get_units_ref(); ld->id ; ++ld) {
				ans0(n) = ld->id;
				ans1(n) = ld->to_meter;
				ans2(n) = ld->name;
				n++;
			}
			ans(0) = ans0;
			ans(1) = ans1;
			ans(2) = ans2;
			ret = ans;
#endif
		} break;
		default:
			Rcpp::stop("unknown type"); // #nocov
		break;
	}
	return ret;
}
