#include "Rcpp.h"

#ifdef HAVE_PROJ_H
#include <proj.h>

// [[Rcpp::export]]
Rcpp::List CPL_proj_info(int type) {

	Rcpp::List ret;
	switch (type) {
		case 0: {
			Rcpp::List ans(2);
			ans.attr("names") = Rcpp::CharacterVector::create("name", "description");
			int n = 0;
			const struct PJ_LIST *lp;
			for (lp = proj_list_operations() ; lp->id ; ++lp) {
				if( strcmp(lp->id,"latlong") == 0
						|| strcmp(lp->id,"longlat") == 0
						|| strcmp(lp->id,"geocent") == 0 )
					continue;
				n++;
			}
			Rcpp::CharacterVector cv0(n);
			Rcpp::CharacterVector cv1(n);
			n = 0;
			for (lp = proj_list_operations() ; lp->id ; ++lp) {
				if (strcmp(lp->id,"latlong") == 0
						|| strcmp(lp->id,"longlat") == 0
						|| strcmp(lp->id,"geocent") == 0 )
					continue;
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
			const struct PJ_ELLPS *le;
			for (le = proj_list_ellps(); le->id ; ++le) 
				n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			Rcpp::CharacterVector ans3(n);
			n = 0;
			for (le = proj_list_ellps(); le->id ; ++le) {
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
			ret = R_NilValue;
		} break;
		case 3: {
			Rcpp::List ans(3);
			ans.attr("names") = Rcpp::CharacterVector::create("id", "to_meter",
				"name");
			int n = 0;
#if ((PROJ_VERSION_MAJOR == 7 && PROJ_VERSION_MINOR >= 1) || PROJ_VERSION_MAJOR > 7)
			PROJ_UNIT_INFO** units;
			units = proj_get_units_from_database(nullptr, nullptr, "linear", false, nullptr);
			for (int i = 0; units && units[i]; i++) {
				if (units[i]->proj_short_name) 
					n++;
			}
			Rcpp::CharacterVector ans0(n);
			Rcpp::NumericVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			int item = 0;
			for (int i = 0; units && units[i]; i++) {
				if (units[i]->proj_short_name) {
					ans0[item] = units[i]->proj_short_name;
					ans1[item] = units[i]->conv_factor;
					ans2[item] = units[i]->name;
					item++;
				}
				if (item >= n)
					break;
			}
			proj_unit_list_destroy(units);
			ans(0) = ans0;
			ans(1) = ans1;
			ans(2) = ans2;
#else
			const struct PJ_UNITS *lu;
			for (lu = proj_list_units(); lu->id ; ++lu) 
				n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			n = 0;
			for (lu = proj_list_units(); lu->id ; ++lu) {
				ans0(n) = lu->id;
				ans1(n) = lu->to_meter;
				ans2(n) = lu->name;
				n++;
			}
			ans(0) = ans0;
			ans(1) = ans1;
			ans(2) = ans2;
#endif
			ret = ans;
		} break;
		case 4: {
			Rcpp::List ans(2);
			ans.attr("names") = Rcpp::CharacterVector::create("id", "definition");
			int n = 0;
			const struct PJ_PRIME_MERIDIANS *lpm;
			for (lpm = proj_list_prime_meridians(); lpm->id ; ++lpm) 
				n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			n = 0;
			for (lpm = proj_list_prime_meridians(); lpm->id ; ++lpm) {
				ans0(n) = lpm->id;
				ans1(n) = lpm->defn;
				n++;
			}
			ans(0) = ans0;
			ans(1) = ans1;
			ret = ans;
		} break;
		default:
			Rcpp::stop("unknown type"); // #nocov
		break;
	}
	return ret;
}

#else
#include <proj_api.h>

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

#endif // HAVE_PROJ_H
