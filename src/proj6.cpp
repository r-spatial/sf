#include <iostream>
#ifdef ACCEPT_USE_OF_DEPRECATED_PROJ_API_H // kludge for 6 only
#include <proj.h>

#include "Rcpp.h"

Rcpp::List CPL_proj_info(int type) {

	Rcpp::List ret;
	switch (type) {
		case 0: {
			Rcpp::List ans(2);
			ans.attr("names") = Rcpp::CharacterVector::create("name", "description");
			int n = 0;
/*			struct PJ_LIST *lp;
			for (lp = pj_get_list_ref() ; lp->id ; ++lp)
				n++;*/
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
			//for (lp = pj_get_list_ref() ; lp->id ; ++lp) {
                        for (lp = proj_list_operations() ; lp->id ; ++lp) {
                            if( strcmp(lp->id,"latlong") == 0
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
/*			struct PJ_ELLPS *le;
			for (le = pj_get_ellps_ref(); le->id ; ++le) 
				n++;*/
        		const struct PJ_ELLPS *le;
        		for (le = proj_list_ellps(); le->id ; ++le) n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			Rcpp::CharacterVector ans3(n);
			n = 0;
			//for (le = pj_get_ellps_ref(); le->id ; ++le) {
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
/*			Rcpp::List ans(4);
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
			ret = ans;*/
                        ret = R_NilValue;
		} break;
		case 3: {
			Rcpp::List ans(3);
			ans.attr("names") = Rcpp::CharacterVector::create("id", "to_meter",
				"name");
			int n = 0;
/*			struct PJ_UNITS *ld;
			for (ld = pj_get_units_ref(); ld->id ; ++ld) 
				n++;*/
        		const struct PJ_UNITS *lu;
        		for (lu = proj_list_units(); lu->id ; ++lu) n++;
			Rcpp::CharacterVector ans0(n);
			Rcpp::CharacterVector ans1(n);
			Rcpp::CharacterVector ans2(n);
			n = 0;
			//for (ld = pj_get_units_ref(); ld->id ; ++ld) {
        		for (lu = proj_list_units(); lu->id ; ++lu) {
				ans0(n) = lu->id;
				ans1(n) = lu->to_meter;
				ans2(n) = lu->name;
				n++;
			}
			ans(0) = ans0;
			ans(1) = ans1;
			ans(2) = ans2;
			ret = ans;
		} break;
		default:
			Rcpp::stop("unknown type"); // #nocov
		break;
	}
	return ret;
}

#endif
