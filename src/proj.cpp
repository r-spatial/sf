#include <iostream>
#include <proj_api.h>

#include "Rcpp.h"

// [[Rcpp::export]]
std::string CPL_proj_version(bool b = false) {
	int v = PJ_VERSION;	
	std::stringstream buffer;
	buffer << v / 100 << "." << (v / 10) % 10 << "." <<  v % 10;
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
