#include <Rcpp.h>

#include "zbox.h"

// [[Rcpp::export]]
Rcpp::NumericVector CPL_get_zbox(Rcpp::List sf, int depth = 0) {
	Rcpp::NumericVector bb(2);
	bb[0] = bb[1] = NA_REAL;
	auto n = sf.size();

	switch(depth) {
	case 0: // points:
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericVector pt = sf[i];
			if (i == 0) {
				bb[0] = pt[2];
				bb[1] = pt[2];
			} else {
				bb[0] = std::min(pt[2],bb[0]);
				bb[1] = std::max(pt[2],bb[1]);
			}
		}
		break;

	case 1: // list of matrices:
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericMatrix m = sf[i];
			auto rows = m.nrow();

			if (i == 0) { // initialize:
				if (rows == 0)
					return bb;
				// Rcpp::stop("CPL_get_zbox: invalid geometry");
				bb[0] = m(0,2);
				bb[1] = m(0,2);
			}
			for (decltype(rows) j = 0; j < rows; j++) {
				bb[0] = std::min(m(j,2),bb[0]);
				bb[1] = std::max(m(j,2),bb[1]);
			}
		}
		break;

	default: // recursive list
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericVector bbi = CPL_get_zbox(sf[i], depth - 1); // recurse
			if (! Rcpp::NumericVector::is_na(bbi[0])) {
				if (i == 0) {
					bb[0] = bbi[0];
					bb[1] = bbi[1];
				} else {
					bb[0] = std::min(bbi[0],bb[0]);
					bb[1] = std::max(bbi[1],bb[1]);
				}
			}
		}
		break;
	}
	return bb;
}
