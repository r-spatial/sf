#include <Rcpp.h>

#include "zm_range.h"


Rcpp::NumericVector CPL_get_zm_range(Rcpp::List sf, int pos, int depth) {

	Rcpp::NumericVector bb(2);
	bb[0] = bb[1] = NA_REAL;
	auto n = sf.size();

	switch(depth) {
	case 0: // points:
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericVector pt = sf[i];
			if (i == 0) {
				bb[0] = pt[pos];
				bb[1] = pt[pos];
			} else {
				bb[0] = std::min(pt[pos],bb[0]);
				bb[1] = std::max(pt[pos],bb[1]);
			}
		}
		break;

	case 1: // list of matrices:
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericMatrix m = sf[i];
			auto rows = m.nrow();

			if (i == 0) { // initialize:
				if (rows == 0)
					return bb; // #nocov
				// Rcpp::stop("CPL_get_zbox: invalid geometry");
				bb[0] = m(0,pos);
				bb[1] = m(0,pos);
			}
			for (decltype(rows) j = 0; j < rows; j++) {
				bb[0] = std::min(m(j,pos),bb[0]);
				bb[1] = std::max(m(j,pos),bb[1]);
			}
		}
		break;

	default: // recursive list
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericVector bbi = CPL_get_zm_range(sf[i], pos, depth - 1); // recurse
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

// [[Rcpp::export]]
Rcpp::NumericVector CPL_get_z_range(Rcpp::List sf, int depth = 0) {
	return( CPL_get_zm_range(sf, 2, depth ));
}

// [[Rcpp::export]]
Rcpp::NumericVector CPL_get_m_range(Rcpp::List sf, int depth = 0) {
	return( CPL_get_zm_range(sf, 3, depth ));
}
