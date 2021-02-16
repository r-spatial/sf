#include <Rcpp.h>

#include "zm_range.h"

int get_m_position(Rcpp::NumericVector& pt) {
	if( pt.size() < 3 ) {
		Rcpp::stop("m error - expecting at least three coordinates");
	}
	int pos = pt.size() == 3 ? 2 : 3;
	return pos;
}

int get_m_position(Rcpp::NumericMatrix& nm) {
	if( nm.ncol() < 3 ) {
		Rcpp::stop("m error - expecting at least three columns");
	}
	int pos = nm.ncol() == 3 ? 2 : 3;
	return pos;
}

int get_z_position(Rcpp::NumericVector& pt) {
	if( pt.size() < 3 ) {
		Rcpp::stop("z error - expecting three coordinates");
	}
	return 2;
}

int get_z_position(Rcpp::NumericMatrix& nm) {
	if( nm.ncol() < 3 ) {
		Rcpp::stop("z error - expecting three columns;");
	}
	return 2;
}

// [[Rcpp::export]]
Rcpp::NumericVector CPL_get_z_range(Rcpp::List sf, int depth) {

	Rcpp::NumericVector bb(2);
	bb[0] = bb[1] = NA_REAL;
	auto n = sf.size();

	switch(depth) {
	case 0: // points:
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericVector pt = sf[i];
			int pos = get_z_position(pt);
			if (i == 0) {
				bb[0] = pt[pos];
				bb[1] = pt[pos];
			} else {
				bb[0] = std::min(pt[pos],bb[0]);
				bb[1] = std::max(pt[pos],bb[1]);
			}
		}
		break;

	case 1: { // list of matrices:
			bool initialised = false;
			for (decltype(n) i = 0; i < n; i++) {
				Rcpp::NumericMatrix m = sf[i];
				int pos = get_z_position(m);
				auto rows = m.nrow();
	
				if (rows > 0) {
					if (! initialised) { // initialize:
						bb[0] = m(0,pos);
						bb[1] = m(0,pos);
						initialised = true;
					}
					for (decltype(rows) j = 0; j < rows; j++) {
						bb[0] = std::min(m(j,pos),bb[0]);
						bb[1] = std::max(m(j,pos),bb[1]);
					}
				}
			}
		}
		break;

	default: // recursive list
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericVector bbi = CPL_get_z_range(sf[i], depth - 1); // recurse
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
Rcpp::NumericVector CPL_get_m_range(Rcpp::List sf, int depth) {

	Rcpp::NumericVector bb(2);
	bb[0] = bb[1] = NA_REAL;
	auto n = sf.size();

	switch(depth) {
	case 0: // points:
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericVector pt = sf[i];
			int pos = get_m_position(pt);
			if (i == 0) {
				bb[0] = pt[pos];
				bb[1] = pt[pos];
			} else {
				bb[0] = std::min(pt[pos],bb[0]);
				bb[1] = std::max(pt[pos],bb[1]);
			}
		}
		break;

	case 1: { // list of matrices:
			bool initialised = false;
			for (decltype(n) i = 0; i < n; i++) {
				Rcpp::NumericMatrix m = sf[i];
				int pos = get_m_position(m);
				auto rows = m.nrow();
	
				if (rows > 0) {
					if (! initialised) { // initialize:
						bb[0] = m(0,pos);
						bb[1] = m(0,pos);
						initialised = true;
					}
					for (decltype(rows) j = 0; j < rows; j++) {
						bb[0] = std::min(m(j,pos),bb[0]);
						bb[1] = std::max(m(j,pos),bb[1]);
					}
				}
			}
		}
		break;

	default: // recursive list
		for (decltype(n) i = 0; i < n; i++) {
			Rcpp::NumericVector bbi = CPL_get_m_range(sf[i], depth - 1); // recurse
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

