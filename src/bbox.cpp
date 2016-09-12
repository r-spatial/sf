#include <Rcpp.h>

#include "bbox.h"

// [[Rcpp::export]]
Rcpp::NumericVector CPL_get_bbox(Rcpp::List sf, int depth = 0) {
	Rcpp::NumericVector bb(4);
	switch(depth) {
		case 0: 
		for (int i = 0; i < sf.size(); i++) {
			Rcpp::NumericVector pt = sf[i];
			if (i == 0) {
				bb(0) = bb(2) = pt(0);
				bb(1) = bb(3) = pt(1);
			} else {
				bb(0) = std::min(pt(0),bb(0));
				bb(1) = std::min(pt(1),bb(1));
				bb(2) = std::max(pt(0),bb(2));
				bb(3) = std::max(pt(1),bb(3));
			}
		}
		break;
		case 1: // matrices
		for (int i = 0; i < sf.size(); i++) {
			Rcpp::NumericMatrix m = sf[i];
			if (i == 0) { // initialize:
				bb(0) = bb(2) = m(0,0);
				bb(1) = bb(3) = m(0,1);
			} 
			for (int j = 0; j < m.nrow(); j++) {
				bb(0) = std::min(m(j,0),bb(0));
				bb(1) = std::min(m(j,1),bb(1));
				bb(2) = std::max(m(j,0),bb(2));
				bb(3) = std::max(m(j,1),bb(3));
			}
		}
		break;
		default:
		for (int i = 0; i < sf.size(); i++) {
			Rcpp::NumericVector bbi = CPL_get_bbox(sf[i], depth - 1); // recurse
			if (i == 0) {
				bb(0) = bbi(0);
				bb(1) = bbi(1);
				bb(2) = bbi(2);
				bb(3) = bbi(3);
			} else {
				bb(0) = std::min(bbi(0),bb(0));
				bb(1) = std::min(bbi(1),bb(1));
				bb(2) = std::max(bbi(2),bb(2));
				bb(3) = std::max(bbi(3),bb(3));
			}
		}
		break;
	}
	return(bb);
}
