#include <Rcpp.h>

#include "../inst/include/bbox.h"

// [[Rcpp::export]]
Rcpp::NumericVector CPL_get_bbox(Rcpp::List sf, int depth = 0) {
	return sf::get_bbox(sf, depth);
}
