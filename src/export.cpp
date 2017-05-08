// [[Rcpp::interfaces(cpp)]]

#include "Rcpp.h"

// [[Rcpp::export]]
Rcpp::NumericVector add_one(Rcpp::NumericVector in) {
	for (int i = 0; i < in.size(); i++)
		in(i) = in(i) + 1.0;
	return in;
}
