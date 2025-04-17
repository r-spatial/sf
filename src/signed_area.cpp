#include <Rcpp.h>
using namespace Rcpp;

// return signed area of a ring;
// positive area indicates a counter clock-wise ring direction
// https://en.wikipedia.org/wiki/Shoelace_formula

// [[Rcpp::export(rng=false)]]
double CPL_signed_area(NumericMatrix pts) {
	double sum = 0.0;
	double x0, x, y1, y2;

	if (pts.ncol() < 2)
		stop("need at least two columns in matrix\n"); // #nocov

	if (pts.nrow() <= 3)
		return 0.0;
	
	x0 = pts(0, 0);
	for (int i = 2; i < pts.nrow(); i++) {
		x =  pts(i-1, 0) - x0; 
		y1 = pts(i,   1);
		y2 = pts(i-2, 1);
		sum += x * (y1 - y2);
	}
    return sum / 2.0; 
}
