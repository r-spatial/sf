#include "Rcpp.h"

using namespace Rcpp;

// [[Rcpp::export(rng=false)]]
List CPL_xy2sfc(NumericMatrix cc, IntegerVector dim, bool to_points, IntegerVector which, bool cc_has_NAs) {
	if (cc.nrow() != dim[0] * dim[1])
		stop("xy2sfc: wrong dimensions"); // #nocov
	List sfc(which.length());
	if (to_points) {
		NumericVector point(2);
		point.attr("class") = CharacterVector::create("XY", "POINT", "sfg");
		for (int i = 0; i < which.length(); i++) {
			int ix = which[i] - 1;
			point(0) = cc(ix, 0);
			point(1) = cc(ix, 1);
			sfc(i) = clone(point);
		}
		sfc.attr("class") = CharacterVector::create("sfc_POINT", "sfc");
	} else {
		for (int i = 0; i < which.length(); i++) {
			int ix = which[i] - 1; // from R, 1-based
			size_t y = ix / (dim[0] - 1); // row index
			size_t x = ix % (dim[0] - 1); // col index
			// Rcpp::Rcout << "x is:" << x << " y is:" << y << std::endl;
			NumericMatrix points(5, 2); // the four corners
			points(0,0) = cc(y * (dim[0]) + x    ,   0); // top left
			points(0,1) = cc(y * (dim[0]) + x    ,   1); // top left
			points(1,0) = cc(y * (dim[0]) + x + 1,   0); // top right
			points(1,1) = cc(y * (dim[0]) + x + 1,   1); // top right
			points(2,0) = cc((y + 1) * (dim[0]) + x + 1, 0); // bottom right
			points(2,1) = cc((y + 1) * (dim[0]) + x + 1, 1); // bottom right
			points(3,0) = cc((y + 1) * (dim[0]) + x    , 0); // bottom left
			points(3,1) = cc((y + 1) * (dim[0]) + x    , 1); // bottom left
			points(4,0) = cc(y * (dim[0]) + x    ,       0); // top left
			points(4,1) = cc(y * (dim[0]) + x    ,       1); // top left
			bool empty = false;
			if (cc_has_NAs) {
				NumericVector xy(8);
				xy(0) = points(0,0);
				xy(1) = points(0,1);
				xy(2) = points(1,0);
				xy(3) = points(1,1);
				xy(4) = points(2,0);
				xy(5) = points(2,1);
				xy(6) = points(3,0);
				xy(7) = points(3,1);
				LogicalVector b = any(is_na(xy));
				if (b[0]) {
					List polygon;
					polygon.attr("class") = CharacterVector::create("XY", "POLYGON", "sfg");
					sfc(i) = polygon; // empty polygon
					empty = true;
				}
			}
			if (!empty) {
				List polygon(1);
				polygon.attr("class") = CharacterVector::create("XY", "POLYGON", "sfg");
				polygon(0) = points;
				sfc(i) = polygon; 
			}
		}
		sfc.attr("class") = CharacterVector::create("sfc_POLYGON", "sfc");
	}
	sfc.attr("precision") = NumericVector::create(0.0);
	return(sfc);
}
