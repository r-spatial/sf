#include "Rcpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
List CPL_xy2sfc(NumericMatrix cc, IntegerVector dim, bool to_points, IntegerVector which) {
	NumericVector bb(4);
	bb(0) = cc(0, 0);
	bb(3) = cc(0, 1);
	bb(1) = cc(cc.nrow() - 1, 1);
	bb(2) = cc(cc.nrow() - 1, 0);
	bb.names() = CharacterVector::create("xmin", "ymin", "xmax", "ymax");
	bb.attr("class") = "bbox";
	if (to_points) {
		List ret(which.length());
		NumericVector point(2);
		point.attr("class") = CharacterVector::create("XY", "POINT", "sfg");
		for (int i = 0; i < which.length(); i++) {
			int ix = which[i] - 1;
			point(0) = cc(ix, 0);
			point(1) = cc(ix, 1);
			ret(i) = clone(point);
		}
		ret.attr("class") = CharacterVector::create("sfc_POINT", "sfc");
		ret.attr("precision") = NumericVector::create(0.0);
		ret.attr("bbox") = bb;
		return(ret);
	} else {
		NumericMatrix points(5, 2);
		List polygon(1);
		polygon.attr("class") = CharacterVector::create("XY", "POLYGON", "sfg");
		// List ret((dim[0] - 1) * (dim[1] - 1));
		List ret(which.length());
		for (int i = 0; i < which.length(); i++) {
			int ix = which[i] - 1; // from R, 1-based
			size_t y = ix / (dim[1] - 1); // row index
			size_t x = ix % (dim[0] - 1); // col index
			points(0,0) = cc(y * dim[0] + x,           0); // top left
			points(0,1) = cc(y * dim[0] + x,           1); // top left
			points(1,0) = cc(y * dim[0] + x + 1,       0); // top right
			points(1,1) = cc(y * dim[0] + x + 1,       1); // top right
			points(2,0) = cc((y + 1) * dim[0] + x + 1, 0); // bottom right
			points(2,1) = cc((y + 1) * dim[0] + x + 1, 1); // bottom right
			points(3,0) = cc((y + 1) * dim[0] + x,     0); // bottom left
			points(3,1) = cc((y + 1) * dim[0] + x,     1); // bottom left
			points(4,0) = cc(y * dim[0] + x,           0); // top left
			points(4,1) = cc(y * dim[0] + x,           1); // top left
			polygon(0) = points;
			ret(i) = clone(polygon);
		}
		ret.attr("class") = CharacterVector::create("sfc_POLYGON", "sfc");
		ret.attr("precision") = NumericVector::create(0.0);
		ret.attr("bbox") = bb;
		return(ret);
	}
}
