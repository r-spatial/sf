#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List points_cpp(NumericMatrix pts, CharacterVector gdim = "XY") {
	int n = pts.nrow();
	List out(n);
	CharacterVector cls = CharacterVector::create(gdim[0], "POINT", "sfg");
	for (int i = 0; i < n; i++) {

		NumericVector lp = pts(i, _);
		lp.attr("class") = cls;
		out[i] = lp;
	}
	return out;
}
