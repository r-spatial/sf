// [[Rcpp::export]]
bool have_geos(bool err = false) {
#ifndef HAVE_GEOS
	return(false);
#else
	return(true);
#endif
}

#include <iostream>
#include <sstream>

#include <Rcpp.h>

#include <geos/geom/Geometry.h>
#include <geos/io/WKBReader.h>
#include <geos/operation/distance/DistanceOp.h>

// [[Rcpp::export]]
double wkb_dist(Rcpp::RawVector wkb1, Rcpp::RawVector wkb2) {

#ifdef HAVE_GEOS
	std::istringstream s1, s2;
	std::istringstream& str1(s1);
	std::istringstream& str2(s2);
	str1.rdbuf()->pubsetbuf( (char *) &(wkb1[0]) , wkb1.size());
	str2.rdbuf()->pubsetbuf( (char *) &(wkb2[0]) , wkb2.size());
	geos::io::WKBReader r;
	geos::geom::Geometry *g1 = r.read(str1);
	geos::geom::Geometry *g2 = r.read(str2);
	return(geos::operation::distance::DistanceOp::distance(g1, g2));
#endif
	throw std::range_error("GEOS not linked to sf");
}
