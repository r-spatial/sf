#include <iostream>
#include <sstream>

#include <Rcpp.h>

#include <geos/geom/Geometry.h>
#include <geos/io/WKBReader.h>
#include <geos/operation/distance/DistanceOp.h>
#include <geos/operation/relate/RelateOp.h>
#include <geos/geom/IntersectionMatrix.h>

// [[Rcpp::export]]
double wkb_dist(Rcpp::RawVector wkb0, Rcpp::RawVector wkb1) {

	std::istringstream s0, s1;
	std::istringstream& str0(s0);
	std::istringstream& str1(s1);
	str0.rdbuf()->pubsetbuf( (char *) &(wkb0[0]) , wkb0.size());
	str1.rdbuf()->pubsetbuf( (char *) &(wkb1[0]) , wkb1.size());
	geos::io::WKBReader r;
	geos::geom::Geometry *g0 = r.read(str0);
	geos::geom::Geometry *g1 = r.read(str1);
	return(geos::operation::distance::DistanceOp::distance(g0, g1));
}

// [[Rcpp::export]]
Rcpp::List relate(Rcpp::RawVector wkb0, Rcpp::RawVector wkb1) {
	std::istringstream s0, s1;
	std::istringstream& str0(s0);
	std::istringstream& str1(s1);
	str0.rdbuf()->pubsetbuf( (char *) &(wkb0[0]) , wkb0.size());
	str1.rdbuf()->pubsetbuf( (char *) &(wkb1[0]) , wkb1.size());
	geos::io::WKBReader r;
	geos::geom::Geometry *g0 = r.read(str0);
	geos::geom::Geometry *g1 = r.read(str1);
	static geos::geom::IntersectionMatrix* im;
	im = geos::operation::relate::RelateOp::relate(g0, g1);
	Rcpp::List ls(1);
	ls[0] = im->toString();
	return(ls);
}

