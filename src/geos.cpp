#include <iostream>
#include <sstream>

#include <Rcpp.h>

#include <geos/geom/Geometry.h>
#include <geos/io/WKBReader.h>
#include <geos/operation/distance/DistanceOp.h>
#include <geos/operation/relate/RelateOp.h>
#include <geos/operation/valid/IsValidOp.h>
#include <geos/geom/IntersectionMatrix.h>


geos::geom::Geometry *GeomFromRaw(Rcpp::RawVector wkb) {
	std::istringstream s;
	std::istringstream& str(s);
	str.rdbuf()->pubsetbuf( (char *) &(wkb[0]), wkb.size());
	geos::io::WKBReader r;
	return(r.read(str));
}

// [[Rcpp::export]]
double st_g_dist(Rcpp::RawVector wkb0, Rcpp::RawVector wkb1) {
	return(geos::operation::distance::DistanceOp::distance(
		GeomFromRaw(wkb0), GeomFromRaw(wkb1)));
}

// [[Rcpp::export]]
Rcpp::List st_g_relate(Rcpp::RawVector wkb0, Rcpp::RawVector wkb1) {
	static geos::geom::IntersectionMatrix* im;
	im = geos::operation::relate::RelateOp::relate(
		GeomFromRaw(wkb0), GeomFromRaw(wkb1));
	Rcpp::List ls(1);
	ls[0] = im->toString();
	return(ls);
}

// [[Rcpp::export]]
bool st_g_isValid(Rcpp::RawVector wkb) { 
	geos::geom::Geometry *g = GeomFromRaw(wkb);
	return(geos::operation::valid::IsValidOp::isValid(*g));
}

// [[Rcpp::export]]
std::string st_g_geosversion(bool b = false) {
	return(geos::geom::geosversion());
}
