#include <iostream>
#include <sstream>

#include <Rcpp.h>

#include <geos/geom/Geometry.h>
#include <geos/io/WKBReader.h>
#include <geos/operation/distance/DistanceOp.h>
#include <geos/operation/relate/RelateOp.h>
#include <geos/operation/valid/IsValidOp.h>
#include <geos/geom/IntersectionMatrix.h>

#include "wkb.h"

geos::geom::Geometry *GeomFromRaw(Rcpp::RawVector wkb) {
	std::istringstream s;
	std::istringstream& str(s);
	str.rdbuf()->pubsetbuf( (char *) &(wkb[0]), wkb.size());
	geos::io::WKBReader r;
	return(r.read(str));
}

std::vector<geos::geom::Geometry *> GeomFromSfc(Rcpp::List sfc) {
	double precision = sfc.attr("precision");
	Rcpp::List wkblst = WriteWKB(sfc, false, 1, "XY", false, precision);
	std::vector<geos::geom::Geometry *> g(sfc.length());
	for (int i = 0; i < wkblst.length(); i++)
		g[i] = GeomFromRaw(wkblst[i]);
	return(g);
}

// [[Rcpp::export]]
double st_g_dist(Rcpp::RawVector wkb0, Rcpp::RawVector wkb1) {
	return(geos::operation::distance::DistanceOp::distance(
		GeomFromRaw(wkb0), GeomFromRaw(wkb1)));
}

// [[Rcpp::export]]
Rcpp::CharacterVector st_g_relate(Rcpp::List sfc0, Rcpp::List sfc1) {
	std::vector<geos::geom::Geometry *> gmv0 = GeomFromSfc(sfc0);
	std::vector<geos::geom::Geometry *> gmv1 = GeomFromSfc(sfc1);

	Rcpp::CharacterVector out(sfc0.length() * sfc1.length());
	for (int i = 0; i < sfc0.length(); i++) {
		for (int j = 0; j < sfc1.length(); j++) {
			static geos::geom::IntersectionMatrix* im;
			im = geos::operation::relate::RelateOp::relate(gmv0[i], gmv1[j]);
			out[i * sfc1.length() + j] = im->toString(); // TODO: does this copy the string?
		}
	}
	Rcpp::NumericVector dim(2);
	dim(0) = sfc1.length(); 
	dim(1) = sfc0.length();
	out.attr("dim") = dim;
	return(out);
}

// [[Rcpp::export]]
Rcpp::LogicalVector st_g_isValid(Rcpp::List sfc) { 
	std::vector<geos::geom::Geometry *> gmv = GeomFromSfc(sfc);
	Rcpp::LogicalVector out(sfc.length());
	for (int i; i < sfc.length(); i++)
		out[i] = geos::operation::valid::IsValidOp::isValid(*gmv[i]);
	return(out);
}

// [[Rcpp::export]]
std::string st_g_geosversion(bool b = false) {
	return(geos::geom::geosversion());
}
