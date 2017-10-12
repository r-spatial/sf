#include <iostream>
#include <sstream>
#include <string>
#include <deque>

#include <boost/geometry.hpp>
#include <boost/geometry/geometries/point_xy.hpp>
#include <boost/geometry/geometries/polygon.hpp>

#include <Rcpp.h>

using namespace Rcpp;

namespace bg = boost::geometry;

// [[Rcpp::export]]
std::string CPL_bg_intersection(std::string p1, std::string p2) {
	typedef bg::model::polygon<bg::model::point<float, 2, bg::cs::spherical_equatorial<bg::degree> > > pol;
	typedef bg::model::point<float, 2, bg::cs::spherical_equatorial<bg::degree> > point;

    pol green, red;

    bg::read_wkt(p1, green);
    bg::read_wkt(p2, red);

    // std::deque<pol> output;
	bg::model::multi_polygon<pol> output;
    bg::intersection(green, red, output);

	/* std::cout << bg::wkt(red);
	std::cout << bg::wkt(green);
	std::cout << bg::wkt(output); */

	bg::model::box<point> b;
	bg::envelope(green, b);
	std::cout << bg::dsv(b) << std::endl;

    std::ostringstream stream;
	stream << bg::wkt(output);
    return stream.str();
}
//  sf:::CPL_bg_intersection("POLYGON((0 0,60 0,60 60,0 60,0 0))", "POLYGON((30 61,31 61,31 62,30 61))")

