#include <ogr_api.h>
#include <ogr_geometry.h>

#include <Rcpp.h>

#include "gdal.h"

/*
// [[Rcpp::export]]
Rcpp::LogicalVector CPL_is_simple(Rcpp::List sfc) { 
	Rcpp::LogicalVector out(sfc.length());
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	for (size_t i = 0; i < g.size(); i++) {
		out[i] = g[i]->IsSimple();
		delete g[i];
	}
	return out;
}
*/

// [[Rcpp::export]]
Rcpp::NumericVector CPL_area(Rcpp::List sfc) { 
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	Rcpp::NumericVector out(sfc.length());
	for (size_t i = 0; i < g.size(); i++) {
		if (g[i]->getDimension() == 2) {
			OGRSurface *a = (OGRSurface *) g[i];
			out[i] = a->get_Area();
		} else
			out[i] = 0.0;
		delete g[i];
	}
	return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector CPL_length(Rcpp::List sfc) { 
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	Rcpp::NumericVector out(sfc.length());
	for (size_t i = 0; i < g.size(); i++) {
		OGRwkbGeometryType gt = OGR_GT_Flatten(g[i]->getGeometryType());
		if (gt == wkbLineString || gt == wkbCircularString || gt == wkbCompoundCurve || gt == wkbCurve) {
			OGRCurve *a = (OGRCurve *) g[i];
			out[i] = a->get_Length();
		} else {
			OGRGeometryCollection *a = (OGRGeometryCollection *) g[i];
			out[i] = a->get_Length();
		}
		delete g[i];
	}
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_gdal_geom_op(std::string op, Rcpp::List sfc, 
		double bufferDist = 0.0, int nQuadSegs = 30,
		double dTolerance = 0.0, bool preserveTopology = false, 
		int bOnlyEdges = 1, double dfMaxLength = 0.0) {

	if (op == "segmentize" && dfMaxLength <= 0.0)
		throw std::invalid_argument("argument dfMaxLength should be positive\n");
		// breaks, strangely!

	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());

	if (op == "segmentize") {
		for (size_t i = 0; i < g.size(); i++) {
			g[i]->segmentize(dfMaxLength);
			out[i] = g[i];
		}
	} else
		throw std::invalid_argument("invalid operation"); // would leak g and out

	if (op != "segmentize")
		for (size_t i = 0; i < g.size(); i++)
			delete g[i];
	Rcpp::List ret = sfc_from_ogr(out, true);
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}
