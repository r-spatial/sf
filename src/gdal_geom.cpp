#include <ogr_api.h>
#include <ogr_geometry.h>

#include <Rcpp.h>

#include "gdal.h"

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
Rcpp::IntegerVector CPL_gdal_dimension(Rcpp::List sfc, bool NA_if_empty = true) {
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	Rcpp::IntegerVector out(sfc.length());
	for (size_t i = 0; i < g.size(); i++) {
		out[i] = g[i]->getDimension();
		if (NA_if_empty && g[i]->IsEmpty())
			out[i] = NA_INTEGER;
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
Rcpp::List CPL_gdal_segmentize(Rcpp::List sfc, double dfMaxLength = 0.0) {

	if (dfMaxLength <= 0.0)
		throw std::invalid_argument("argument dfMaxLength should be positive\n");

	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	for (size_t i = 0; i < g.size(); i++)
		g[i]->segmentize(dfMaxLength);
	Rcpp::List ret = sfc_from_ogr(g, true);
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_gdal_linestring_sample(Rcpp::List sfc, Rcpp::List distLst) {
	if (sfc.size() != distLst.size())
		throw std::invalid_argument("sfc and dist should have equal length");
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRGeometryCollection *gc = new OGRGeometryCollection;
		Rcpp::NumericVector dists = distLst[i];
		for (size_t j = 0; j < dists.size(); j++) {
			OGRPoint *poPoint  = new OGRPoint;
			((OGRLineString *) g[i])->Value(dists[j], poPoint);
			gc->addGeometry(poPoint);
		}
		out[i] = OGRGeometryFactory::forceToMultiPoint(gc);
	}
	Rcpp::List ret = sfc_from_ogr(out, true);
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}
