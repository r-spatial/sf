#include <ogr_api.h>
#include <ogr_geometry.h>

#include <Rcpp.h>

#include "gdal_sf_pkg.h"

// [[Rcpp::export]]
Rcpp::NumericVector CPL_area(Rcpp::List sfc) { 
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	Rcpp::NumericVector out(sfc.length());
	for (size_t i = 0; i < g.size(); i++) {
		if (g[i]->getDimension() == 2) {
			OGRwkbGeometryType gt = OGR_GT_Flatten(g[i]->getGeometryType());
			if (OGR_GT_IsSubClassOf(gt, wkbGeometryCollection)) {
				// will match OGRMultiPolygon, OGRMultiSurface and OGRGeometryCollection
				OGRGeometryCollection *gc = (OGRGeometryCollection *) g[i];
				out[i] = gc->get_Area();
			} else if (OGR_GT_IsSurface(gt)) {
				OGRSurface *surf = (OGRSurface *) g[i];
				out[i] = surf->get_Area();
			} else {
				out[i] = 0.0; // not supposed to happen, but who knows...
			}
		} else
			out[i] = 0.0;
		OGRGeometryFactory::destroyGeometry(g[i]);
	}
	return out;
}

// [[Rcpp::export]]
Rcpp::IntegerVector CPL_gdal_dimension(Rcpp::List sfc, bool NA_if_empty = true) {
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	Rcpp::IntegerVector out(sfc.length());
	for (size_t i = 0; i < g.size(); i++) {
		if (NA_if_empty && g[i]->IsEmpty())
			out[i] = NA_INTEGER;
		else
			out[i] = g[i]->getDimension();
		OGRGeometryFactory f;
		f.destroyGeometry(g[i]);
	}
	return out;
}

// [[Rcpp::export]]
Rcpp::NumericVector CPL_length(Rcpp::List sfc) { 
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	Rcpp::NumericVector out(sfc.length());
	for (size_t i = 0; i < g.size(); i++) {
		OGRwkbGeometryType gt = OGR_GT_Flatten(g[i]->getGeometryType());
		switch (gt) {
			case wkbPoint: 
			case wkbMultiPoint:
			case wkbPolygon:
			case wkbMultiPolygon:
				out[i] = 0.0;
				break;
			case wkbLineString:
			case wkbCircularString:
			case wkbCompoundCurve:
			case wkbCurve: {
					OGRCurve *a = (OGRCurve *) g[i];
					out[i] = a->get_Length();
				}
				break;
			default: {
					if (OGR_GT_IsSubClassOf(gt, wkbGeometryCollection)) {
						OGRGeometryCollection *a = (OGRGeometryCollection *) g[i];
						out[i] = a->get_Length();
					} else {
						out[i] = 0.0;
					}
				}
		}
		OGRGeometryFactory f;
		f.destroyGeometry(g[i]);
	}
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_gdal_segmentize(Rcpp::List sfc, double dfMaxLength = 0.0) {

	if (dfMaxLength <= 0.0)
		Rcpp::stop("argument dfMaxLength should be positive\n");

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
		Rcpp::stop("sfc and dist should have equal length"); // #nocov
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		if (wkbFlatten(g[i]->getGeometryType()) != wkbLineString)
			Rcpp::stop("CPL_gdal_linestring_sample only available for LINESTRING"); // #nocov
		OGRGeometryCollection *gc = new OGRGeometryCollection;
		Rcpp::NumericVector dists = distLst[i];
		for (int j = 0; j < dists.size(); j++) {
			OGRPoint *poPoint  = new OGRPoint;
			((OGRLineString *) g[i])->Value(dists[j], poPoint);
			gc->addGeometryDirectly(poPoint);
		}
		out[i] = OGRGeometryFactory::forceToMultiPoint(gc);
	}
	Rcpp::List ret = sfc_from_ogr(g, true); // releases g
	ret = sfc_from_ogr(out, true); // releases out
	ret.attr("crs") = sfc.attr("crs");
	return ret;
}
