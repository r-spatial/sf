#include <ogr_api.h>
#include <ogr_geometry.h>

#include <Rcpp.h>

#include "gdal.h"

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_is_simple(Rcpp::List sfc) { 
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	Rcpp::LogicalVector out(sfc.length());
	for (size_t i = 0; i < g.size(); i++)
		out[i] = g[i]->IsSimple();
	return(out);
}

// [[Rcpp::export]]
Rcpp::List CPL_geom_op(std::string op, Rcpp::List sfc, 
		double bufferDist = 0.0, int nQuadSegs = 30,
		double dTolerance = 0.0, bool preserveTopology = false, 
		int bOnlyEdges = 1, double dfMaxLength = 0.0) {

	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	if (op == "buffer") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->Buffer(bufferDist, nQuadSegs);
	} else if (op == "boundary") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->Boundary();
	} else if (op == "convexhull") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->ConvexHull();
	} else if (op == "unioncascaded") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->UnionCascaded();
	} else if (op == "simplify") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = preserveTopology ?  g[i]->SimplifyPreserveTopology(dTolerance) : g[i]->Simplify(dTolerance);
	} else if (op == "triangulate") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->DelaunayTriangulation(dTolerance, bOnlyEdges);
	} else if (op == "polygonize") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->Polygonize();
	} else if (op == "segmentize") {
		if (dfMaxLength <= 0.0)
			throw std::invalid_argument("argument dfMaxLength should be positive\n");
		for (size_t i = 0; i < g.size(); i++)
			g[i]->segmentize(dfMaxLength);
	} else if (op == "centroid") {
		for (size_t i = 0; i < g.size(); i++) {
			OGRPoint *gm = new OGRPoint;
			g[i]->Centroid(gm);
			delete g[i];
			g[i] = gm;
		}
	} else
		throw std::invalid_argument("invalid operation");

	Rcpp::List ret = sfc_from_ogr(g, true);
	ret.attr("epsg") = sfc.attr("epsg");
	ret.attr("proj4string") = sfc.attr("proj4string");
	return(ret);
}

// [[Rcpp::export]]
Rcpp::List CPL_geom_op2(std::string op, Rcpp::List sfc, Rcpp::List sf0) {

	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> g0 = ogr_from_sfc(sf0, NULL);

	if (op == "intersection") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->Intersection(g0[0]);
	} else if (op == "union") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->Union(g0[0]);
	} else if (op == "difference") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->Difference(g0[0]);
	} else if (op == "sym_difference") {
		for (size_t i = 0; i < g.size(); i++)
			g[i] = g[i]->SymDifference(g0[0]);
	} else 
		throw std::invalid_argument("invalid operation");
	Rcpp::List ret = sfc_from_ogr(g, true);
	ret.attr("epsg") = sfc.attr("epsg");
	ret.attr("proj4string") = sfc.attr("proj4string");
	return(ret);
}
