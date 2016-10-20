#include <ogr_api.h>
#include <ogr_geometry.h>

#include <Rcpp.h>

#include "gdal.h"

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_is_simple(Rcpp::List sfc) { 
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	Rcpp::LogicalVector out(sfc.length());
	for (size_t i = 0; i < g.size(); i++) {
		out[i] = g[i]->IsSimple();
		delete g[i];
	}
	return(out);
}

// [[Rcpp::export]]
Rcpp::List CPL_geom_op(std::string op, Rcpp::List sfc, 
		double bufferDist = 0.0, int nQuadSegs = 30,
		double dTolerance = 0.0, bool preserveTopology = false, 
		int bOnlyEdges = 1, double dfMaxLength = 0.0) {

	if (op == "segmentize" && dfMaxLength <= 0.0)
		throw std::invalid_argument("argument dfMaxLength should be positive\n");

	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());

	if (op == "buffer") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->Buffer(bufferDist, nQuadSegs);
	} else if (op == "boundary") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->Boundary();
	} else if (op == "convex_hull") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->ConvexHull();
	} else if (op == "union_cascaded") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->UnionCascaded();
	} else if (op == "simplify") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = preserveTopology ?  g[i]->SimplifyPreserveTopology(dTolerance) : 
					g[i]->Simplify(dTolerance);
	} else if (op == "triangulate") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->DelaunayTriangulation(dTolerance, bOnlyEdges);
	} else if (op == "polygonize") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->Polygonize();
	} else if (op == "segmentize") {
		for (size_t i = 0; i < g.size(); i++) {
			g[i]->segmentize(dfMaxLength);
			out[i] = g[i];
		}
	} else if (op == "centroid") {
		for (size_t i = 0; i < g.size(); i++) {
			OGRPoint *gm = new OGRPoint;
			g[i]->Centroid(gm);
			out[i] = gm;
		}
	} else
		throw std::invalid_argument("invalid operation"); // would leak g and out

	if (op != "segmentize")
		for (int i = 0; i < g.size(); i++)
			delete g[i];
	Rcpp::List ret = sfc_from_ogr(out, true);
	ret.attr("epsg") = sfc.attr("epsg");
	ret.attr("proj4string") = sfc.attr("proj4string");
	return(ret);
}

// [[Rcpp::export]]
Rcpp::List CPL_geom_op2(std::string op, Rcpp::List sfc, Rcpp::List sf0) {

	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> g0 = ogr_from_sfc(sf0, NULL);
	std::vector<OGRGeometry *> out(g.size());

	if (op == "intersection") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->Intersection(g0[0]);
	} else if (op == "union") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->Union(g0[0]);
	} else if (op == "difference") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->Difference(g0[0]);
	} else if (op == "sym_difference") {
		for (size_t i = 0; i < g.size(); i++)
			out[i] = g[i]->SymDifference(g0[0]);
	} else 
		throw std::invalid_argument("invalid operation"); // would leak g, g0 and out
	// clean up:
	for (int i = 0; i < g.size(); i++)
		delete g[i];
	for (int i = 0; i < g0.size(); i++)
		delete g0[i];

	OGRGeometryFactory f;
	for (int i = 0; i < out.size(); i++)
		if (out[i] == NULL)
			out[i] = f.createGeometry(wkbGeometryCollection);
	Rcpp::List ret = sfc_from_ogr(out, true);
	ret.attr("epsg") = sfc.attr("epsg");
	ret.attr("proj4string") = sfc.attr("proj4string");

	return(ret);
}
