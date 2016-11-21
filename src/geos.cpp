#include <ogr_geometry.h>

#define GEOS_USE_ONLY_R_API // avoid using non-thread-safe GEOSxx functions without _r extension.
#include <geos_c.h>

#include <Rcpp.h>

#include "gdal.h"

std::vector<GEOSGeom> geometries_from_sfc(GEOSContextHandle_t hGEOSCtxt, Rcpp::List sfc) {
	std::vector<OGRGeometry *> ogr = ogr_from_sfc(sfc, NULL);
	std::vector<GEOSGeom> g(ogr.size());
	for (int i = 0; i < ogr.size(); i++) {
		g[i] = ogr[i]->exportToGEOS(hGEOSCtxt);
		delete ogr[i];
	}
	return g;
}

Rcpp::List sfc_from_geometry(GEOSContextHandle_t hGEOSCtxt, std::vector<GEOSGeom> geom) {
	std::vector<OGRGeometry *> ogr(geom.size());
	for (int i = 0; i < geom.size(); i++) {
		ogr[i] = OGRGeometryFactory::createFromGEOS(hGEOSCtxt, geom[i]);
		GEOSGeom_destroy_r(hGEOSCtxt, geom[i]);
	}
	return sfc_from_ogr(ogr, true); // destroy
}

Rcpp::NumericVector get_dim(double dim0, double dim1) {
	Rcpp::NumericVector dim(2);
	dim(0) = dim0;
	dim(1) = dim1;
	return dim;
}

Rcpp::IntegerVector get_which(Rcpp::LogicalVector row) {
	int j = 0;
	for (int i = 0; i < row.length(); i++)
		if (row(i))
			j++;
	Rcpp::IntegerVector ret(j);
	for (int i = 0, j = 0; i < row.length(); i++)
		if (row(i))
			ret(j++) = i + 1; // R is 1-based
	return ret;
}

bool chk_(char value) {
	if (value == 2)
		throw std::range_error("GEOS exception");
	return value == 1;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_binop(Rcpp::List sfc0, Rcpp::List sfc1, std::string op, double par = 0.0, 
		bool sparse = true) {

	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();

	std::vector<GEOSGeom> gmv0 = geometries_from_sfc(hGEOSCtxt, sfc0);
	std::vector<GEOSGeom> gmv1 = geometries_from_sfc(hGEOSCtxt, sfc1);

	Rcpp::List ret_list;

	using namespace Rcpp; // so that later on the (i,_) works
	if (op == "relate") { // character return matrix:
		Rcpp::CharacterVector out(sfc0.length() * sfc1.length());
		for (int i = 0; i < sfc0.length(); i++)
			for (int j = 0; j < sfc1.length(); j++)
				out[j * sfc0.length() + i] = GEOSRelate_r(hGEOSCtxt, gmv0[i], gmv1[j]);
		out.attr("dim") = get_dim(sfc0.length(), sfc1.length());
		ret_list = Rcpp::List::create(out);
	} else if (op == "distance") { // return double matrix:
		Rcpp::NumericMatrix out(sfc0.length(), sfc1.length());
		for (int i = 0; i < gmv0.size(); i++)
			for (int j = 0; j < gmv1.size(); j++) {
				double dist;
				GEOSDistance_r(hGEOSCtxt, gmv0[i], gmv1[j], &dist);
				out(i,j) = dist;
			}
		ret_list = Rcpp::List::create(out);
	} else {
		// other cases: boolean return matrix, either dense or sparse
		Rcpp::LogicalMatrix densemat;
		if (! sparse)  // allocate:
			densemat = Rcpp::LogicalMatrix(sfc0.length(), sfc1.length());
		Rcpp::List sparsemat(sfc0.length());
		for (int i = 0; i < sfc0.length(); i++) { // row
		// TODO: speed up contains, containsproperly, covers, and intersects with prepared geometry i
			Rcpp::LogicalVector rowi(sfc1.length()); 
			if (op == "intersects")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSIntersects_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "disjoint")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSDisjoint_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "touches")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSTouches_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "crosses")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSCrosses_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "within")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSWithin_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "contains")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSContains_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "overlaps")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSOverlaps_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "equals")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSEquals_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "covers")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSCovers_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "covered_by")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSCoveredBy_r(hGEOSCtxt, gmv0[i], gmv1[j]));
			else if (op == "equals_exact")
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = chk_(GEOSEqualsExact_r(hGEOSCtxt, gmv0[i], gmv1[j], par));
			/* else if (op == "is_within_distance") ==>> no C interface??
				for (int j = 0; j < sfc1.length(); j++) 
					rowi(j) = gmv0[i]->isWithinDistance(gmv1[j].get(), par);
			*/
			else
				throw std::range_error("wrong value for op"); // unlikely to happen unless user wants to
			if (! sparse)
				densemat(i,_) = rowi;
			else
				sparsemat[i] = get_which(rowi);
		}
		if (sparse)
			ret_list = sparsemat;
		else
			ret_list = Rcpp::List::create(densemat);
	}
	for (int i = 0; i < gmv0.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, gmv0[i]);
	for (int i = 0; i < gmv1.size(); i++)
		GEOSGeom_destroy_r(hGEOSCtxt, gmv1[i]);
	OGRGeometry::freeGEOSContext(hGEOSCtxt);
	return ret_list;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_geos_is_valid(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();
	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc);
	Rcpp::LogicalVector out(gmv.size());
	for (int i = 0; i < out.length(); i++) {
		out[i] = chk_(GEOSisValid_r(hGEOSCtxt, gmv[i]));
		GEOSGeom_destroy_r(hGEOSCtxt, gmv[i]);
	}
	OGRGeometry::freeGEOSContext(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_geos_union(Rcpp::List sfc) { 
	GEOSContextHandle_t hGEOSCtxt = OGRGeometry::createGEOSContext();
	std::vector<GEOSGeom> gmv = geometries_from_sfc(hGEOSCtxt, sfc);
	std::vector<GEOSGeom> gmv_out(gmv.size());
	for (int i = 0; i < gmv.size(); i++) {
		gmv_out[i] = GEOSUnaryUnion_r(hGEOSCtxt, gmv[i]);
		GEOSGeom_destroy_r(hGEOSCtxt, gmv[i]);
	}
	Rcpp::List out(sfc_from_geometry(hGEOSCtxt, gmv_out)); // destroys gmv
	OGRGeometry::freeGEOSContext(hGEOSCtxt);
	return out;
}

// [[Rcpp::export]]
std::string CPL_geos_version(bool b = false) {
	return GEOS_VERSION;
}

// [[Rcpp::export]]
Rcpp::NumericMatrix CPL_geos_dist(Rcpp::List sfc0, Rcpp::List sfc1) {
	Rcpp::NumericMatrix out = CPL_geos_binop(sfc0, sfc1, "distance", 0.0, false)[0];
	return out;
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_geos_relate(Rcpp::List sfc0, Rcpp::List sfc1) {
	Rcpp::CharacterVector out = CPL_geos_binop(sfc0, sfc1, "relate", 0.0, false)[0];
	return out;	
}
