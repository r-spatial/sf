#include <gdal.h>
#include <gdal_alg.h>
#include <gdal_priv.h> // GDALDriver
#include <ogr_api.h>
#include <ogr_geometry.h>
#include <ogr_srs_api.h>
#include <ogr_spatialref.h>
#include <cpl_conv.h>
#include <cpl_string.h>

#include <stdlib.h> // atoi
#include <string.h>

#include <Rcpp.h>

#include "wkb.h"

//
// Returns errors to R
// Note only case 4 actually returns immediately
// Lower error codes are recoverable
// Compile with -DCONTINUE_ON_ERROR to ignore fatal errors
// This may be needed when running R under a different main loop
//
static void __err_handler(CPLErr eErrClass, int err_no, const char *msg)
{
    switch ( eErrClass )
    {
        case 0:
            break; // #nocov
        case 1:
        case 2:
            Rf_warning("GDAL Message %d: %s\n", err_no, msg); // #nocov
            break; // #nocov
        case 3:
            Rf_warning("GDAL Error %d: %s\n", err_no, msg);
            break;
        case 4:
            Rf_warning("GDAL Error %d: %s\n", err_no, msg); // #nocov
            #ifndef CONTINUE_ON_ERROR
            Rcpp::stop("Unrecoverable GDAL error\n"); // #nocov
            #endif
            break;        
        default:
            Rf_warning("Received invalid error class %d (errno %d: %s)\n", eErrClass, err_no, msg); // #nocov
            break; // #nocov
    }
    return;
}

// [[Rcpp::export]]
void CPL_gdal_init()
{
    CPLSetErrorHandler((CPLErrorHandler)__err_handler);
    GDALAllRegister();
    OGRRegisterAll();
}

// [[Rcpp::export]]
void CPL_gdal_cleanup_all()
{
    OGRCleanupAll();
    OSRCleanup();
}

// [[Rcpp::export]]
const char* CPL_gdal_version(const char* what = "RELEASE_NAME")
{
	return GDALVersionInfo(what);
}

void handle_error(OGRErr err) {
	if (err != 0) {
		if (err == OGRERR_NOT_ENOUGH_DATA)
			Rcpp::Rcout << "OGR: Not enough data " << std::endl; // #nocov
		else if (err == OGRERR_UNSUPPORTED_GEOMETRY_TYPE)
			Rcpp::Rcout << "OGR: Unsupported geometry type" << std::endl;
		else if (err == OGRERR_CORRUPT_DATA)
			Rcpp::Rcout << "OGR: Corrupt data" << std::endl;
		else 
			Rcpp::Rcout << "Error code: " << err << std::endl; // #nocov
		throw std::range_error("OGR error");
	}
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_parameters(std::string p4s) {
	Rcpp::List out(6);
	OGRSpatialReference *srs = new OGRSpatialReference;
	handle_error(srs->importFromProj4(p4s.c_str()));
	out(0) = Rcpp::NumericVector::create(srs->GetSemiMajor());
	out(1) = Rcpp::NumericVector::create(srs->GetInvFlattening());
	out(2) = Rcpp::CharacterVector::create(srs->GetAttrValue("UNIT", 0));
	out(3) = Rcpp::LogicalVector::create(srs->IsVertical());
	char *cp;
	srs->exportToPrettyWkt(&cp);
	out(4) = Rcpp::CharacterVector::create(cp);
	CPLFree(cp);
	srs->exportToWkt(&cp);
	out(5) = Rcpp::CharacterVector::create(cp);
	CPLFree(cp);
	return out;
}

Rcpp::CharacterVector get_dim(Rcpp::List sfc) {

	if (sfc.length() == 0)
		return "XY";

	// we have data:
	Rcpp::CharacterVector cls = sfc.attr("class");
	unsigned int tp = make_type(cls[0], "", false, NULL, 0);
	if (tp == SF_Unknown) {
		cls = sfc.attr("classes");
		tp = make_type(cls[0], "", false, NULL, 0);
	}
	switch (tp) {
		case SF_Unknown: { // further check:
			throw std::range_error("impossible classs in get_dim()"); // #nocov
		} break;
		case SF_Point: { // numeric:
			Rcpp::NumericVector v = sfc[0];
			cls = v.attr("class");
		} break;
		case SF_LineString:  // matrix:
		case SF_MultiPoint:
		case SF_CircularString:
		case SF_Curve: {
			Rcpp::NumericMatrix m = sfc[0];
			cls = m.attr("class");
		} break;
		case SF_Polygon: // list:
		case SF_MultiLineString:
		case SF_MultiPolygon:
		case SF_GeometryCollection:
		case SF_CompoundCurve:
		case SF_CurvePolygon:
		case SF_MultiCurve:
		case SF_MultiSurface:
		case SF_Surface:
		case SF_PolyhedralSurface:
		case SF_TIN:
		case SF_Triangle: {
			Rcpp::List l = sfc[0];
			cls = l.attr("class");
		} break;
	}
	return cls;
}

std::vector<OGRGeometry *> ogr_from_sfc(Rcpp::List sfc, OGRSpatialReference **sref) {
	double precision = sfc.attr("precision");
	Rcpp::List wkblst = CPL_write_wkb(sfc, false, native_endian(), get_dim(sfc), precision);
	std::vector<OGRGeometry *> g(sfc.length());
	OGRGeometryFactory f;
	OGRSpatialReference *local_srs = NULL;
	Rcpp::List crs = sfc.attr("crs");
	Rcpp::String p4s = crs["proj4string"];
	if (p4s != NA_STRING) {
		Rcpp::CharacterVector cv = crs["proj4string"];
		local_srs = new OGRSpatialReference;
		handle_error(local_srs->importFromProj4(cv[0]));
	}
	for (int i = 0; i < wkblst.length(); i++) {
		Rcpp::RawVector r = wkblst[i];
		handle_error(f.createFromWkb(&(r[0]), local_srs, &(g[i]), r.length(), wkbVariantIso));
	}
	if (sref != NULL)
		*sref = local_srs; // return and release later, or
	else if (local_srs != NULL)
		local_srs->Release(); // release now
	return g;
}

std::vector<char *> create_options(Rcpp::CharacterVector lco, bool quiet = false) {
	if (lco.size() == 0)
		quiet = true; // nothing to report
	if (! quiet)
		Rcpp::Rcout << "options:        ";
	std::vector<char *> ret(lco.size() + 1);
	for (int i = 0; i < lco.size(); i++) {
		ret[i] = (char *) (lco[i]);
		if (! quiet)
			Rcpp::Rcout << ret[i] << " ";
	}
	ret[lco.size()] = NULL;
	if (! quiet)
		Rcpp::Rcout << std::endl;
	return ret;
}

Rcpp::CharacterVector p4s_from_spatial_reference(OGRSpatialReference *ref) {
	Rcpp::CharacterVector proj4string(1);
	char *cp;
	CPLPushErrorHandler(CPLQuietErrorHandler); // don't break on EPSG's without proj4string
	(void) ref->exportToProj4(&cp);

	// eliminate trailing white space, the C-way:
	if (strlen(cp) > 0)
		for (char *cpws = cp + strlen(cp) - 1; cpws != cp && *cpws == ' '; cpws--)
			*cpws = '\0';

	proj4string[0] = cp;
	CPLFree(cp);
	CPLPopErrorHandler();
	return proj4string;
}

Rcpp::List get_crs(OGRSpatialReference *ref) {
	Rcpp::List crs(2);
	if (ref == NULL) {
		crs(0) = NA_INTEGER;
		crs(1) = Rcpp::CharacterVector::create(NA_STRING);
	} else {
		const char *cp;
		if (ref->AutoIdentifyEPSG() == OGRERR_NONE &&
				(cp = ref->GetAuthorityCode(NULL)) != NULL)
			crs(0) = atoi(cp);
		else
			crs(0) = NA_INTEGER;
		crs(1) = p4s_from_spatial_reference(ref);
	}
	Rcpp::CharacterVector nms(2);
	nms(0) = "epsg";
	nms(1) = "proj4string";
	crs.attr("names") = nms;
	crs.attr("class") = "crs";
	return crs;
}

Rcpp::List sfc_from_ogr(std::vector<OGRGeometry *> g, bool destroy = false) {
	Rcpp::List lst(g.size());
	Rcpp::List crs = get_crs(g.size() && g[0] != NULL ? g[0]->getSpatialReference() : NULL);
	for (size_t i = 0; i < g.size(); i++) {
		if (g[i] == NULL)
			throw std::range_error("NULL error in sfc_from_ogr"); // #nocov
		Rcpp::RawVector raw(g[i]->WkbSize());
		handle_error(g[i]->exportToWkb(wkbNDR, &(raw[0]), wkbVariantIso));
		lst[i] = raw;
		if (destroy)
			delete g[i];
	}
	Rcpp::List ret = CPL_read_wkb(lst, false, native_endian());
	ret.attr("crs") = crs;
	ret.attr("class") = "sfc";
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_from_epsg(int epsg) {
	OGRSpatialReference ref;
	if (ref.importFromEPSG(epsg) == OGRERR_NONE)
		return get_crs(&ref);
	else
		return get_crs(NULL);
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_from_wkt(Rcpp::CharacterVector wkt) {
	char *cp = wkt[0];
	OGRSpatialReference ref;
	handle_error(ref.importFromWkt(&cp));
	return get_crs(&ref);
}

// [[Rcpp::export]]
Rcpp::List CPL_roundtrip(Rcpp::List sfc) { // for debug purposes
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	for (size_t i; i < g.size(); i++) {
		char *out;
		g[i]->exportToWkt(&out);
		Rcpp::Rcout << out << std::endl;
		CPLFree(out);
	}
	return sfc_from_ogr(g, true); // destroys g;
}

// [[Rcpp::export]]
Rcpp::List CPL_transform(Rcpp::List sfc, Rcpp::CharacterVector proj4, Rcpp::IntegerVector epsg) {
	// a hard (but quite sane) assumption here is that in case epsg[0] != NA_INTEGER,
	// proj4 and epsg correspond, and point to the same SRS.

	// import proj4string:
	OGRSpatialReference *dest = new OGRSpatialReference;
	handle_error(dest->importFromProj4((const char *) (proj4[0])));

	// transform geometries:
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	OGRCoordinateTransformation *ct = 
		OGRCreateCoordinateTransformation(g[0]->getSpatialReference(), dest);
	if (ct == NULL)
		throw std::range_error("OGRCreateCoordinateTransformation() returned NULL: PROJ.4 available?");
	for (size_t i = 0; i < g.size(); i++)
		handle_error(g[i]->transform(ct));

	Rcpp::List ret = sfc_from_ogr(g, true); // destroys g;
	if (epsg[0] != NA_INTEGER) {
		Rcpp::List crs = ret.attr("crs");
		crs(0) = epsg;
		ret.attr("crs") = crs;
	}
	ct->DestroyCT(ct);
	dest->Release();
	return ret; 
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_from_proj4string(Rcpp::CharacterVector p4s) {
	OGRSpatialReference ref;
	if (ref.importFromProj4(p4s[0]) == OGRERR_NONE)
		return get_crs(&ref);
	else {
		const char *cp = p4s[0];
		Rf_warning("Cannot import crs from PROJ.4 string `%s', missing crs returned\n", cp);
		return get_crs(NULL);
	}
}

// [[Rcpp::export]]
Rcpp::List CPL_get_rgdal_drivers(int dummy) {

	int ndr = GetGDALDriverManager()->GetDriverCount();
	Rcpp::CharacterVector name(ndr);
	Rcpp::CharacterVector long_name(ndr);
	Rcpp::LogicalVector create(ndr);
	Rcpp::LogicalVector copy(ndr);
	Rcpp::LogicalVector vattr(ndr);
	Rcpp::LogicalVector rattr(ndr);
	for (int i = 0; i < ndr; i++) {
		GDALDriver *pDriver = GetGDALDriverManager()->GetDriver(i);
		name(i) = GDALGetDriverShortName( pDriver );
		long_name(i) = GDALGetDriverLongName( pDriver );
		create(i) = (pDriver->GetMetadataItem(GDAL_DCAP_CREATE) != NULL);
		copy(i) =   (pDriver->GetMetadataItem(GDAL_DCAP_CREATECOPY) != NULL);
		vattr(i) =  (pDriver->GetMetadataItem(GDAL_DCAP_VECTOR) != NULL);
		rattr(i) =  (pDriver->GetMetadataItem(GDAL_DCAP_RASTER) != NULL);
	}
	Rcpp::List ret(6);
	ret(0) = name;
	ret(1) = long_name;
	ret(2) = create;
	ret(3) = copy;
	ret(4) = rattr;
	ret(5) = vattr;
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_sfc_from_wkt(Rcpp::CharacterVector wkt) {
	std::vector<OGRGeometry *> g(wkt.size());
	OGRGeometryFactory f;
	for (int i = 0; i < wkt.size(); i++) {
		char *wkt_str = wkt(i);
		handle_error(f.createFromWkt(&wkt_str, NULL, &(g[i])));
	}
	return sfc_from_ogr(g, true);
}
