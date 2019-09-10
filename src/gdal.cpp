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

#include "gdal.h"
#include "wkb.h"
#include "gdal_sf_pkg.h"

#if GDAL_VERSION_MAJOR == 2
# if GDAL_VERSION_MINOR >= 5
#  define HAVE250
# endif
#else
# if GDAL_VERSION_MAJOR > 2
#  define HAVE250
# endif
#endif

//
// Returns errors to R
// Note only case 4 actually returns immediately
// Lower error codes are recoverable
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
            Rcpp::stop("Unrecoverable GDAL error\n"); // #nocov
            break;        
        default:
            Rf_warning("Received invalid error class %d (errno %d: %s)\n", eErrClass, err_no, msg); // #nocov
            break; // #nocov
    }
    return;
}

// #nocov start
static void __err_silent(CPLErr eErrClass, int err_no, const char *msg)
{
    return;
}
// #nocov end

void set_error_handler(void) 
{
    CPLSetErrorHandler((CPLErrorHandler)__err_handler);
}

void unset_error_handler(void) 
{
    CPLSetErrorHandler((CPLErrorHandler)__err_silent);
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
	if (err != OGRERR_NONE) {
		switch (err) {
			case OGRERR_NOT_ENOUGH_DATA:
				Rcpp::Rcout << "OGR: Not enough data " << std::endl; // #nocov
				break; // #nocov
			case OGRERR_UNSUPPORTED_GEOMETRY_TYPE:
				Rcpp::Rcout << "OGR: Unsupported geometry type" << std::endl;
				break;
			case OGRERR_CORRUPT_DATA:
				Rcpp::Rcout << "OGR: Corrupt data" << std::endl;     // #nocov
				break; // #nocov
			case OGRERR_FAILURE:
				Rcpp::Rcout << "OGR: index invalid?" << std::endl;    // #nocov
				break; // #nocov
			default:
				Rcpp::Rcout << "Error code: " << err << std::endl;    // #nocov
		}
		Rcpp::stop("OGR error");
	}
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_parameters(std::string p4s) {
	Rcpp::List out(7);
	OGRErr Err;
	OGRSpatialReference *srs = new OGRSpatialReference;
	srs = handle_axis_order(srs);
	handle_error(srs->importFromProj4(p4s.c_str()));
	out(0) = Rcpp::NumericVector::create(srs->GetSemiMajor());
	out(1) = Rcpp::NumericVector::create(srs->GetSemiMinor());
	Rcpp::NumericVector InvFlattening(1);
	srs->GetInvFlattening(&Err);
	if (Err == OGRERR_FAILURE)
		InvFlattening(0) = NA_REAL; // #nocov
	else
		InvFlattening(0) = srs->GetInvFlattening(NULL); // for +ellps=sphere, still zero :-(
	out(2) = InvFlattening;
	out(3) = Rcpp::CharacterVector::create(srs->GetAttrValue("UNIT", 0));
	out(4) = Rcpp::LogicalVector::create(srs->IsVertical());
	char *cp;
	srs->exportToPrettyWkt(&cp);
	out(5) = Rcpp::CharacterVector::create(cp);
	CPLFree(cp);
	srs->exportToWkt(&cp);
	out(6) = Rcpp::CharacterVector::create(cp);
	CPLFree(cp);
	delete srs;
	return out;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_crs_equivalent(std::string crs1, std::string crs2) {
	Rcpp::LogicalVector out(1);
	OGRSpatialReference *srs1 = new OGRSpatialReference;
	srs1 = handle_axis_order(srs1);
	handle_error(srs1->importFromProj4(crs1.c_str()));
	OGRSpatialReference *srs2 = new OGRSpatialReference;
	srs2 = handle_axis_order(srs2);
	handle_error(srs2->importFromProj4(crs2.c_str()));
	out(0) = (bool) srs1->IsSame(srs2);
	delete srs1;
	delete srs2;
	return out;
}

std::vector<OGRGeometry *> ogr_from_sfc(Rcpp::List sfc, OGRSpatialReference **sref) {
	Rcpp::List wkblst = CPL_write_wkb(sfc, false);
	std::vector<OGRGeometry *> g(sfc.length());
	OGRSpatialReference *local_srs = NULL;
	Rcpp::List crs = sfc.attr("crs");
	Rcpp::IntegerVector epsg(1);
	epsg[0] = crs["epsg"];
	Rcpp::String p4s = crs["proj4string"];
	if (p4s != NA_STRING) {
		Rcpp::CharacterVector cv = crs["proj4string"];
		local_srs = new OGRSpatialReference;
		local_srs = handle_axis_order(local_srs);
		OGRErr err = local_srs->importFromProj4(cv[0]);
		if (err != OGRERR_NONE) {
			local_srs->Release(); // #nocov
			handle_error(err);    // #nocov
		}
	}
	for (int i = 0; i < wkblst.length(); i++) {
		Rcpp::RawVector r = wkblst[i];
		OGRErr err = OGRGeometryFactory::createFromWkb(&(r[0]), local_srs, &(g[i]), 
			r.length(), wkbVariantIso);
		if (err != OGRERR_NONE) {
			if (g[i] != NULL) // release: #nocov
				OGRGeometryFactory::destroyGeometry(g[i]); // #nocov
			if (local_srs != NULL)      // #nocov start
				local_srs->Release();
			handle_error(err);          // #nocov end
		}
	}
	if (sref != NULL)
		*sref = local_srs; // return and release later, or
	else if (local_srs != NULL)
		local_srs->Release(); // release now
	return g;
}

std::vector<char *> create_options(Rcpp::CharacterVector lco, bool quiet) {
	if (lco.size() == 0)
		quiet = true; // nothing to report
	if (! quiet)
		Rcpp::Rcout <<  "options:        "; // #nocov
	std::vector<char *> ret(lco.size() + 1);
	for (int i = 0; i < lco.size(); i++) {
		ret[i] = (char *) (lco[i]);
		if (! quiet)
			Rcpp::Rcout << ret[i] << " "; // #nocov
	}
	ret[lco.size()] = NULL;
	if (! quiet)
		Rcpp::Rcout << std::endl;         // #nocov
	return ret;
}

// convert NULL-terminated array of strings to Rcpp::CharacterVector
Rcpp::CharacterVector charpp2CV(char **cp) {
	int n = 0;
	while (cp && cp[n] != NULL)
		n++; // count
	Rcpp::CharacterVector ret(n);
	for (int i = 0; i < n; i++)
		ret(i) = cp[i];
	return ret;
}

Rcpp::CharacterVector p4s_from_spatial_reference(OGRSpatialReference *ref) {
	Rcpp::CharacterVector proj4string(1);
	char *cp;
	CPLPushErrorHandler(CPLQuietErrorHandler); // don't break on EPSG's without proj4string
	ref->morphFromESRI();
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
	OGRwkbGeometryType type = wkbGeometryCollection;
	Rcpp::List lst(g.size());
	Rcpp::List crs = get_crs(g.size() && g[0] != NULL ? g[0]->getSpatialReference() : NULL);
	for (size_t i = 0; i < g.size(); i++) {
		if (g[i] == NULL)
			g[i] = OGRGeometryFactory::createGeometry(type); // #nocov
		else
			type = g[i]->getGeometryType();
		Rcpp::RawVector raw(g[i]->WkbSize());
		handle_error(g[i]->exportToWkb(wkbNDR, &(raw[0]), wkbVariantIso));
		lst[i] = raw;
		if (destroy)
			OGRGeometryFactory::destroyGeometry(g[i]);
	}
	Rcpp::List ret = CPL_read_wkb(lst, false, false);
	ret.attr("crs") = crs;
	ret.attr("class") = "sfc";
	return ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_from_epsg(int epsg) {
	OGRSpatialReference ref;
	handle_axis_order(&ref);
	if (ref.importFromEPSG(epsg) == OGRERR_NONE)
		return get_crs(&ref);
	else
		return get_crs(NULL);
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_from_wkt(Rcpp::CharacterVector wkt) {
	char *cp = wkt[0];
	OGRSpatialReference ref;
	handle_axis_order(&ref);
#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
	handle_error(ref.importFromWkt(&cp));
#else
	handle_error(ref.importFromWkt( (const char*) cp));
#endif
	return get_crs(&ref);
}

// [[Rcpp::export]]
Rcpp::List CPL_roundtrip(Rcpp::List sfc) { // for debug purposes
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	for (size_t i = 0; i < g.size(); i++) {
		char *out;
		g[i]->exportToWkt(&out);
		Rcpp::Rcout << out << std::endl;
		CPLFree(out);
	}
	return sfc_from_ogr(g, true); // destroys g;
}

// [[Rcpp::export]]
Rcpp::List CPL_circularstring_to_linestring(Rcpp::List sfc) { // need to pass more parameters?
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRCircularString *cs = (OGRCircularString *) g[i];
		out[i] = cs->CurveToLine();
		OGRGeometryFactory::destroyGeometry(g[i]);
	}
	return sfc_from_ogr(out, true); // destroys out;
}

// [[Rcpp::export]]
Rcpp::List CPL_multisurface_to_multipolygon(Rcpp::List sfc) { // need to pass more parameters?
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRMultiSurface *cs = (OGRMultiSurface *) g[i];
		if (cs->hasCurveGeometry(true)) {
			out[i] = cs->getLinearGeometry();
			OGRGeometryFactory::destroyGeometry(g[i]);
		} else
			out[i] = cs->CastToMultiPolygon(cs); // consumes cs #nocov
		if (out[i] == NULL)
			Rcpp::stop("CPL_multisurface_to_multipolygon: NULL returned - non-polygonal surface?"); // #nocov
	}
	return sfc_from_ogr(out, true); // destroys out;
}

// [[Rcpp::export]]
Rcpp::List CPL_compoundcurve_to_linear(Rcpp::List sfc) { // need to pass more parameters?
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRCompoundCurve *cs = (OGRCompoundCurve *) g[i];
		out[i] = cs->getLinearGeometry();
		OGRGeometryFactory::destroyGeometry(g[i]);
	}
	return sfc_from_ogr(out, true); // destroys out;
}

// [[Rcpp::export]]
Rcpp::List CPL_curve_to_linestring(Rcpp::List sfc) { // need to pass more parameters? #nocov start
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> out(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		OGRCurve *cs = (OGRCurve *) g[i];
		out[i] = cs->CastToLineString(cs);
	}
	return sfc_from_ogr(out, true); // destroys out;
} // #nocov end

// [[Rcpp::export]]
Rcpp::List CPL_transform(Rcpp::List sfc, Rcpp::CharacterVector proj4) {

	// import proj4string:
	OGRSpatialReference *dest = new OGRSpatialReference;
	dest = handle_axis_order(dest);
	handle_error(dest->importFromProj4((const char *) (proj4[0])));

	// transform geometries:
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	if (g.size() == 0) {
		dest->Release();
		return sfc_from_ogr(g, true); // destroys g
	}
	OGRCoordinateTransformation *ct = 
		OGRCreateCoordinateTransformation(g[0]->getSpatialReference(), dest);
	if (ct == NULL) {
		dest->Release(); // #nocov
		Rcpp::stop("OGRCreateCoordinateTransformation() returned NULL: PROJ available?"); // #nocov
	}
	for (size_t i = 0; i < g.size(); i++) {
		CPLPushErrorHandler(CPLQuietErrorHandler);
		OGRErr err = 0;
		if (! g[i]->IsEmpty())
			err = g[i]->transform(ct);
		CPLPopErrorHandler();
		if (err == 1 || err == 6) {
			OGRwkbGeometryType geomType = g[i]->getGeometryType();
			OGRGeometryFactory f;
			f.destroyGeometry(g[i]);
			g[i] = f.createGeometry(geomType);
		} else
			handle_error(err);
	}

	Rcpp::List ret = sfc_from_ogr(g, true); // destroys g;
	ct->DestroyCT(ct);
	dest->Release();
	return ret; 
}

// [[Rcpp::export]]
Rcpp::List CPL_wrap_dateline(Rcpp::List sfc, Rcpp::CharacterVector opt, bool quiet = true) {

	std::vector <char *> options = create_options(opt, quiet);
	std::vector<OGRGeometry *> g = ogr_from_sfc(sfc, NULL);
	std::vector<OGRGeometry *> ret(g.size());
	for (size_t i = 0; i < g.size(); i++) {
		ret[i] = OGRGeometryFactory::transformWithOptions(g[i], NULL, options.data());
		OGRGeometryFactory::destroyGeometry(g[i]);
	}
	return sfc_from_ogr(ret, true); // destroys ret;
}

// [[Rcpp::export]]
Rcpp::List CPL_crs_from_proj4string(Rcpp::CharacterVector p4s) {
	OGRSpatialReference ref;
	handle_axis_order(&ref);
	if (ref.importFromProj4(p4s[0]) == OGRERR_NONE)
		return get_crs(&ref);
	else {
		const char *cp = p4s[0];
		Rf_warning("GDAL cannot import PROJ.4 string `%s': returning missing CRS\n", cp);
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
	Rcpp::LogicalVector vsi_attr(ndr);
	for (int i = 0; i < ndr; i++) {
		GDALDriver *pDriver = GetGDALDriverManager()->GetDriver(i);
		name(i) = GDALGetDriverShortName( pDriver );
		long_name(i) = GDALGetDriverLongName( pDriver );
		create(i) = (pDriver->GetMetadataItem(GDAL_DCAP_CREATE) != NULL);
		copy(i) =   (pDriver->GetMetadataItem(GDAL_DCAP_CREATECOPY) != NULL);
		vattr(i) =  (pDriver->GetMetadataItem(GDAL_DCAP_VECTOR) != NULL);
		rattr(i) =  (pDriver->GetMetadataItem(GDAL_DCAP_RASTER) != NULL);
		vsi_attr(i) =  (pDriver->GetMetadataItem(GDAL_DCAP_VIRTUALIO) != NULL);
	}
	return Rcpp::DataFrame::create(
		Rcpp::Named("name") = name,
		Rcpp::Named("long_name") = long_name,
		Rcpp::Named("write") = create,
		Rcpp::Named("copy") = copy,
		Rcpp::Named("is_raster") = rattr,
		Rcpp::Named("is_vector") = vattr,
		Rcpp::Named("vsi") = vsi_attr);
}

// [[Rcpp::export]]
Rcpp::List CPL_sfc_from_wkt(Rcpp::CharacterVector wkt) {
	std::vector<OGRGeometry *> g(wkt.size());
	OGRGeometryFactory f;
	for (int i = 0; i < wkt.size(); i++) {
		char *wkt_str = wkt(i);
#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
		handle_error(f.createFromWkt(&wkt_str, NULL, &(g[i])));
#else
		handle_error(f.createFromWkt( (const char*) wkt_str, NULL, &(g[i])));
#endif
	}
	return sfc_from_ogr(g, true);
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdal_with_geos() {
	bool withGEOS = OGRGeometryFactory::haveGEOS(); 
	return Rcpp::LogicalVector::create(withGEOS);
}

OGRSpatialReference *handle_axis_order(OGRSpatialReference *sr, bool traditional) {
#ifdef HAVE250
	if (sr != NULL) {
		if (traditional)
			sr->SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
		else
			sr->SetAxisMappingStrategy(OAMS_AUTHORITY_COMPLIANT);
	}
#endif
	return sr;
}
