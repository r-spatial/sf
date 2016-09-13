#include <gdal.h>
#include <gdal_alg.h>
#include <ogr_api.h>
#include <ogr_geometry.h>
#include <ogr_srs_api.h>
#include <ogr_spatialref.h>
#include <cpl_conv.h>
#include <cpl_string.h>

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
            break;
        case 1:
        case 2:
            Rf_warning("GDAL Message %d: %s\n", err_no, msg);
            break;
        case 3:
            Rf_warning("GDAL Error %d: %s\n", err_no, msg);
            break;
        case 4:
            Rf_warning("GDAL Error %d: %s\n", err_no, msg);
            #ifndef CONTINUE_ON_ERROR
            Rcpp::stop("Unrecoverable GDAL error\n");
            #endif
            break;        
        default:
            Rf_warning("Received invalid error class %d (errno %d: %s)\n", eErrClass, err_no, msg);
            break;
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
		Rcpp::Rcout << "Error code: " << err << std::endl;
		throw std::range_error("OGR error");
	}
}

std::vector<OGRGeometry *> geometries_from_sfc(Rcpp::List sfc, const char *proj4) {
	double precision = sfc.attr("precision");
	Rcpp::List wkblst = CPL_write_wkb(sfc, false, native_endian(), "XY", precision);
	std::vector<OGRGeometry *> g(sfc.length());
	OGRGeometryFactory f;
	OGRSpatialReference *ref = new OGRSpatialReference;
	handle_error(ref->importFromProj4(proj4));
	for (int i = 0; i < wkblst.length(); i++) {
		Rcpp::RawVector r = wkblst[i];
		handle_error(f.createFromWkb(&(r[0]), ref, &(g[i]), -1, wkbVariantIso));
	}
	ref->Release();
	return(g);
}

Rcpp::List sfc_from_geometries(std::vector<OGRGeometry *> g, bool destroy = false) {
	Rcpp::List lst(g.size());
	OGRGeometryFactory f;
	for (size_t i = 0; i < g.size(); i++) {
		Rcpp::RawVector raw(g[i]->WkbSize());
		handle_error(g[i]->exportToWkb(wkbNDR, &(raw[0]), wkbVariantIso));
		lst[i] = raw;
		if (destroy)
			f.destroyGeometry(g[i]);
	}
	return(CPL_read_wkb(lst, false, native_endian()));
}

Rcpp::CharacterVector p4s_from_spatial_reference(OGRSpatialReference *ref) {
	Rcpp::CharacterVector proj4string(1);
	char *cp;
	CPLPushErrorHandler(CPLQuietErrorHandler); // don't break on EPSG's without proj4string
	(void) ref->exportToProj4(&cp);
	proj4string[0] = cp;
	CPLFree(cp);
	CPLPopErrorHandler();
	return(proj4string);
}

// [[Rcpp::export]]
Rcpp::List CPL_transform(Rcpp::List sfc, Rcpp::CharacterVector proj4) {

	// import proj4string:
	OGRSpatialReference *dest = new OGRSpatialReference;
	handle_error(dest->importFromProj4((const char *) (proj4[0])));

	// get the proj4string as OGR thinks it is (e.g., resolve epsg)
	Rcpp::CharacterVector proj4string = p4s_from_spatial_reference(dest);

	// transform geometries:
	std::vector<OGRGeometry *> g = geometries_from_sfc(sfc, sfc.attr("proj4string"));
	OGRCoordinateTransformation *ct = 
		OGRCreateCoordinateTransformation(g[0]->getSpatialReference(), dest);
	for (size_t i = 0; i < g.size(); i++)
		handle_error(g[i]->transform(ct));

	ct->DestroyCT(ct);
	dest->Release();
	Rcpp::List ret = sfc_from_geometries(g, true); // destroys g;
	ret.attr("proj4string") = proj4string;
	return(ret);
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_proj4string_from_epsg(int epsg) {
	OGRSpatialReference ref;
	ref.importFromEPSG(epsg);
	return(p4s_from_spatial_reference(&ref));
}
