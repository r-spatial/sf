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
void GDALInit()
{
    CPLSetErrorHandler((CPLErrorHandler)__err_handler);
    GDALAllRegister();
    OGRRegisterAll();
}

// [[Rcpp::export]]
void GDAL_CleanupAll()
{
    OGRCleanupAll();
    OSRCleanup();
}

// [[Rcpp::export]]
const char* versionInfo(const char* what = "RELEASE_NAME")
{
  return GDALVersionInfo(what);
}

void HandleError(OGRErr err) {
	if (err != 0) {
		Rcpp::Rcout << "Error code: " << err << std::endl;
		throw std::range_error("OGR error");
	}
}

std::vector<OGRGeometry *> OGRGeometryFromSfc(Rcpp::List sfc, const char *proj4) {
	double precision = sfc.attr("precision");
	Rcpp::List wkblst = WriteWKB(sfc, false, native_endian(), "XY", false, precision);
	std::vector<OGRGeometry *> g(sfc.length());
	OGRGeometryFactory f;
	OGRSpatialReference *ref = new OGRSpatialReference;
	HandleError(ref->importFromProj4(proj4));
	for (int i = 0; i < wkblst.length(); i++) {
		Rcpp::RawVector r = wkblst[i];
		HandleError(f.createFromWkb(&(r[0]), ref, &(g[i]), -1, wkbVariantIso));
	}
	ref->Release();
	return(g);
}

Rcpp::List SfcFromOGRGeometries(std::vector<OGRGeometry *> g, bool destroy = false) {
	Rcpp::List lst(g.size());
	OGRGeometryFactory f;
	for (int i = 0; i < g.size(); i++) {
		Rcpp::RawVector raw(g[i]->WkbSize());
		HandleError(g[i]->exportToWkb(wkbNDR, &(raw[0]), wkbVariantIso));
		lst[i] = raw;
		if (destroy)
			f.destroyGeometry(g[i]);
	}
	return(ReadWKB(lst, false, native_endian(), false));
}

Rcpp::CharacterVector proj4stringFromSpatialReference(OGRSpatialReference *ref, char **cp) {
	Rcpp::CharacterVector proj4string(1);
	ref->exportToProj4(cp);
	proj4string[0] = *cp;
	return(proj4string);
}

// [[Rcpp::export]]
Rcpp::List OGR_Transform(Rcpp::List sfc, Rcpp::CharacterVector proj4) {

	// import proj4string:
	OGRSpatialReference *dest = new OGRSpatialReference;
	HandleError(dest->importFromProj4((const char *) (proj4[0])));

	// get the proj4string as OGR thinks it is (e.g., resolve epsg)
	char *cp;
	Rcpp::CharacterVector proj4string = proj4stringFromSpatialReference(dest, &cp);

	// transform geometries:
	std::vector<OGRGeometry *> g = OGRGeometryFromSfc(sfc, sfc.attr("proj4string"));
	OGRCoordinateTransformation *ct = 
		OGRCreateCoordinateTransformation(g[0]->getSpatialReference(), dest);
	for (int i = 0; i < g.size(); i++)
		HandleError(g[i]->transform(ct));

	ct->DestroyCT(ct);
	dest->Release();
	Rcpp::List ret = SfcFromOGRGeometries(g, true); // destroys g;
	ret.attr("proj4string") = proj4string;
	CPLFree(cp); // valgrind said
	return(ret);
}
