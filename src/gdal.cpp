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
    // GDALAllRegister();
    OGRRegisterAll();
}

// [[Rcpp::export]]
void RGDAL_CleanupAll()
{
    OGRCleanupAll();
    OSRCleanup();
}

// [[Rcpp::export]]
const char* versionInfo(const char* what = "RELEASE_NAME")
{
  return GDALVersionInfo(what);
}

void OGR_Error(OGRErr err) {
	if (err != 0) {
		Rcpp::Rcout << "Error code: " << err << std::endl;
		throw std::range_error("OGR error");
	}
}

std::vector<OGRGeometry *> OGRGeometryFromSfc(Rcpp::List sfc, const char *proj4) {
	double precision = sfc.attr("precision");
	Rcpp::List wkblst = WriteWKB(sfc, false, 1, "XY", false, precision);
	std::vector<OGRGeometry *> g(sfc.length());
	OGRGeometryFactory f;
	for (int i = 0; i < wkblst.length(); i++) {
		Rcpp::RawVector r = wkblst[i];
		OGRSpatialReference *ref = new OGRSpatialReference;
		OGR_Error(ref->importFromProj4(strdup(proj4))); // TODO: strdup every time?
		OGR_Error(f.createFromWkb(&(r[0]), ref, &(g[i]), -1, wkbVariantIso));
	}
	return(g);
}

Rcpp::List SfcFromOGRGeometries(std::vector<OGRGeometry *> g) {
	Rcpp::List lst(g.size());
	OGRGeometryFactory f;
	for (int i = 0; i < g.size(); i++) {
		Rcpp::RawVector raw(g[i]->WkbSize());
		OGR_Error(g[i]->exportToWkb(wkbNDR, &(raw[0]), wkbVariantIso));
		lst[i] = raw;
		f.destroyGeometry(g[i]);
	}
	Rcpp::List ret = ReadWKB(lst, false, 1, false);
	return(ret);
}

// [[Rcpp::export]]
Rcpp::List OGR_Transform(Rcpp::List sfc, Rcpp::CharacterVector proj4) {
	std::vector<OGRGeometry *> g = OGRGeometryFromSfc(sfc, sfc.attr("proj4string"));
	OGRSpatialReference *dest = new OGRSpatialReference;
	const char *prj = proj4[0];
	OGR_Error(dest->importFromProj4(strdup(prj)));
	// return value for proj4string: get substitutions from GDAL:
	Rcpp::CharacterVector p4atr(1);
	char *cp; 
	dest->exportToProj4(&cp);
	p4atr[0] = cp;
	OGRCoordinateTransformation *ct = 
		OGRCreateCoordinateTransformation(g[0]->getSpatialReference(), dest);
	for (int i = 0; i < g.size(); i++)
		OGR_Error(g[i]->transform(ct));
	Rcpp::List ret = SfcFromOGRGeometries(g); // destroys g;
	ret.attr("proj4string") = p4atr;
	delete dest;
	ct->DestroyCT(ct);
	return(ret);
}
