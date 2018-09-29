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

#include <ogrsf_frmts.h>

#include <Rcpp.h>

#include "gdal.h"
#include "gdal_read.h"
#include "gdal_sf_pkg.h"

// [[Rcpp::export]]
Rcpp::List CPL_polygonize(Rcpp::CharacterVector raster, Rcpp::CharacterVector mask_name,
		Rcpp::CharacterVector raster_driver, 
		Rcpp::CharacterVector vector_driver, Rcpp::CharacterVector vector_dsn,
		Rcpp::CharacterVector options, Rcpp::IntegerVector iPixValField,
		Rcpp::CharacterVector contour_options, bool use_contours = false,
		bool use_integer = true) {

    GDALDataset  *poDataset = (GDALDataset *) GDALOpenEx(raster[0], GA_ReadOnly,
		raster_driver.size() ? create_options(raster_driver).data() : NULL,
		// options.size() ? create_options(options).data() : NULL,
		NULL, NULL);
    if (poDataset == NULL) {
		Rcpp::Rcout << "trying to read file: " << raster[0] << std::endl; // #nocov
        Rcpp::stop("file not found"); // #nocov
	}

	const char *wkt = poDataset->GetProjectionRef();
	GDALRasterBand *poBand = NULL;
	if (poDataset->GetRasterCount() > 0)
		poBand = poDataset->GetRasterBand( 1 );
	else
		Rcpp::Rcout << "No bands in raster file." << std::endl;

	// mask:
    GDALDataset  *maskDataset = NULL;
	GDALRasterBand *maskBand = NULL;
	if (mask_name.size()) {
		maskDataset = (GDALDataset *) GDALOpenEx(mask_name[0], GA_ReadOnly,
			raster_driver.size() ? create_options(raster_driver).data() : NULL,
			// options.size() ? create_options(options).data() : NULL,
			NULL, NULL);
    	if (maskDataset == NULL) {
			Rcpp::Rcout << "trying to read file: " << mask_name[0] << std::endl; // #nocov
        	Rcpp::stop("file not found"); // #nocov
		}
	
		if (maskDataset->GetRasterCount() > 0)
			maskBand = maskDataset->GetRasterBand( 1 );
		else
			Rcpp::Rcout << "No bands in mask file." << std::endl;
	}


	// output: vector layer
	GDALDriver *poDriver = GetGDALDriverManager()->GetDriverByName(vector_driver[0]);
	if (poDriver == NULL) {
		Rcpp::Rcout << "driver `" << vector_driver[0] << "' not available." << std::endl;
		Rcpp::stop("Driver not available.\n");
	}
	GDALDataset *poDS; 
	if ((poDS = poDriver->Create(vector_dsn[0], 0, 0, 0, GDT_Unknown, NULL)) == NULL) {
		Rcpp::Rcout << "Creating dataset " <<  vector_dsn[0] << " failed." << std::endl;
		Rcpp::stop("Creation failed.\n");
	}
	OGRSpatialReference *sr = new OGRSpatialReference;
	char **ppt = (char **) &wkt;
#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
	sr->importFromWkt(ppt);
#else
	sr->importFromWkt( (const char**) ppt);
#endif
	OGRLayer *poLayer = poDS->CreateLayer("raster", sr, wkbMultiPolygon, NULL);
	delete sr;

	// create field:
	OGRFieldDefn oField("Name", OFTInteger);
	if (poLayer->CreateField(&oField) != OGRERR_NONE)
    	Rcpp::stop("Creating attribute field failed.\n");

	CPLErr err;
	if (use_contours) {
#if ((GDAL_VERSION_MAJOR > 3) || (GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR >= 4)) 
		OGRFieldDefn idField("ID", OFTInteger);
		if (poLayer->CreateField(&idField) != OGRERR_NONE)
    		Rcpp::stop("Creating attribute field failed.\n");
		OGRFieldDefn elevField("ELEV", OFTReal);
		if (poLayer->CreateField(&elevField) != OGRERR_NONE)
    		Rcpp::stop("Creating attribute field failed.\n");
    	Rcpp::stop("Creating attribute field failed.\n");
		err = GDALContourGenerateEx((GDALRasterBandH) poBand, (void *) poLayer,
                       create_options(contour_options).data(), NULL, NULL);
		if (err != OGRERR_NONE)
			Rcpp::Rcout << "GDALContourGenerateEx returned an error" << std::endl;
#else
		Rcpp::stop("contour only available in GDAL >= 2.4.0");
#endif
	} else if (use_integer)
		err = GDALPolygonize((GDALRasterBandH) poBand, maskBand,
			(OGRLayerH) poLayer,
			iPixValField[0],
			NULL, // create_options(options, true),
			NULL, NULL);
	else
		err = GDALFPolygonize((GDALRasterBandH) poBand, maskBand,
			(OGRLayerH) poLayer,
			iPixValField[0],
			NULL, // create_options(options, true),
			NULL, NULL);

	if (err != OGRERR_NONE)
		Rcpp::Rcout << "GDAL[F}Polygonize returned an error" << std::endl;

	Rcpp::NumericVector type(1);
	type[0] = 0;
	Rcpp::List lst = sf_from_ogrlayer(poLayer, false, true, type, true);
	GDALClose(poDataset); // raster
	GDALClose(poDS); // vector
	if (maskDataset != NULL)
		GDALClose(maskDataset); // mask
	return lst;
}

// [[Rcpp::export]]
Rcpp::List CPL_rasterize(Rcpp::CharacterVector raster, Rcpp::CharacterVector raster_driver,
		Rcpp::List sfc, Rcpp::NumericVector values,
		Rcpp::CharacterVector options,
		Rcpp::NumericVector NA_value) {

    GDALDataset  *poDataset = (GDALDataset *) GDALOpenEx(raster[0], GDAL_OF_UPDATE,
		raster_driver.size() ? create_options(raster_driver).data() : NULL,
		// options.size() ? create_options(options).data() : NULL,
		NULL, NULL);
    if (poDataset == NULL) {
		Rcpp::Rcout << "trying to read file: " << raster[0] << std::endl; // #nocov
        Rcpp::stop("file not found"); // #nocov
	}
	
	std::vector<OGRGeometry *> geoms = ogr_from_sfc(sfc, NULL);

	int bandlist = 1;
	CPLErr err = GDALRasterizeGeometries((GDALDatasetH) poDataset, // hDS,
		1, // int 	nBandCount,
		&bandlist, // int * 	panBandList,
		geoms.size(), // int 	nGeomCount,
		(OGRGeometryH *) geoms.data(), // OGRGeometryH * 	pahGeometries,
		NULL, // GDALTransformerFunc 	pfnTransformer,
		NULL, // void * 	pTransformArg,
		(double *) &(values[0]), // double * 	padfGeomBurnValue,
		options.size() ? create_options(options).data() : NULL, // char ** 	papszOptions,
		NULL, // GDALProgressFunc 	pfnProgress,
		NULL  //void * 	pProgressArg 
	);
	if (err != OGRERR_NONE)
		Rcpp::Rcout << "GDALRasterizeGeometries returned an error" << std::endl;
	GDALClose(poDataset); // raster
	return Rcpp::List::create();
}
