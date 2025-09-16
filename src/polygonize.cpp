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

// # nocov start
// [[Rcpp::export(rng=false)]]
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
		Rcpp::Rcout << "No bands in raster file." << std::endl; // #nocov

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
			Rcpp::Rcout << "No bands in mask file." << std::endl; // #nocov
	}


	// output: vector layer
	GDALDriver *poDriver = GetGDALDriverManager()->GetDriverByName(vector_driver[0]);
	if (poDriver == NULL) {
		Rcpp::Rcout << "driver `" << vector_driver[0] << "' not available." << std::endl; // #nocov
		Rcpp::stop("Driver not available.\n"); // #nocov
	}
	GDALDataset *poDS; 
	if ((poDS = poDriver->Create(vector_dsn[0], 0, 0, 0, GDT_Unknown, NULL)) == NULL) {
		Rcpp::Rcout << "Creating dataset " <<  vector_dsn[0] << " failed." << std::endl; // #nocov
		Rcpp::stop("Creation failed.\n"); // #nocov
	}
	OGRSpatialReference *sr = NULL;
	if (wkt != NULL && *wkt != '\0') {
		sr = new OGRSpatialReference;
		sr = handle_axis_order(sr);
		char **ppt = (char **) &wkt;
#if GDAL_VERSION_NUM < 2030000
		sr->importFromWkt(ppt);
#else
		sr->importFromWkt( (const char**) ppt);
#endif
	}
	OGRLayer *poLayer = poDS->CreateLayer("raster", sr, wkbMultiPolygon, NULL);
	delete sr;

	if (use_integer) {
		// create field:
		OGRFieldDefn oField("Value", OFTInteger);
		if (poLayer->CreateField(&oField) != OGRERR_NONE)
    		Rcpp::stop("Creating attribute field failed.\n"); // #nocov
		if (GDALPolygonize((GDALRasterBandH) poBand, maskBand,
			(OGRLayerH) poLayer,
			iPixValField[0],
			NULL, // create_options(options, true),
			NULL, NULL) != OGRERR_NONE)
				Rcpp::Rcout << "GDALPolygonize returned an error" << std::endl; // #nocov
	} else {
		OGRFieldDefn oField("Value", OFTReal);
		if (poLayer->CreateField(&oField) != OGRERR_NONE)
    		Rcpp::stop("Creating attribute field failed.\n"); // #nocov
		OGRFieldDefn minField("Min", OFTReal);
		if (poLayer->CreateField(&minField) != OGRERR_NONE)
    		Rcpp::stop("Creating attribute field failed.\n"); // #nocov
		OGRFieldDefn maxField("Max", OFTReal);
		if (poLayer->CreateField(&maxField) != OGRERR_NONE)
    		Rcpp::stop("Creating attribute field failed.\n"); // #nocov

		if (!use_contours) {
			if (GDALFPolygonize((GDALRasterBandH) poBand, maskBand,
				(OGRLayerH) poLayer,
				iPixValField[0],
				create_options(options, true).data(),
				NULL, NULL) != OGRERR_NONE)
					Rcpp::Rcout << "GDALFPolygonize returned an error" << std::endl; // #nocov
		} else {
#if GDAL_VERSION_NUM >= 2040000
			if (GDALContourGenerateEx((GDALRasterBandH) poBand, (void *) poLayer,
                       	create_options(contour_options).data(), NULL, NULL) != OGRERR_NONE)
				Rcpp::stop("GDALContourGenerateEx returned an error");
#else
			Rcpp::stop("contour requires GDAL >= 2.4.0");
#endif
		}
	}

	Rcpp::NumericVector type(1);
	type[0] = 0;
	Rcpp::CharacterVector fid_column; // empty
	Rcpp::List lst = sf_from_ogrlayer(poLayer, false, true, type, fid_column, true, -1);
	GDALClose(poDataset); // raster
	GDALClose(poDS); // vector
	if (maskDataset != NULL)
		GDALClose(maskDataset); // mask
	return lst; 
} 
// # nocov end

// [[Rcpp::export(rng=false)]]
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

	// int bandlist = 1;
	std::vector<int> bandlist(poDataset->GetRasterCount());
	for (size_t i = 0; i < bandlist.size(); i++)
		bandlist[i] = (int) i+1; // 1-based

	CPLErr err = GDALRasterizeGeometries((GDALDatasetH) poDataset, // hDS,
		poDataset->GetRasterCount(), // int 	nBandCount,
		bandlist.data(), // int * 	panBandList,
		geoms.size(), // int 	nGeomCount,
		(OGRGeometryH *) geoms.data(), // OGRGeometryH * 	pahGeometries,
		NULL, // GDALTransformerFunc 	pfnTransformer,
		NULL, // void * 	pTransformArg,
		(double *) &(values[0]), // double * 	padfGeomBurnValue,
		options.size() ? create_options(options).data() : NULL, // char ** 	papszOptions,
		NULL, // GDALProgressFunc 	pfnProgress,
		NULL  //void * 	pProgressArg 
	);

	for (size_t i = 0; i < geoms.size(); i++)
		OGRGeometryFactory::destroyGeometry(geoms[i]);

	if (err != OGRERR_NONE)
		Rcpp::Rcout << "GDALRasterizeGeometries returned an error" << std::endl; // #nocov
	GDALClose(poDataset); // raster
	return Rcpp::List::create();
}
