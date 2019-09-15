#include "cpl_port.h"
#include "cpl_conv.h" // CPLFree()
#include "gdal_version.h"
#include "gdalwarper.h"

#include <ogr_srs_api.h>
#include <ogr_spatialref.h>
#include "gdal.h" // local

#if (!(GDAL_VERSION_MAJOR == 2 && GDAL_VERSION_MINOR < 1))
# include "gdal_utils.h" // requires >= 2.1

#include "Rcpp.h"

#define NO_GDAL_CPP_HEADERS
#include "gdal_sf_pkg.h"

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_gdalinfo(Rcpp::CharacterVector obj, Rcpp::CharacterVector options) {
	std::vector <char *> options_char = create_options(options, true);
	GDALInfoOptions* opt = GDALInfoOptionsNew(options_char.data(), NULL);
	GDALDatasetH ds = GDALOpen((const char *) obj[0], GA_ReadOnly);
	if (ds == NULL)
		return 1; // #nocov
	char *ret_val = GDALInfo(ds, opt);
	Rcpp::CharacterVector ret = ret_val; // copies
	CPLFree(ret_val);
	GDALInfoOptionsFree(opt);
	GDALClose(ds);
	return ret;
}

// #nocov start

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalwarp(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {

	int err = 0;

	std::vector<GDALDatasetH> src_pt(src.size());
	for (int i = 0; i < src.size(); i++)
		src_pt[i] = GDALOpen((const char *) src[i], GA_ReadOnly);

	std::vector <char *> options_char = create_options(options, true);
	GDALWarpAppOptions* opt = GDALWarpAppOptionsNew(options_char.data(), NULL);

	GDALDatasetH result = GDALWarp((const char *) dst[0], NULL, src.size(), src_pt.data(), opt, &err);
	GDALWarpAppOptionsFree(opt);
	for (int i = 0; i < src.size(); i++)
		if (src_pt[i] != NULL)
			GDALClose(src_pt[i]);
	if (result != NULL)
		GDALClose(result);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalrasterize(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, bool overwrite = false) {

	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALRasterizeOptions* opt =  GDALRasterizeOptionsNew(options_char.data(), NULL);

	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_VECTOR | GA_ReadOnly, NULL, NULL, NULL);
	if (src_pt == NULL)
		Rcpp::stop("source dataset not found");
	unset_error_handler();
	GDALDatasetH dst_pt = NULL;
	if (! overwrite)
		dst_pt = GDALOpen((const char *) dst[0], GA_Update);
	set_error_handler();
	GDALDatasetH result = NULL;
	if (dst_pt == NULL)
		result = GDALRasterize((const char *) dst[0], NULL, src_pt, opt, &err);
	else
		result = GDALRasterize(NULL, dst_pt, src_pt, opt, &err);
	GDALRasterizeOptionsFree(opt);
	if (src_pt != NULL)
		GDALClose(src_pt);
	if (result != NULL)
		GDALClose(result);
	return result == NULL || err;
}
// #nocov end


// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdaltranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {

	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALTranslateOptions* opt =  GDALTranslateOptionsNew(options_char.data(), NULL);

	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_RASTER | GA_ReadOnly, NULL, NULL, NULL);
	if (src_pt == NULL)
		return 1; // #nocov
	GDALDatasetH result = GDALTranslate((const char *) dst[0], src_pt, opt, &err);
	if (src_pt != NULL)
		GDALClose(src_pt);
	GDALTranslateOptionsFree(opt);
	if (result != NULL)
		GDALClose(result);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalvectortranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {

	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALVectorTranslateOptions* opt =  GDALVectorTranslateOptionsNew(options_char.data(), NULL);

	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_VECTOR | GA_ReadOnly, NULL, NULL, NULL);
	if (src_pt == NULL)
		return 1; // #nocov
	// GDALDatasetH dst_pt = GDALOpen((const char *) dst[0], GA_Update);
	GDALDatasetH result = GDALVectorTranslate((const char *) dst[0], NULL, 1, &src_pt, opt, &err);
	if (src_pt != NULL)
		GDALClose(src_pt);
	GDALVectorTranslateOptionsFree(opt);
	if (result != NULL)
		GDALClose(result);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalbuildvrt(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {

	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALBuildVRTOptions* opt = GDALBuildVRTOptionsNew(options_char.data(), NULL);

	std::vector<const char *> srcpt(src.size());
	for (int i = 0; i < src.size(); i++)
		srcpt[i] = (const char *) src[i];

	GDALDatasetH result = GDALBuildVRT((const char *) dst[0], src.size(), NULL, srcpt.data(), opt, &err);

	GDALBuildVRTOptionsFree(opt);
	if (result != NULL)
		GDALClose(result);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdaldemprocessing(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector processing, Rcpp::CharacterVector colorfilename) {

	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALDEMProcessingOptions* opt =  GDALDEMProcessingOptionsNew(options_char.data(), NULL);

	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_RASTER | GA_ReadOnly, NULL, NULL, NULL);
	if (src_pt == NULL)
		Rcpp::stop("cannot open source dataset"); // #nocov
	GDALDatasetH result = GDALDEMProcessing((const char *) dst[0], src_pt, 
		processing.size() == 0 ? NULL : (const char *) processing[0], 
		colorfilename.size() == 0 ? NULL : (const char *) colorfilename[0], 
		opt, &err);
	GDALDEMProcessingOptionsFree(opt);
	if (result != NULL)
		GDALClose(result);
	if (src_pt != NULL)
		GDALClose(src_pt);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalnearblack(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {

	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALNearblackOptions* opt =  GDALNearblackOptionsNew(options_char.data(), NULL);

	// GDALDatasetH src_pt = GDALOpen((const char *) src[0], GA_ReadOnly);
	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_RASTER | GA_ReadOnly, NULL, NULL, NULL);
	GDALDatasetH dst_pt = GDALOpen((const char *) dst[0], GA_Update);
	GDALDatasetH result = GDALNearblack(NULL, dst_pt, src_pt, opt, &err);
	GDALNearblackOptionsFree(opt);
	if (src_pt != NULL) 
		GDALClose(src_pt);
/*	if (dst_pt != NULL) // don't: result == dst_pt
		GDALClose(dst_pt); */
	if (result != NULL)
		GDALClose(result);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalgrid(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {

	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALGridOptions* opt =  GDALGridOptionsNew(options_char.data(), NULL);

	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_ALL | GA_ReadOnly, NULL, NULL, NULL);
	GDALDatasetH result = GDALGrid((const char *) dst[0], src_pt, opt, &err);
	GDALGridOptionsFree(opt);
	if (src_pt != NULL)
		GDALClose(src_pt);
	if (result != NULL)
		GDALClose(result);
	return result == NULL || err;
}

#else
#include "Rcpp.h"

Rcpp::CharacterVector CPL_gdalinfo(Rcpp::CharacterVector obj, Rcpp::CharacterVector options) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalwarp(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalrasterize(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdaltranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalvectortranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalbuildvrt(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdaldemprocessing(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector processing, Rcpp::CharacterVector colorfilename) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalnearblack(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalgrid(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}
#endif

// #nocov start
// https://www.gdal.org/warptut.html :
// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdal_warper(Rcpp::CharacterVector infile, Rcpp::CharacterVector outfile,
		Rcpp::IntegerVector options)
{
    GDALDatasetH  hSrcDS, hDstDS;
    // Open input and output files.
    GDALAllRegister();
    hSrcDS = GDALOpen( infile[0], GA_ReadOnly );
	if (hSrcDS == NULL)
		Rcpp::stop("input file not found");
    hDstDS = GDALOpen( outfile[0], GA_Update );
	if (hDstDS == NULL)
		Rcpp::stop("could not open output file for writing");
    // Setup warp options.
    GDALWarpOptions *psWarpOptions = GDALCreateWarpOptions();
    psWarpOptions->hSrcDS = hSrcDS;
    psWarpOptions->hDstDS = hDstDS;

    psWarpOptions->nBandCount = 0;

	if (GDALGetRasterCount(hSrcDS) > GDALGetRasterCount(hDstDS))
		Rcpp::stop("warper: source has more bands than destination");

    psWarpOptions->padfSrcNoDataReal = (double *) CPLMalloc(sizeof(double) * GDALGetRasterCount(hSrcDS));
    psWarpOptions->padfDstNoDataReal = (double *) CPLMalloc(sizeof(double) * GDALGetRasterCount(hSrcDS));

    GDALRasterBandH poBand;
	int success;
	double d = 0xffffffff;
	for (int i = 0; i < GDALGetRasterCount(hSrcDS); i++) {
    	poBand = GDALGetRasterBand(hSrcDS, i + 1);
    	GDALGetRasterNoDataValue(poBand, &success);
		if (success)
    		psWarpOptions->padfSrcNoDataReal[i] = GDALGetRasterNoDataValue(poBand, &success);
    		// Rcpp::Rcout << GDALGetRasterNoDataValue(poBand, &success) << std::endl;
		else
			memcpy(&(psWarpOptions->padfSrcNoDataReal[i]), &d, sizeof(double));
    	poBand = GDALGetRasterBand(hDstDS, i + 1);
    	GDALGetRasterNoDataValue(poBand, &success);
		if (success)
    		psWarpOptions->padfDstNoDataReal[0] = GDALGetRasterNoDataValue(poBand, &success);
    		// Rcpp::Rcout << GDALGetRasterNoDataValue(poBand, &success) << std::endl;
		else // NaN:
			memcpy(&(psWarpOptions->padfDstNoDataReal[i]), &d, sizeof(double));
	}

    // psWarpOptions->pfnProgress = GDALTermProgress; // 0...10...20...30...40...50...60...70...80...90...100 - done.
    psWarpOptions->pfnProgress = GDALDummyProgress;
    // Establish reprojection transformer.
	if (options.size() == 1)
		psWarpOptions->eResampleAlg = (GDALResampleAlg) options[0];
    psWarpOptions->pTransformerArg =
        GDALCreateGenImgProjTransformer( hSrcDS,
                                         GDALGetProjectionRef(hSrcDS),
                                         hDstDS,
                                         GDALGetProjectionRef(hDstDS),
                                         FALSE, 0.0, 1 );
    psWarpOptions->pfnTransformer = GDALGenImgProjTransform;
    // Initialize and execute the warp operation.
    GDALWarpOperation oOperation;
    oOperation.Initialize( psWarpOptions );
    oOperation.ChunkAndWarpImage( 0, 0,
                                  GDALGetRasterXSize( hDstDS ),
                                  GDALGetRasterYSize( hDstDS ) );
    GDALDestroyGenImgProjTransformer( psWarpOptions->pTransformerArg );
    GDALDestroyWarpOptions( psWarpOptions );
    if (hDstDS)
		GDALClose( hDstDS );
    if (hSrcDS)
		GDALClose( hSrcDS );
    return false;
}
// #nocov end
