#include "cpl_port.h"
#include "cpl_conv.h" // CPLFree()
#include "gdal_version.h"
#include "gdalwarper.h"

#include <ogr_srs_api.h>
#include <ogr_spatialref.h>

#include "Rcpp.h"
#include "gdal.h" // local

#define NO_GDAL_CPP_HEADERS
#include "gdal_sf_pkg.h"

/* modified from GDALTermProgress: */
int CPL_STDCALL GDALRProgress( double dfComplete,
                                  CPL_UNUSED const char * pszMessage,
                                  CPL_UNUSED void * pProgressArg )
{
    const int nThisTick = std::min(40, std::max(0,
        static_cast<int>(dfComplete * 40.0) ));

    // Have we started a new progress run?
    static int nLastTick = -1;
    if( nThisTick < nLastTick && nLastTick >= 39 )
        nLastTick = -1;

    if( nThisTick <= nLastTick )
        return TRUE;

    while( nThisTick > nLastTick )
    {
        ++nLastTick;
        if( nLastTick % 4 == 0 )
            Rprintf("%d", (nLastTick / 4) * 10 );
        else
            Rprintf("." );
    }

    if( nThisTick == 40 )
        Rprintf(" - done.\n" );

    return TRUE;
}

#if GDAL_VERSION_NUM >= 2010000
# include "gdal_utils.h" // requires >= 2.1

void set_config_options(Rcpp::CharacterVector ConfigOptions) {
	if (ConfigOptions.size()) {
		if (ConfigOptions.attr("names") == R_NilValue)
			Rcpp::stop("config_options should be a character vector with names, as in c(key=\"value\")");
		Rcpp::CharacterVector names = ConfigOptions.attr("names");
		for (int i = 0; i < ConfigOptions.size(); i++)
			CPLSetConfigOption(names[i], ConfigOptions[i]);
	}
}

void unset_config_options(Rcpp::CharacterVector ConfigOptions) {
	if (ConfigOptions.size()) {
		Rcpp::CharacterVector names = ConfigOptions.attr("names");
		for (int i = 0; i < ConfigOptions.size(); i++)
			CPLSetConfigOption(names[i], NULL);
	}
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_gdalinfo(Rcpp::CharacterVector obj, Rcpp::CharacterVector options, 
		Rcpp::CharacterVector oo, Rcpp::CharacterVector co) {
	set_config_options(co);
	std::vector <char *> options_char = create_options(options, true);
	std::vector <char *> oo_char = create_options(oo, true); // open options
	GDALInfoOptions* opt = GDALInfoOptionsNew(options_char.data(), NULL);
	GDALDatasetH ds = NULL;
	if (obj.size())
		ds = GDALOpenEx((const char *) obj[0], GA_ReadOnly, NULL, oo_char.data(), NULL);
	char *ret_val = GDALInfo(ds, opt);
	if (ret_val == NULL)
		return Rcpp::CharacterVector::create();
	Rcpp::CharacterVector ret = ret_val; // copies
	CPLFree(ret_val);
	GDALInfoOptionsFree(opt);
	if (ds)
		GDALClose(ds);
	unset_config_options(co);
	return ret;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdaladdo(Rcpp::CharacterVector obj, Rcpp::CharacterVector method, 
		Rcpp::IntegerVector overviews, Rcpp::IntegerVector bands, Rcpp::CharacterVector oo,
		Rcpp::CharacterVector co, bool clean = false, bool read_only = false) {

	set_config_options(co);
	/*
	std::vector <char *> options_char = create_options(options, true);
	GDALInfoOptions* opt = GDALInfoOptionsNew(options_char.data(), NULL);
	*/
	std::vector <char *> oo_char = create_options(oo, true); // open options

	GDALDatasetH ds;
	if ((ds = GDALOpenEx((const char *) obj[0], 
						GDAL_OF_RASTER | (read_only ? GA_ReadOnly : GA_Update),
						NULL, oo_char.data(), NULL)) == NULL)
		Rcpp::stop(read_only ? "cannot open file for reading" : "cannot open file for writing");

	if (clean) { // remove overviews:
		if (GDALBuildOverviews(ds, method[0], 0, nullptr,
                               0, nullptr, GDALRProgress, nullptr) != CE_None) {
			GDALClose(ds);
			Rcpp::stop("error while cleaning overviews");
		}
	} else { // build overviews:
		if (GDALBuildOverviews(ds, method[0], 
					overviews.size(), overviews.size() ? &(overviews[0]) : NULL,
					bands.size(), bands.size() ? &(bands[0]) : NULL, 
					GDALRProgress, NULL) != CE_None) {
			GDALClose(ds);
			Rcpp::stop("error while building overviews");
		}
	}
	GDALClose(ds);
	unset_config_options(co);
	return true;
}

// #nocov start

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalwarp(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo,
		Rcpp::CharacterVector co, bool quiet = true, bool overwrite = false) {

	int err = 0;

	set_config_options(co);
	std::vector <char *> oo_char = create_options(oo, true); // open options
	std::vector<GDALDatasetH> src_pt(src.size());
	for (int i = 0; i < src.size(); i++)
		src_pt[i] = GDALOpenEx((const char *) src[i], GA_ReadOnly, NULL, oo_char.data(), NULL);

	std::vector <char *> doo_char = create_options(doo, true); // open options
	GDALDatasetH dst_ds = GDALOpenEx((const char *) dst[0], GDAL_OF_RASTER | GA_Update, NULL, doo_char.data(), NULL);

	std::vector <char *> options_char = create_options(options, true);
	GDALWarpAppOptions* opt = GDALWarpAppOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("warp: options error");

	if (! quiet) {
		GDALWarpAppOptionsSetProgress(opt, GDALRProgress, NULL);
#if GDAL_VERSION_NUM >= 2030000
		GDALWarpAppOptionsSetQuiet(opt, 0);
#endif
	}
	if (overwrite && dst_ds != NULL) {
		GDALClose(dst_ds);
		dst_ds = NULL;
	}
	GDALDatasetH result = GDALWarp(dst_ds == NULL ? (const char *) dst[0] : NULL, dst_ds, 
		src.size(), src_pt.data(), opt, &err);
	GDALWarpAppOptionsFree(opt);
	for (int i = 0; i < src.size(); i++)
		if (src_pt[i] != NULL)
			GDALClose(src_pt[i]);
	if (result != NULL)
		GDALClose(result);
	unset_config_options(co);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalrasterize(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo,
		Rcpp::CharacterVector co, bool overwrite = false, bool quiet = true) {

	set_config_options(co);
	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	std::vector <char *> oo_char = create_options(oo, true); // open options
	GDALRasterizeOptions* opt =  GDALRasterizeOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("rasterize: options error");

	if (! quiet)
		GDALRasterizeOptionsSetProgress(opt, GDALRProgress, NULL);
	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_VECTOR | GA_ReadOnly, 
		NULL, oo_char.data(), NULL);
	if (src_pt == NULL)
		Rcpp::stop("source dataset not found");
	unset_error_handler();
	GDALDatasetH dst_pt = NULL;
	if (! overwrite) {
		std::vector <char *> doo_char = create_options(doo, true); // open options
		dst_pt = GDALOpenEx((const char *) dst[0], GDAL_OF_RASTER | GA_Update, NULL, doo_char.data(), NULL);
	}
	set_error_handler();
	GDALDatasetH result = 
		GDALRasterize(dst_pt == NULL ? (const char *) dst[0] : NULL, dst_pt, src_pt, opt, &err);
	GDALRasterizeOptionsFree(opt);
	if (src_pt != NULL)
		GDALClose(src_pt);
	if (result != NULL)
		GDALClose(result);
	unset_config_options(co);
	return result == NULL || err;
}
// #nocov end


// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdaltranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector co,
		bool quiet = true) {

	set_config_options(co);
	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	std::vector <char *> oo_char = create_options(oo, true);
	GDALTranslateOptions* opt =  GDALTranslateOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("translate: options error");

	if (! quiet)
		GDALTranslateOptionsSetProgress(opt, GDALRProgress, NULL);
	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_RASTER | GA_ReadOnly, 
		NULL, oo_char.data(), NULL);
	if (src_pt == NULL)
		return 1; // #nocov
	GDALDatasetH result = GDALTranslate((const char *) dst[0], src_pt, opt, &err);
	GDALTranslateOptionsFree(opt);
	// see https://github.com/r-spatial/sf/issues/1352:
	if (result != NULL)
		GDALClose(result);
	if (src_pt != NULL)
		GDALClose(src_pt);
	unset_config_options(co);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalvectortranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo,
		Rcpp::CharacterVector co, bool quiet = true) {

	set_config_options(co);
	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALVectorTranslateOptions* opt =  GDALVectorTranslateOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("vectortranslate: options error");

	if (! quiet)
		GDALVectorTranslateOptionsSetProgress(opt, GDALRProgress, NULL);
	std::vector <char *> oo_char = create_options(oo, true); // open options
	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_VECTOR | GA_ReadOnly, NULL, 
		oo_char.data(), NULL);
	if (src_pt == NULL)
		return 1; // #nocov
	std::vector <char *> doo_char = create_options(doo, true); // open options
	unset_error_handler();
	GDALDatasetH dst_pt = GDALOpenEx((const char *) dst[0], GDAL_OF_VECTOR | GA_Update, NULL, doo_char.data(), NULL);
	set_error_handler();
	GDALDatasetH result = 
		GDALVectorTranslate(dst_pt == NULL ? (const char *) dst[0] : NULL, dst_pt, 1, &src_pt, opt, &err);
	GDALVectorTranslateOptionsFree(opt);
	GDALClose(src_pt);
	if (result != NULL)
		GDALClose(result);
	unset_config_options(co);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalbuildvrt(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, 
		Rcpp::CharacterVector co, bool quiet = true) {

	set_config_options(co);
	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALBuildVRTOptions* opt = GDALBuildVRTOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("buildvrt: options error");

	if (! quiet)
		GDALBuildVRTOptionsSetProgress(opt, GDALRProgress, NULL);
	GDALDatasetH result = NULL;
	if (oo.size()) { 
		// to understand why we don't always take this path, read: 
		// https://github.com/r-spatial/sf/issues/1336
		std::vector <char *> oo_char = create_options(oo, true); // open options
		std::vector<GDALDatasetH> srcpt(src.size());
		for (int i = 0; i < src.size(); i++) {
			srcpt[i] = GDALOpenEx((const char *) src[i], GDAL_OF_RASTER | GA_ReadOnly, NULL, 
				oo_char.data(), NULL);
			if (srcpt[i] == NULL)
				Rcpp::stop("cannot open source dataset");
		}
		result = GDALBuildVRT((const char *) dst[0], src.size(), srcpt.data(), NULL, opt, &err);
		for (int i = 0; i < src.size(); i++)
			GDALClose(srcpt[i]);
	} else {
		std::vector<const char *> srcpt(src.size());
		for (int i = 0; i < src.size(); i++)
			srcpt[i] = (const char *) src[i];
		result = GDALBuildVRT((const char *) dst[0], src.size(), NULL, srcpt.data(), opt, &err);
	}
	GDALBuildVRTOptionsFree(opt);
	if (result != NULL)
		GDALClose(result);
	unset_config_options(co);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdaldemprocessing(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector processing, 
		Rcpp::CharacterVector colorfilename, Rcpp::CharacterVector oo, 
		Rcpp::CharacterVector co, bool quiet = true) {

	int err = 0;
	set_config_options(co);
	std::vector <char *> options_char = create_options(options, true);
	std::vector <char *> oo_char = create_options(oo, true); // open options
	GDALDEMProcessingOptions* opt =  GDALDEMProcessingOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("demprocessing: options error");

	if (! quiet)
		GDALDEMProcessingOptionsSetProgress(opt, GDALRProgress, NULL);
	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_RASTER | GA_ReadOnly, 
		NULL, oo_char.data(), NULL);
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
	unset_config_options(co);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalnearblack(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo, 
		Rcpp::CharacterVector co, bool quiet = true) {

	set_config_options(co);
	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	std::vector <char *> oo_char = create_options(oo, true); // open options
	std::vector <char *> doo_char = create_options(doo, true); // open options
	GDALNearblackOptions* opt =  GDALNearblackOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("nearblack: options error");

	if (! quiet)
		GDALNearblackOptionsSetProgress(opt, GDALRProgress, NULL);
	// GDALDatasetH src_pt = GDALOpen((const char *) src[0], GA_ReadOnly);
	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_RASTER | GA_ReadOnly, NULL, oo_char.data(), NULL);
	GDALDatasetH dst_pt = GDALOpenEx((const char *) dst[0], GDAL_OF_RASTER | GA_Update, NULL, doo_char.data(), NULL);
	GDALDatasetH result = GDALNearblack(dst_pt == NULL ? (const char *) dst[0] : NULL, dst_pt, src_pt, opt, &err);
	GDALNearblackOptionsFree(opt);
	if (src_pt != NULL) 
		GDALClose(src_pt);
	if (result != NULL)
		GDALClose(result);
	unset_config_options(co);
	return result == NULL || err;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalgrid(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo,
		Rcpp::CharacterVector co, bool quiet = true) {

	set_config_options(co);
	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	std::vector <char *> oo_char = create_options(oo, true); // open options
	GDALGridOptions* opt =  GDALGridOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("grid: options error");

	if (! quiet)
		GDALGridOptionsSetProgress(opt, GDALRProgress, NULL);
	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_ALL | GA_ReadOnly, 
		NULL, oo_char.data(), NULL);
	GDALDatasetH result = GDALGrid((const char *) dst[0], src_pt, opt, &err);
	GDALGridOptionsFree(opt);
	if (src_pt != NULL)
		GDALClose(src_pt);
	if (result != NULL)
		GDALClose(result);
	unset_config_options(co);
	return result == NULL || err;
}

// gdal >= 3.1: mdim utils:
#if GDAL_VERSION_NUM >= 3010000
// [[Rcpp::export]]
Rcpp::CharacterVector CPL_gdalmdiminfo(Rcpp::CharacterVector obj, Rcpp::CharacterVector options,
		Rcpp::CharacterVector oo, Rcpp::CharacterVector co) { 
	set_config_options(co);
	std::vector <char *> oo_char = create_options(oo, true);
	GDALDatasetH ds = GDALOpenEx((const char *) obj[0], GDAL_OF_MULTIDIM_RASTER | GDAL_OF_VERBOSE_ERROR , NULL, oo_char.data(), NULL);
	if (ds == NULL) {
		Rcpp::Rcout << "failed to open " << obj[0] << std::endl;
		Rcpp::stop("Error opening data source");
	}
	std::vector <char *> options_char = create_options(options, true);
	GDALMultiDimInfoOptions* opt = GDALMultiDimInfoOptionsNew(options_char.data(), NULL);
	char *ret_val = GDALMultiDimInfo(ds, opt);
	GDALMultiDimInfoOptionsFree(opt);
	GDALClose(ds);
	if (ret_val == NULL)
		Rcpp::stop("GDALMultiDimInfo returned NULL");
	Rcpp::CharacterVector ret(1);
	ret[0] = ret_val;
	CPLFree(ret_val);
	unset_config_options(co);
	return ret;
}

// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdalmdimtranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector co,
		bool quiet = true) {

	set_config_options(co);
	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	std::vector <char *> oo_char = create_options(oo, true);
	GDALMultiDimTranslateOptions* opt = GDALMultiDimTranslateOptionsNew(options_char.data(), NULL);
	if (opt == NULL)
		Rcpp::stop("mdimtranslate: options error");

	if (! quiet)
		GDALMultiDimTranslateOptionsSetProgress(opt, GDALRProgress, NULL);
	std::vector<GDALDatasetH> srcpt(src.size());
	for (int i = 0; i < src.size(); i++) {
		srcpt[i] = GDALOpenEx((const char *) src[i], GDAL_OF_RASTER | GDAL_OF_MULTIDIM_RASTER | GDAL_OF_VERBOSE_ERROR, NULL, 
			oo_char.data(), NULL);
		if (srcpt[i] == NULL) {
			Rcpp::Rcout << "dataset: " << src[i] << ": " << std::endl;
			Rcpp::stop("Cannot open source dataset");
		}
	}
	GDALDatasetH result = GDALMultiDimTranslate((const char *) dst[0], NULL, srcpt.size(), srcpt.data(), opt, &err);
	GDALMultiDimTranslateOptionsFree(opt);
	if (result != NULL)
		GDALClose(result);
	else 
		Rcpp::stop("failed to open destination data set");
	for (int i = 0; i < src.size(); i++)
		GDALClose(srcpt[i]);
	unset_config_options(co);
	return result == NULL || err;
}
#else
Rcpp::CharacterVector CPL_gdalmdiminfo(Rcpp::CharacterVector obj, Rcpp::CharacterVector options, 
		Rcpp::CharacterVector oo) {
	Rcpp::stop("GDAL version >= 3.1 required for mdiminfo");
}
Rcpp::LogicalVector CPL_gdalmdimtranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, bool quiet = true) {
	Rcpp::stop("GDAL version >= 3.1 required for mdimtranslate");
}
#endif

#else
#include "Rcpp.h"

Rcpp::CharacterVector CPL_gdalinfo(Rcpp::CharacterVector obj, Rcpp::CharacterVector options, 
		Rcpp::CharacterVector oo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalwarp(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalrasterize(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo, Rcpp::CharacterVector co,
		bool overwrite = false) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdaltranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalvectortranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalbuildvrt(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdaldemprocessing(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector processing, Rcpp::CharacterVector colorfilename,
		Rcpp::CharacterVector oo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalnearblack(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::LogicalVector CPL_gdalgrid(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 2.1 required for gdal_utils");
}

Rcpp::CharacterVector CPL_gdalmdiminfo(Rcpp::CharacterVector obj, Rcpp::CharacterVector options, 
		Rcpp::CharacterVector oo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 3.1 required for mdiminfo");
}

Rcpp::LogicalVector CPL_gdalmdimtranslate(Rcpp::CharacterVector src, Rcpp::CharacterVector dst,
		Rcpp::CharacterVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector co) {
	Rcpp::stop("GDAL version >= 3.1 required for mdimtranslate");
}
#endif

// #nocov start
// https://gdal.org/tutorials/warp_tut.html
// [[Rcpp::export]]
Rcpp::LogicalVector CPL_gdal_warper(Rcpp::CharacterVector infile, Rcpp::CharacterVector outfile,
		Rcpp::IntegerVector options, Rcpp::CharacterVector oo, Rcpp::CharacterVector doo,
		Rcpp::CharacterVector co, bool quiet = true) {

	set_config_options(co);
	std::vector <char *> oo_char = create_options(oo, true); // open options
	GDALDatasetH  hSrcDS, hDstDS;
	// Open input and output files.
	GDALAllRegister();
	hSrcDS = GDALOpenEx( infile[0], GA_ReadOnly, NULL, oo_char.data(), NULL );
	if (hSrcDS == NULL)
		Rcpp::stop("input file not found");
	std::vector <char *> doo_char = create_options(doo, true); // open options
	hDstDS = GDALOpenEx(outfile[0], GA_Update, NULL, doo_char.data(), NULL);
	if (hDstDS == NULL)
		Rcpp::stop("could not open output file for writing");
	// Setup warp options.
	GDALWarpOptions *psWarpOptions = GDALCreateWarpOptions();
	psWarpOptions->hSrcDS = hSrcDS;
	psWarpOptions->hDstDS = hDstDS;

	if (GDALGetRasterCount(hSrcDS) != GDALGetRasterCount(hDstDS))
		Rcpp::stop("warper: source and destination must have the same number of bands");

	psWarpOptions->nBandCount = GDALGetRasterCount(hSrcDS);
	psWarpOptions->panSrcBands =
		(int *) CPLMalloc(sizeof(int) * psWarpOptions->nBandCount );
	psWarpOptions->panDstBands =
		(int *) CPLMalloc(sizeof(int) * psWarpOptions->nBandCount );
	for (int i = 0; i < psWarpOptions->nBandCount; i++) {
		psWarpOptions->panSrcBands[i] = i + 1;
		psWarpOptions->panDstBands[i] = i + 1;
	}

	psWarpOptions->padfSrcNoDataReal = (double *) CPLMalloc(sizeof(double) * GDALGetRasterCount(hSrcDS));
	psWarpOptions->padfDstNoDataReal = (double *) CPLMalloc(sizeof(double) * GDALGetRasterCount(hSrcDS));

	GDALRasterBandH poBand;
	int success;
	double d = 0xffffffff;
	for (int i = 0; i < GDALGetRasterCount(hSrcDS); i++) {
		poBand = GDALGetRasterBand(hSrcDS, i + 1);
		GDALGetRasterNoDataValue(poBand, &success);
		if (success) {
			psWarpOptions->padfSrcNoDataReal[i] = GDALGetRasterNoDataValue(poBand, &success);
			// Rcpp::Rcout << "1: " << GDALGetRasterNoDataValue(poBand, &success) << std::endl;
		 } else
			memcpy(&(psWarpOptions->padfSrcNoDataReal[i]), &d, sizeof(double));
		poBand = GDALGetRasterBand(hDstDS, i + 1);
		GDALGetRasterNoDataValue(poBand, &success);
		if (success) {
			psWarpOptions->padfDstNoDataReal[i] = GDALGetRasterNoDataValue(poBand, &success);
			// Rcpp::Rcout << "2: " << GDALGetRasterNoDataValue(poBand, &success) << std::endl;
		} else // NaN:
			memcpy(&(psWarpOptions->padfDstNoDataReal[i]), &d, sizeof(double));
	}

	if (quiet)
		psWarpOptions->pfnProgress = GDALDummyProgress;
	else
		psWarpOptions->pfnProgress = GDALRProgress; // 0...10...20...30...40...50...60...70...80...90...100 - done.
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
	unset_config_options(co);
	return false;
}
// #nocov end
