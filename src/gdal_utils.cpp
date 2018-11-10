#include "cpl_port.h"
#include "cpl_conv.h" // CPLFree()
#include "gdal_version.h"

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
	char *ret_val = GDALInfo(ds, opt);
	Rcpp::CharacterVector ret = ret_val; // copies
	CPLFree(ret_val);
	GDALInfoOptionsFree(opt);
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
		Rcpp::CharacterVector options) {

	int err = 0;
	std::vector <char *> options_char = create_options(options, true);
	GDALRasterizeOptions* opt =  GDALRasterizeOptionsNew(options_char.data(), NULL);

	GDALDatasetH src_pt = GDALOpenEx((const char *) src[0], GDAL_OF_VECTOR | GA_ReadOnly, NULL, NULL, NULL);
	if (src_pt == NULL)
		Rcpp::stop("source dataset not found");
	GDALDatasetH dst_pt = GDALOpen((const char *) dst[0], GA_Update);
	if (dst_pt == NULL)
		Rcpp::stop("cannot write to destination dataset");
	GDALDatasetH result = GDALRasterize(NULL, dst_pt, src_pt, opt, &err);
	GDALRasterizeOptionsFree(opt);
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
	GDALDatasetH result = GDALTranslate((const char *) dst[0], src_pt, opt, &err);
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
	// GDALDatasetH dst_pt = GDALOpen((const char *) dst[0], GA_Update);
	GDALDatasetH result = GDALVectorTranslate((const char *) dst[0], NULL, 1, &src_pt, opt, &err);
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
	GDALClose(src_pt);
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
