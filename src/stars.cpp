#include <cpl_port.h>
#include <gdal.h>
#include <gdal_priv.h>
#include <ogr_api.h>
#include <ogr_geometry.h>
#include <ogr_srs_api.h>

#include <Rcpp.h>

#include "gdal_sf_pkg.h"

using namespace Rcpp;

CharacterVector get_meta_data(GDALDatasetH ds, CharacterVector domain_item) {

	CharacterVector ret;
	if (ds == NULL)
		return NA_STRING;
	if (domain_item.size() == 0)
		ret = charpp2CV(GDALGetMetadata(ds, NULL)); // #nocov
	else if (domain_item.size() == 1) {
		if (CharacterVector::is_na(domain_item[0])) {
			 char **dl = GDALGetMetadataDomainList(ds);
			 ret = charpp2CV(dl);
			 CSLDestroy(dl);
		} else
			 ret = charpp2CV(GDALGetMetadata(ds, domain_item[0]));
	} else if (domain_item.size() == 2) // domain and item // #nocov start
		ret = CharacterVector::create(GDALGetMetadataItem(ds, domain_item[1], domain_item[0]));
	else
		ret = NA_STRING; // #nocov end
	return(ret);
}

// [[Rcpp::export]]
CharacterVector CPL_get_metadata(CharacterVector obj, CharacterVector domain_item,
		CharacterVector options) {
	
	GDALDatasetH ds = GDALOpenEx(obj[0], GDAL_OF_RASTER | GDAL_OF_READONLY, NULL, NULL, 
		create_options(options).data());
	CharacterVector ret = get_meta_data(ds, domain_item);
	GDALClose(ds);
	return ret;
}

// [[Rcpp::export]]
List CPL_get_crs(CharacterVector obj, CharacterVector options) {
	List ret(4);
	GDALDatasetH ds = GDALOpenEx(obj[0], GDAL_OF_RASTER | GDAL_OF_READONLY, NULL, NULL, 
		create_options(options).data());
	if (ds == NULL)
		return ret; // #nocov

	ret(0) = GDALGetRasterCount(ds);

	ret(1) = GDALGetProjectionRef(ds);

	double gt[6];
	GDALGetGeoTransform(ds, gt);
	NumericVector gt_r(6);
	for (int i = 0; i < 6; i++)
		gt_r(i) = gt[i];
	ret(2) =  gt_r;

	double gt_inv[6];
	int retval = GDALInvGeoTransform(gt, gt_inv);
	NumericVector gt_r_inv(6);
	for (int i = 0; i < 6; i++)
		gt_r_inv(i) = retval ? gt_inv[i] : NA_REAL;
	ret(3) =  gt_r_inv;

	ret.attr("names") = CharacterVector::create("nbands", "crs", "gt", "gt_inv");

	return ret;
}
// [[Rcpp::export]]
NumericVector CPL_inv_geotransform(NumericVector gt_r) {
	if (gt_r.size() != 6)
		stop("wrong length geotransform"); // #nocov
	double gt_inv[6], gt[6];
	for (int i = 0; i < 6; i++)
		gt[i] = gt_r[i];
	int retval = GDALInvGeoTransform(gt, gt_inv);
	NumericVector gt_r_inv(6);
	for (int i = 0; i < 6; i++)
		gt_r_inv(i) = retval ? gt_inv[i] : NA_REAL;
	return gt_r_inv;
}

// formerly: stars/src/gdal.cpp
NumericMatrix CPL_read_gdal_data(Rcpp::List meta, GDALDataset *poDataset, NumericVector nodatavalue) {

	IntegerVector x = meta["cols"];
	IntegerVector y = meta["rows"];
	IntegerVector bands = meta["bands"];

	size_t nx = diff(x)[0] + 1, ny = diff(y)[0] + 1, nbands = diff(bands)[0] + 1,
		resample = 1; // still need to find out what resample exactly does, 
		              // and how it interacts with/should change geotransform

	// GDALRasterBand *poBand = poDataset->GetRasterBand( 1 );
	NumericMatrix mat( (nx / resample) * (ny / resample), nbands );

	IntegerVector band_index(nbands);
	for (int j = 0, i = bands(0); i <= bands(1); i++)
		band_index(j++) = i;
	// read the full set of bands:
	CPLErr err = poDataset->RasterIO( GF_Read,
			x(0) - 1, y(0) - 1,
			nx, ny,
			mat.begin(),
			nx / resample, ny / resample,
			GDT_Float64, nbands, band_index.begin(), 0, 0, 0);
	if (err == CE_Failure)
		stop("read failure"); // #nocov

	for (int band = bands(0); band <= bands(1); band++) { // unlike x & y, band is 1-based
		GDALRasterBand *poBand = poDataset->GetRasterBand( band );
		NumericVector nodatavalue = NumericVector::create(NA_REAL);
		int success = 0, has_scale = 0, has_offset = 0;
		double offset = 0.0, scale = 1.0;
		poBand->GetNoDataValue(&success);
		if (success)
			nodatavalue = poBand->GetNoDataValue(NULL); // #nocov
		poBand->GetScale(&has_scale);
		if (has_scale)
			scale = poBand->GetScale(NULL);
		poBand->GetOffset(&has_offset);
		if (has_offset)
			offset = poBand->GetOffset(NULL);
		// char *units = poBand->GetUnits();
		if (! NumericVector::is_na(nodatavalue[0]) || has_offset || has_scale) {
			NumericVector nv = mat(_, band - 1);
			for (int i = 0; i < nv.size(); i++) {
				if (nv[i] == nodatavalue[0])
					nv[i] = NA_REAL; // #nocov
				else
					nv[i] = (nv[i] * scale) + offset;
			}
			mat(_, band - 1) = nv;
		}
		checkUserInterrupt();
	}

	// dim:
	IntegerVector dims;
	if (diff(bands)[0] == 0) { // one band:
		dims = IntegerVector::create(nx / resample, ny / resample);
		dims.attr("names") = CharacterVector::create("x", "y");
	} else {
		dims = IntegerVector::create(nx / resample, ny / resample, diff(bands)[0] + 1); // #nocov
		dims.attr("names") = CharacterVector::create("x", "y", "band"); // #nocov
	}
	mat.attr("dim") = dims;
	return mat;
}

// [[Rcpp::export]]
List CPL_read_gdal(CharacterVector fname, CharacterVector options, CharacterVector driver,
		bool read_data = true) {
// reads data set metadata, and if read_data is true, adds data array
    GDALDataset  *poDataset;
	poDataset = (GDALDataset *) GDALOpenEx(fname[0], GA_ReadOnly,
		driver.size() ? create_options(driver).data() : NULL,
		options.size() ? create_options(options).data() : NULL,
		NULL);
    if( poDataset == NULL )
        stop("file not found"); // #nocov

	CharacterVector Driver = CharacterVector::create(
        poDataset->GetDriver()->GetDescription(),
        poDataset->GetDriver()->GetMetadataItem( GDAL_DMD_LONGNAME ));

	/*
	if (poDataset->GetRasterCount() == 0)
		stop("zero bands");
	*/

	// geotransform:
	double adfGeoTransform[6];
	CPLErr err = poDataset->GetGeoTransform( adfGeoTransform );
	NumericVector geotransform = NumericVector::create(
		err == CE_None ? adfGeoTransform[0] : NA_REAL,
		err == CE_None ? adfGeoTransform[1] : NA_REAL,
		err == CE_None ? adfGeoTransform[2] : NA_REAL,
		err == CE_None ? adfGeoTransform[3] : NA_REAL,
		err == CE_None ? adfGeoTransform[4] : NA_REAL,
		err == CE_None ? adfGeoTransform[5] : NA_REAL);
/*	 
	// from GDAL tutorial:

	int             nBlockXSize, nBlockYSize;
	int             bGotMin, bGotMax;
	double          adfMinMax[2];
	poBand->GetBlockSize( &nBlockXSize, &nBlockYSize );
	printf( "Block=%dx%d Type=%s, ColorInterp=%s\n",
       		nBlockXSize, nBlockYSize,
        	GDALGetDataTypeName(poBand->GetRasterDataType()),
        	GDALGetColorInterpretationName(
           	poBand->GetColorInterpretation()) ); 

	adfMinMax[0] = poBand->GetMinimum( &bGotMin );
	adfMinMax[1] = poBand->GetMaximum( &bGotMax );
	if( ! (bGotMin && bGotMax) )
    	GDALComputeRasterMinMax((GDALRasterBandH)poBand, TRUE, adfMinMax);
	printf( "Min=%.3fd, Max=%.3f\n", adfMinMax[0], adfMinMax[1] );
	if( poBand->GetOverviewCount() > 0 )
    	printf( "Band has %d overviews.\n", poBand->GetOverviewCount() );
	if( poBand->GetColorTable() != NULL )
    	printf( "Band has a color table with %d entries.\n",
             		poBand->GetColorTable()->GetColorEntryCount() );
*/
	// projection:
	const char *wkt = poDataset->GetProjectionRef();
	CharacterVector proj = NA_STRING;
	CharacterVector p4 = NA_STRING;
	if (*wkt != '\0') {
		proj = CharacterVector::create(wkt);
		// proj4string:
		OGRSpatialReference *sr = new OGRSpatialReference;
		char **ppt = (char **) &wkt;
		sr->importFromWkt(ppt);
		char *proj4; 
		if (sr->exportToProj4(&proj4) != OGRERR_NONE) // need to error check?
			stop("failure to export SRS to proj.4"); // #nocov
		p4 = CharacterVector::create(proj4); // need to CPLFree?
		CPLFree(proj4);
		delete sr;
	}

	GDALRasterBand *poBand = NULL;
	NumericVector nodatavalue = NumericVector::create(NA_REAL);
	if (poDataset->GetRasterCount()) {
		poBand = poDataset->GetRasterBand( 1 );
		int set = 0;
		poBand->GetNoDataValue(&set);
		if (set)
			nodatavalue = poBand->GetNoDataValue(NULL); // #nocov
	}

	CharacterVector items = get_meta_data((GDALDatasetH) poDataset, NA_STRING);
	CharacterVector sub = NA_STRING;
	for (int i = 0; i < items.size(); i++) {
		// Rcpp::Rcout << items[i] << std::endl;
		if (items[i] == "SUBDATASETS")
			sub = get_meta_data(poDataset, "SUBDATASETS"); // #nocov
	}

	List ReturnList = List::create(
		_["filename"] = fname,
		_["driver"] = Driver,
		_["cols"] = NumericVector::create(1, poDataset->GetRasterXSize()),
		_["rows"] = NumericVector::create(1, poDataset->GetRasterYSize()),
		_["bands"] = NumericVector::create(1, poDataset->GetRasterCount()),
		_["proj_wkt"] = proj,
		_["proj4string"] = p4,
		_["geotransform"] = geotransform,
        _["datatype"] =	poBand != NULL ? 
			GDALGetDataTypeName(poBand->GetRasterDataType()) :
			CharacterVector::create(NA_STRING),
		_["sub"] = sub,
		_["meta"] = get_meta_data(poDataset, CharacterVector::create())
	);
	if (read_data)
		ReturnList.attr("data") = CPL_read_gdal_data(ReturnList, poDataset, nodatavalue);

	GDALClose(poDataset);
	return ReturnList;
}
