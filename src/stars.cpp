#include <cpl_port.h>
#include <gdal.h>
#include <gdal_priv.h>
#include <ogr_api.h>
#include <ogr_geometry.h>
#include <ogr_srs_api.h>
#include <cpl_string.h>

#include <Rcpp.h>

#include "gdal.h"
#include "gdal_read.h"
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

List get_band_meta_data(GDALDataset *poDataset) {
	int n_bands = poDataset->GetRasterCount(); 
	List ret(n_bands);
	for (int band = 1; band <= n_bands; band++) { // unlike x & y, band is 1-based
		GDALRasterBand *poBand = poDataset->GetRasterBand( band );
		ret[band - 1] = charpp2CV(poBand->GetMetadata(NULL));
	}
	return ret;
}

// [[Rcpp::export]]
CharacterVector CPL_get_metadata(CharacterVector obj, CharacterVector domain_item,
		CharacterVector options) {
	
	GDALDatasetH ds = GDALOpenEx(obj[0], GDAL_OF_RASTER | GDAL_OF_READONLY, NULL, NULL, 
		create_options(options).data());
	CharacterVector ret = get_meta_data(ds, domain_item);
	if (ds != NULL)
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

bool equals_na(double value, double na, GDALDataType dt) {
	if (ISNAN(value))
		return true;
	if (dt == GDT_Float32)
		return (float) value == (float) na;
	else
		return value == na;
}

// formerly: stars/src/gdal.cpp
NumericVector read_gdal_data(GDALDataset *poDataset, 
			NumericVector nodatavalue, 
			int nXOff, 
			int nYOff, 
			int nXSize, 
			int nYSize, 
			int nBufXSize, 
			int nBufYSize, 
			IntegerVector bands,
			GDALRasterIOExtraArg *resample
		) {

	// collapse x & y into rows, redim later:
	NumericVector vec( 1.0 * nBufXSize * nBufYSize * bands.size() ); 
	// floor returns double -> no integer overflow

	// read bands:
	if (poDataset->RasterIO( GF_Read,
			nXOff,
			nYOff,
			nXSize,
			nYSize,
			vec.begin(),
			nBufXSize,
			nBufYSize,
			GDT_Float64, 
			bands.size(),
			bands.begin(), 
			0, 
			0, 
			0, 
			resample) == CE_Failure)
		stop("read failure"); // #nocov

	CharacterVector units(bands.size());
	// scale && set NA:
	for (int i = 0; i < bands.size(); i++) {
		int band = bands(i);
		GDALRasterBand *poBand = poDataset->GetRasterBand( band );
		// NumericVector nodatavalue = NumericVector::create(NA_REAL);
		// int success = 0, 
		int has_scale = 0, has_offset = 0;
		double offset = 0.0, scale = 1.0;
		// poBand->GetNoDataValue(&success);
		// if (success)
		// 	nodatavalue = poBand->GetNoDataValue(NULL); // #nocov
		poBand->GetScale(&has_scale);
		if (has_scale)
			scale = poBand->GetScale(NULL);
		poBand->GetOffset(&has_offset);
		if (has_offset)
			offset = poBand->GetOffset(NULL);
		units[i] = poBand->GetUnitType();
		if (! NumericVector::is_na(nodatavalue[0]) || has_offset || has_scale) {


			// for (R_xlen_t j = 0; j < Rf_xlength(vec); j++) {
			for (R_xlen_t j = i * (((R_xlen_t) nBufXSize) * nBufYSize); // start of band i
					j < (i + 1) * (((R_xlen_t) nBufXSize) * nBufYSize); // end of band i
					j++) {
				if (equals_na(vec[j], nodatavalue[0], poBand->GetRasterDataType()))
					vec[j] = NA_REAL; // #nocov
				else
					vec[j] = (vec[j] * scale) + offset;
			}
		}
		checkUserInterrupt();
	}
	vec.attr("units") = units;

	// set dim attr:
	IntegerVector dims;
	if (bands.size() == 1) { // single band:
		dims = IntegerVector::create(nBufXSize, nBufYSize);
		dims.attr("names") = CharacterVector::create("x", "y");
	} else { // multiple bands:
		dims = IntegerVector::create(nBufXSize, nBufYSize, bands.size()); // #nocov
		dims.attr("names") = CharacterVector::create("x", "y", "band"); // #nocov
	}
	vec.attr("dim") = dims;
	return vec;
}

int get_from_list(List lst, const char *name, int otherwise) {
	if (lst.containsElementNamed(name)) {
		IntegerVector ret = lst[name]; // #nocov
		return(ret[0]);                // #nocov
	} else
		return(otherwise);
}

// [[Rcpp::export]]
List CPL_read_gdal(CharacterVector fname, CharacterVector options, CharacterVector driver,
		bool read_data, NumericVector NA_value, List RasterIO_parameters) {
// reads and returns data set metadata, and if read_data is true, adds data array
    GDALDataset *poDataset = (GDALDataset *) GDALOpenEx(fname[0], GA_ReadOnly,
		driver.size() ? create_options(driver).data() : NULL,
		options.size() ? create_options(options).data() : NULL,
		NULL);
    if (poDataset == NULL) {
		Rcout << "trying to read file: " << fname[0] << std::endl; // #nocov
        stop("file not found"); // #nocov
	}

	CharacterVector Driver = CharacterVector::create(
        poDataset->GetDriver()->GetDescription(),
        poDataset->GetDriver()->GetMetadataItem( GDAL_DMD_LONGNAME ));

	/*
	if (poDataset->GetRasterCount() == 0)
		stop("zero bands"); -->> this indicates there are (only) subdatasets
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
	bool geo_transform_set = (err == CE_None);

	// CRS, projection:
	const char *wkt = poDataset->GetProjectionRef();
	CharacterVector proj = NA_STRING;
	CharacterVector p4 = NA_STRING;
	if (*wkt != '\0') {
		proj = CharacterVector::create(wkt);
		// proj4string:
		OGRSpatialReference *sr = new OGRSpatialReference;
		sr = handle_axis_order(sr);
		char **ppt = (char **) &wkt;
#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
		sr->importFromWkt(ppt);
#else
		sr->importFromWkt( (const char**) ppt);
#endif
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
		if (NA_value.size() > 0 && !NumericVector::is_na(NA_value[0])) {
			if (set)
				warning("NoDataValue of band is ignored"); // #nocov
			nodatavalue[0] = NA_value[0];
		} else if (set)
			nodatavalue[0] = poBand->GetNoDataValue(NULL); // #nocov
	}

	CharacterVector items = get_meta_data((GDALDatasetH) poDataset, NA_STRING);
	CharacterVector sub = NA_STRING;
	for (int i = 0; i < items.size(); i++) {
		// Rcpp::Rcout << items[i] << std::endl;
		if (items[i] == "SUBDATASETS")
			sub = get_meta_data(poDataset, "SUBDATASETS"); // #nocov
	}

	// image dimension:
	int nXOff = get_from_list(RasterIO_parameters, "nXOff", 1) - 1;
	int nYOff = get_from_list(RasterIO_parameters, "nYOff", 1) - 1;
	int nXSize = get_from_list(RasterIO_parameters, "nXSize", poDataset->GetRasterXSize() - nXOff);
	int nYSize = get_from_list(RasterIO_parameters, "nYSize", poDataset->GetRasterYSize() - nYOff);
	int nBufXSize = get_from_list(RasterIO_parameters, "nBufXSize", nXSize);
	int nBufYSize = get_from_list(RasterIO_parameters, "nBufYSize", nYSize);

	// bands:
	IntegerVector bands; 
	if (RasterIO_parameters.containsElementNamed("bands"))
		bands = RasterIO_parameters["bands"]; // #nocov
	else {
		bands = IntegerVector(poDataset->GetRasterCount());
		for (int j = 0; j < bands.size(); j++)
			bands(j) = j + 1; // bands is 1-based
	}

	// resampling method:
	GDALRasterIOExtraArg resample;
	INIT_RASTERIO_EXTRA_ARG(resample);

	if (RasterIO_parameters.containsElementNamed("resample")) { // #nocov start
		CharacterVector res = RasterIO_parameters["resample"];
		if (res[0] == "bilinear")
			resample.eResampleAlg = GRIORA_Bilinear;
		else if (res[0] == "cubic")
			resample.eResampleAlg = GRIORA_Cubic;
		else if (res[0] == "cubic_spline")
			resample.eResampleAlg = GRIORA_CubicSpline;
		else if (res[0] == "lanczos")
			resample.eResampleAlg = GRIORA_Lanczos;
		else if (res[0] == "average")
			resample.eResampleAlg = GRIORA_Average;
		else if (res[0] == "mode")
			resample.eResampleAlg = GRIORA_Mode;
		else if (res[0] == "Gauss")
			resample.eResampleAlg = GRIORA_Gauss;
		else if (res[0] == "nearest_neighbour")
			resample.eResampleAlg = GRIORA_NearestNeighbour;
		else stop("unknown method for resample"); // #nocov end
	} 

	List ReturnList = List::create(
		_["filename"] = fname,
		_["driver"] = Driver,
		_["cols"] = NumericVector::create(nXOff + 1, nXOff + nBufXSize),
		_["rows"] = NumericVector::create(nYOff + 1, nYOff + nBufYSize),
		_["bands"] = bands,
		_["proj_wkt"] = proj,
		_["proj4string"] = p4,
		_["geotransform"] = geotransform,
		_["datatype"] =	poBand != NULL ? 
			GDALGetDataTypeName(poBand->GetRasterDataType()) :
			CharacterVector::create(NA_STRING),
		_["sub"] = sub,
		_["meta"] = get_meta_data(poDataset, CharacterVector::create()),
		_["band_meta"] = get_band_meta_data(poDataset)
	);
	if (read_data) {
		ReturnList.attr("data") = read_gdal_data(poDataset, nodatavalue, nXOff, nYOff, 
			nXSize, nYSize, nBufXSize, nBufYSize, bands, &resample);
	}
	GDALClose(poDataset);

	// adjust geotransform & offset if Buf?Size was set:
	if ((nXSize != nBufXSize || nYSize != nBufYSize) && geo_transform_set) { // #nocov start
		if (geotransform[2] != 0.0 || geotransform[4] != 0.0)
			stop("reading affine grids with resampling would result in a wrong geotransform; please file an issue"); // #nocov
		double ratio_x = (1.0 * nXSize) / nBufXSize;
		double ratio_y = (1.0 * nYSize) / nBufYSize;
		geotransform[1] = geotransform[1] * ratio_x;
		geotransform[5] = geotransform[5] * ratio_y;
		ReturnList["geotransform"] = geotransform;
		nXOff = (int) (nXOff / ratio_x);
		nYOff = (int) (nYOff / ratio_y);
		ReturnList["cols"] = NumericVector::create(nXOff + 1, nXOff + nBufXSize);
		ReturnList["rows"] = NumericVector::create(nYOff + 1, nYOff + nBufYSize);
	} // #nocov end

	return ReturnList;
}

// [[Rcpp::export]]
void CPL_write_gdal(NumericMatrix x, CharacterVector fname, CharacterVector driver, CharacterVector options,
		CharacterVector Type, IntegerVector dims, IntegerVector from,
		NumericVector gt, CharacterVector p4s, NumericVector na_val,
		bool create = true, bool only_create = false) {

	// figure out driver:
	if (driver.length() != 1)
		stop("driver should have length one"); // #nocov
	GDALDriver *poDriver;
	if ((poDriver = GetGDALDriverManager()->GetDriverByName(driver[0])) == NULL)
		stop("driver not recognized."); // #nocov

	// can this driver Create()?
	char **papszMetadata = poDriver->GetMetadata();
	if (!CSLFetchBoolean( papszMetadata, GDAL_DCAP_CREATE, FALSE ) )
		stop("driver does not support Create() method."); // #nocov

	// figure out eType:
	GDALDataType eType = GDT_Unknown;
	if (Type.length() != 1)
		stop("Type should have length 1"); // #nocov
	if (Type[0] == "Byte")
		eType = GDT_Byte; // #nocov
	else if (Type[0] == "UInt16")
		eType = GDT_UInt16; // #nocov
	else if (Type[0] == "Int16")
		eType = GDT_Int16; // #nocov
	else if (Type[0] == "UInt32")
		eType = GDT_UInt32; // #nocov
	else if (Type[0] == "Int16")
		eType = GDT_Int32; // #nocov
	else if (Type[0] == "Float32")
		eType = GDT_Float32;
	else if (Type[0] == "Float64") // #nocov
		eType = GDT_Float64; // #nocov
	else
		stop("unknown data type"); // #nocov

	// sanity checks:
	if (fname.length() != 1)
		stop("fname should have length one"); // #nocov
	if (dims.length() != 3)
		stop("dims should have length three"); // #nocov
	if (from.length() != 2)
		stop("from should have length two"); // #nocov
	if (na_val.length() != 1)
		stop("na_val should have length 1"); // #nocov

	// create dataset:
	GDALDataset *poDstDS;
	if (create) {
		if (from[0] != 0 || from[1] != 0)
			stop("from values should be zero when creating a dataset"); // #nocov
		if ((poDstDS = poDriver->Create( fname[0], dims[0], dims[1], dims[2], eType,
				create_options(options).data())) == NULL)
			stop("creating dataset failed"); // #nocov

		// set geotransform:
		double adfGeoTransform[6];
		if (gt.length() != 6)
			stop("gt should have length 6"); // #nocov
		for (int i = 0; i < gt.length(); i++)
			adfGeoTransform[i] = gt[i];
		if (poDstDS->SetGeoTransform( adfGeoTransform ) != CE_None)
			warning("SetGeoTransform() returned an error: not available?"); // #nocov

		// CRS:
		if (p4s.length() != 1)
			stop("p4s should have length three"); // #nocov
		OGRSpatialReference oSRS;
		oSRS.importFromProj4( p4s[0] );
		char *pszSRS_WKT = NULL;
		oSRS.exportToWkt( &pszSRS_WKT );
		if (poDstDS->SetProjection( pszSRS_WKT ) != CE_None)
			stop("SetProjection: error"); // #nocov
		CPLFree( pszSRS_WKT );

		// set band NA's
		if (! NumericVector::is_na(na_val[0])) {
			for (int band = 1; band <= dims(2); band++) { // unlike x & y, band is 1-based
				GDALRasterBand *poBand = poDstDS->GetRasterBand( band );
				if (poBand->SetNoDataValue(na_val[0]) != CE_None) {
					warning("SetNoDataValue not supported by driver"); // #nocov
					break; // #nocov
				}
			}
		}

	} else { // no create, update:
		if ((poDstDS = (GDALDataset *) GDALOpen(fname[0], GA_Update)) == NULL) // #nocov
			stop("updating dataset failed"); // #nocov
	}

	if (! only_create) {
		if (! NumericVector::is_na(na_val[0])) { // replace R's NA's with GDAL write NA value
			for (int i = 0; i < x.ncol(); i++) {
				NumericVector nv = x(_, i);
				for (int j = 0; j < x.nrow(); j++) {
					if (NumericVector::is_na(nv[j]))
						nv[j] = na_val[0];
				}
				x(_, i) = nv;
			}
			checkUserInterrupt();
		}
		// write values:
		// write the whole lot:
		if (poDstDS->RasterIO(GF_Write, from[0], from[1], dims[0] - from[0], dims[1] - from[1],
				x.begin(), dims[0] - from[0], dims[1] - from[1], GDT_Float64, 
				dims[2], NULL, 0, 0, 0, NULL) == CE_Failure)
			stop("write failure"); // #nocov
	}

	/* close: */
	GDALClose( (GDALDatasetH) poDstDS );
	return;
}
