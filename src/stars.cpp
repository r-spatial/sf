#include <cpl_port.h>
#include <gdal.h>
#include <gdal_priv.h>
#include <gdal_rat.h>
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

	ret(1) = GDALGetProjectionRef(ds); // wkt

	double gt[6];
	GDALGetGeoTransform(ds, gt);
	NumericVector gt_r(6);
	for (int i = 0; i < 6; i++)
		gt_r(i) = gt[i];
	ret(2) = gt_r;

	double gt_inv[6];
	int retval = GDALInvGeoTransform(gt, gt_inv);
	NumericVector gt_r_inv(6);
	for (int i = 0; i < 6; i++)
		gt_r_inv(i) = retval ? gt_inv[i] : NA_REAL;
	ret(3) = gt_r_inv;

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
		// if (! NumericVector::is_na(nodatavalue[0]) || has_offset || has_scale) {
		// outcommented because of NaN handling, https://github.com/r-spatial/stars/issues/333
		for (R_xlen_t j = i * (((R_xlen_t) nBufXSize) * nBufYSize); // start of band i
				j < (i + 1) * (((R_xlen_t) nBufXSize) * nBufYSize); // end of band i
				j++) {
			if (equals_na(vec[j], nodatavalue[0], poBand->GetRasterDataType()))
				vec[j] = NA_REAL; // #nocov
			else
				vec[j] = (vec[j] * scale) + offset;
		}
		// }
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
	if (lst.containsElementNamed(name) && lst[name] != R_NilValue) {
		IntegerVector ret = lst[name]; // #nocov
		return(ret[0]);                // #nocov
	} else
		return(otherwise);
}

NumericMatrix get_color_table(GDALColorTable *tbl) {
	int n = tbl->GetColorEntryCount();
	NumericMatrix t(n, 4);
	for (int i = 0; i < n; i++) {
		const GDALColorEntry *ce = tbl->GetColorEntry(i);
		t(i, 0) = ce->c1;
		t(i, 1) = ce->c2;
		t(i, 2) = ce->c3;
		t(i, 3) = ce->c4;
	}
	int i = (int) tbl->GetPaletteInterpretation();
	t.attr("interpretation") = IntegerVector::create(i);
	return t;
}

List get_cat(char **cat) {
	if (cat == NULL)
		return(List::create());

	int n = 0;
	for (n = 0; cat[n] != NULL; n++)
		; // n is number of categories

	List t(1);
	CharacterVector col(n);
	IntegerVector row_names(n);
	for (int i = 0; i < n; i++) {
		col(i) = cat[i];
		row_names(i) = i+1;
	}
	t(0) = col;
	t.attr("names") = CharacterVector::create("category");
	t.attr("row.names") = row_names;
	t.attr("class") = CharacterVector::create("data.frame");
	return t;
}

List get_rat(GDALRasterAttributeTable *tbl) {

	if (tbl == NULL)
		return(List::create());

	List t(tbl->GetColumnCount());
	List names(tbl->GetColumnCount());
	for (int i = 0; i < tbl->GetColumnCount(); i++) {
		switch (tbl->GetTypeOfCol(i)) {
			case GFT_Integer: {
					IntegerVector col(tbl->GetRowCount());
					for (int j = 0; j < tbl->GetRowCount(); j++)
						col(j) = tbl->GetValueAsInt(j, i);
					t(i) = col;
				}
				break;
			case GFT_Real: {
					NumericVector col(tbl->GetRowCount());
					for (int j = 0; j < tbl->GetRowCount(); j++)
						col(j) = tbl->GetValueAsDouble(j, i);
					t(i) = col;
				}
				break;
			case GFT_String: {
					CharacterVector col(tbl->GetRowCount());
					for (int j = 0; j < tbl->GetRowCount(); j++)
						col(j) = tbl->GetValueAsString(j, i);
					t(i) = col;
				}
				break;
			default:
				stop("unknown column type in raster attribute table");
		}
		names(i) = tbl->GetNameOfCol(i);
	}
	IntegerVector row_names(tbl->GetRowCount());
	for (int i = 0; i < tbl->GetRowCount(); i++)
		row_names(i) = i+1;
	t.attr("names") = names;
	t.attr("row.names") = row_names;
	t.attr("class") = CharacterVector::create("data.frame");
	return t;
}

// [[Rcpp::export]]
List CPL_read_gdal(CharacterVector fname, CharacterVector options, CharacterVector driver,
		bool read_data, NumericVector NA_value, List RasterIO_parameters, double max_cells) {
// reads and returns data set metadata, and adds data array if read_data is true, or less 
// than max_cells are to be read
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
	// return the default geotransform as per the
	// GetGeoTransform() doc in classGDALDataset
	NumericVector geotransform = NumericVector::create(
		err == CE_None ? adfGeoTransform[0] : 0,
		err == CE_None ? adfGeoTransform[1] : 1,
		err == CE_None ? adfGeoTransform[2] : 0,
		err == CE_None ? adfGeoTransform[3] : 0,
		err == CE_None ? adfGeoTransform[4] : 0,
		err == CE_None ? adfGeoTransform[5] : 1); // see https://github.com/r-spatial/sf/pull/1307
	int default_geotransform = 0;
	if (err != CE_None) {
		default_geotransform = 1;
	}
	bool geo_transform_set = (err == CE_None);

	// CRS, projection:
#if GDAL_VERSION_NUM >= 3000000
	const OGRSpatialReference *sr = poDataset->GetSpatialRef();
	// sr = handle_axis_order(sr); -- should be done by GDAL; xy
	Rcpp::List crs = create_crs(sr, true);
#else
	const char *wkt_gdal = poDataset->GetProjectionRef();
	CharacterVector wkt = NA_STRING;
	if (*wkt_gdal != '\0')
		wkt = CharacterVector::create(wkt_gdal);
#endif

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

	// bands:
	IntegerVector bands;
	if (RasterIO_parameters.containsElementNamed("bands"))
		bands = RasterIO_parameters["bands"]; // #nocov
	else {
		bands = IntegerVector(poDataset->GetRasterCount());
		for (int j = 0; j < bands.size(); j++)
			bands(j) = j + 1; // bands is 1-based
	}

	// get color table, attribute table, and min/max values:
	List colorTables(bands.size());
	List attributeTables(bands.size());
	CharacterVector descriptions(bands.size());
	NumericMatrix ranges(bands.size(), 4);
	IntegerMatrix blocksizes(bands.size(), 2);
	for (int i = 0; i < bands.size(); i++) {
		if ((poBand = poDataset->GetRasterBand(bands(i))) == NULL)
			stop("trying to read a band that is not present");
		const char *md = poBand->GetMetadataItem("BANDNAME", NULL);
		if (md == NULL)
			descriptions(i) = poBand->GetDescription();
		else
			descriptions(i) = md;

		if (poBand->GetColorTable() != NULL)
			colorTables(i) = get_color_table(poBand->GetColorTable());
		if (poBand->GetCategoryNames() != NULL)
			attributeTables(i) = get_cat(poBand->GetCategoryNames());
		else
			attributeTables(i) = get_rat(poBand->GetDefaultRAT());
		int set = 1;
		ranges(i, 0) = poBand->GetMinimum(&set);
		ranges(i, 1) = (double) set;
		ranges(i, 2) = poBand->GetMaximum(&set);
		ranges(i, 3) = (double) set;
		int nBlockXSize = 0;
		int nBlockYSize = 0;
		poBand->GetBlockSize(&nBlockXSize, &nBlockYSize);
		blocksizes(i, 0) = nBlockXSize;
		blocksizes(i, 1) = nBlockYSize;
	}

	// get metadata items:
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

	if (max_cells > 0) 
		read_data = (bands.size() * nBufXSize * nBufYSize) < max_cells;

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
#if GDAL_VERSION_NUM >= 3000000
		_["crs"] = crs,
#else
		_["proj_wkt"] = wkt,
#endif
		_["geotransform"] = geotransform,
		_["datatype"] =	poBand != NULL ?
			GDALGetDataTypeName(poBand->GetRasterDataType()) :
			CharacterVector::create(NA_STRING),
		_["sub"] = sub,
		_["meta"] = get_meta_data(poDataset, CharacterVector::create()),
		_["band_meta"] = get_band_meta_data(poDataset),
		_["attribute_tables"] = attributeTables,
		_["color_tables"] = colorTables,
		_["ranges"] = ranges,
		_["blocksizes"] = blocksizes,
		_["descriptions"] = descriptions,
		_["default_geotransform"] = default_geotransform,
		_["proxy"] = LogicalVector::create(!read_data)
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
void CPL_write_gdal(NumericMatrix x, CharacterVector fname, CharacterVector driver,
		CharacterVector options, CharacterVector Type, IntegerVector dims, IntegerVector from,
		NumericVector gt, CharacterVector p4s, NumericVector na_val, NumericVector scale_offset,
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
#if GDAL_VERSION_NUM >= 3070000
	else if (Type[0] == "Int8")
		eType = GDT_Int8; // #nocov
#endif
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
	if (scale_offset.size() != 2)
		stop("scale_offset should have length 2");
	bool set_scale_offset = (scale_offset[0] != 1.0 || scale_offset[1] != 0.0);

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
			stop("p4s should have length one"); // #nocov
		if (p4s[0] != NA_STRING) {
			OGRSpatialReference oSRS;
			oSRS.SetFromUserInput((const char *) p4s[0]); // handles wkt too
#if GDAL_VERSION_NUM >= 3000000
			poDstDS->SetSpatialRef(&oSRS);
#else
			char *pszSRS_WKT = NULL;
			oSRS.exportToWkt( &pszSRS_WKT );
			if (poDstDS->SetProjection( pszSRS_WKT ) != CE_None)
				stop("SetProjection: error"); // #nocov
			CPLFree( pszSRS_WKT );
#endif
		}

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
		// set scale_offset
		if (set_scale_offset) {
			for (int band = 1; band <= dims(2); band++) { // unlike x & y, band is 1-based
				GDALRasterBand *poBand = poDstDS->GetRasterBand( band );
				if (poBand->SetScale(scale_offset[0]) != CE_None ||
						poBand->SetOffset(scale_offset[1]) != CE_None)
					warning("writing scale and/or offset failed (not supported by driver)"); // #nocov
			}
		}
		// set descriptions:
		if (x.attr("descriptions") != R_NilValue) {
			Rcpp::CharacterVector descriptions = x.attr("descriptions");
			for (int band = 1; band <= dims(2); band++) {
				GDALRasterBand *poBand = poDstDS->GetRasterBand( band );
				poBand->SetDescription(descriptions(band-1));
			}
		}

		// write factor levels to CategoryNames:
		if (x.attr("levels") != R_NilValue) {
			Rcpp::CharacterVector levels = x.attr("levels");
			for (int band = 1; band <= dims(2); band++) {
				GDALRasterBand *poBand = poDstDS->GetRasterBand( band );
				if (poBand->SetCategoryNames(create_options(levels).data()) != CE_None)
					warning("error writing factor levels to raster band");
			}
		}
		// write color table:
		if (x.attr("rgba") != R_NilValue) {
			Rcpp::NumericMatrix co = x.attr("rgba"); // r g b alpha in columns; levels in rows
			GDALColorTable ct = GDALColorTable(GPI_RGB);
			GDALColorEntry ce;
			for (int i = 0; i < co.nrow(); i++) {
				ce.c1 = co(i, 0);
				ce.c2 = co(i, 1);
				ce.c3 = co(i, 2);
				ce.c4 = co(i, 3);
				ct.SetColorEntry(i, &ce);
			}
			GDALRasterBand *poBand = poDstDS->GetRasterBand( 1 ); // can only set CT for band 1
			if (poBand->SetColorTable(&ct) != CE_None)
				warning("error writing color table to raster band");
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
		if (set_scale_offset) {
			for (int i = 0; i < x.ncol(); i++) {
				NumericVector nv = x(_, i);
				for (int j = 0; j < x.nrow(); j++)
					nv[j] = (nv[j] - scale_offset[1]) / scale_offset[0]; // raw = (units - offset) / scale
				x(_, i) = nv;
			}
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

double get_bilinear(GDALRasterBand *poBand, double Pixel, double Line,
						int iPixel, int iLine, double RasterXSize, double RasterYSize,
						int na_set, double na_value) {

	double pixels[4];
	double dY  = Line - iLine; // [0, 1) over a raster cell
	double dX  = Pixel - iPixel; // [0, 1) over a raster cell
	if ((dY < 0.5 && iLine > 0) || (iLine == RasterYSize - 1)) // where to start reading
		iLine -= 1;
	if ((dX < 0.5 && iPixel > 0) || (iPixel == RasterXSize - 1))
		iPixel -= 1;

	// x:
	if (Pixel < 0.5) // border:
		dX = 0.0;
	else if (Pixel > RasterXSize - 0.5)
		dX = 1.0;
	else if (dX < 0.5) // shift to pixel center:
		dX += 0.5;
	else
		dX -= 0.5;

	// y:
	if (Line < 0.5)
		dY = 0.0;
	else if (Line > RasterYSize - 0.5)
		dY = 1.0;
	else if (dY < 0.5)
		dY += 0.5;
	else
		dY -= 0.5;

	// read:
	if (GDALRasterIO(poBand, GF_Read, iPixel, iLine, 2, 2,
			(void *) pixels, 2, 2, GDT_CFloat64, sizeof(double), 0) != CE_None)
		stop("Error reading!");
	// f(0,0): pixels[0], f(1,0): pixels[1], f(0,1): pixels[2], f(1,1): pixels[3]
	if (na_set && (pixels[0] == na_value || pixels[1] == na_value ||
			pixels[2] == na_value || pixels[3] == na_value))
		return na_value;
	else // https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_square
		return	pixels[0] * (1-dX) * (1-dY) +
				pixels[1] * dX     * (1-dY) +
				pixels[2] * (1-dX) * dY +
				pixels[3] * dX     * dY;
}

// [[Rcpp::export]]
NumericMatrix CPL_extract(CharacterVector input, NumericMatrix xy, bool interpolate = false) {
	// mostly taken from gdal/apps/gdallocationinfo.cpp

	GDALDataset *poDataset = (GDALDataset *) GDALOpenEx(input[0], GA_ReadOnly,
		NULL, NULL, NULL);
	if (poDataset == NULL) {
		Rcout << "trying to read file: " << input[0] << std::endl; // #nocov
		stop("file not found"); // #nocov
	}

	NumericMatrix ret(xy.nrow(), poDataset->GetRasterCount());
	int xsize = poDataset->GetRasterXSize();
	int ysize = poDataset->GetRasterYSize();

	double gt[6];
	poDataset->GetGeoTransform(gt);
	double gt_inv[6];
	// int retval = GDALInvGeoTransform(gt, gt_inv);
	if (! GDALInvGeoTransform(gt, gt_inv))
		stop("geotransform not invertible");

	for (int j = 0; j < poDataset->GetRasterCount(); j++) {
		GDALRasterBand *poBand = poDataset->GetRasterBand(j+1);
		int bSuccess;
		double dfOffset = poBand->GetOffset(&bSuccess);
		double dfScale  = poBand->GetScale(&bSuccess);
		double nodata = NA_REAL;
		int nodata_set = 0;
		poBand->GetNoDataValue(&nodata_set);
		if (nodata_set)
			nodata = poBand->GetNoDataValue(NULL);
		for (int i = 0; i < xy.nrow(); i++) {
			double dfGeoX = xy(i, 0);
			double dfGeoY = xy(i, 1);
			double Pixel = gt_inv[0] + gt_inv[1] * dfGeoX + gt_inv[2] * dfGeoY;
			double Line  = gt_inv[3] + gt_inv[4] * dfGeoX + gt_inv[5] * dfGeoY;
			int iPixel = static_cast<int>(floor( Pixel ));
			int iLine  = static_cast<int>(floor( Line ));
			double pixel;
			if (iPixel < 0 || iLine < 0 || iPixel >= xsize || iLine >= ysize) // outside bbox:
				pixel = NA_REAL;
			else { // read pixel:
				if (interpolate)
					// stop("interpolate not implemented");
					pixel = get_bilinear(poBand, Pixel, Line, iPixel, iLine,
						xsize, ysize, nodata_set, nodata);
				else if (GDALRasterIO(poBand, GF_Read, iPixel, iLine, 1, 1,
						&pixel, 1, 1, GDT_CFloat64, 0, 0) != CE_None)
					stop("Error reading!");
				if (nodata_set && pixel == nodata)
					pixel = NA_REAL;
				else if (dfOffset != 0.0 || dfScale != 1.0)
					pixel = pixel * dfScale + dfOffset;
			}
			ret(i, j) = pixel;
		}
	}
	GDALClose(poDataset);
	return ret;
}

// [[Rcpp::export]]
void CPL_create(CharacterVector file, IntegerVector nxy, NumericVector value, CharacterVector wkt, 
				NumericVector xlim, NumericVector ylim) {
//
// modified from gdal/apps/gdal_create.cpp:
//
	int nPixels = nxy[0], nLines = nxy[1];
	GDALDatasetH hDS = GDALCreate(GDALGetDriverByName("GTiff"),
                                   file[0], nPixels, nLines,
                                   1, GDT_Byte, NULL);
	OGRSpatialReference oSRS;
#if GDAL_VERSION_NUM >= 2050000
	oSRS.SetAxisMappingStrategy(OAMS_TRADITIONAL_GIS_ORDER);
#endif
	if (oSRS.SetFromUserInput( wkt[0] ) != OGRERR_NONE) {
		CPLError(CE_Failure, CPLE_AppDefined, "Failed to process SRS definition");
		stop("CPL_create failed");
	}
	char* pszSRS = nullptr;
	oSRS.exportToWkt( &pszSRS );
	if( GDALSetProjection(hDS, pszSRS) != CE_None ) {
		CPLFree(pszSRS);
		GDALClose(hDS);
		stop("CPL_create failed");
	}
	double adfGeoTransform[6];
	adfGeoTransform[0] = xlim[0];
	adfGeoTransform[1] = (xlim[1] - xlim[0]) / nPixels;
	adfGeoTransform[2] = 0;
	adfGeoTransform[3] = ylim[1];
	adfGeoTransform[4] = 0;
	adfGeoTransform[5] = (ylim[0] - ylim[1]) / nLines;
	GDALSetGeoTransform(hDS, adfGeoTransform);
	GDALFillRaster(GDALGetRasterBand(hDS, 1), value[0], 0);
	CPLFree(pszSRS);
	GDALClose(hDS);
}
