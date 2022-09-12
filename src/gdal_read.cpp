#include <string>
#include <sstream>

#include <ogrsf_frmts.h>

#define RCPP_DEFAULT_INCLUDE_CALL false
#include "Rcpp.h"

#include "gdal_sf_pkg.h"
#include "gdal_read.h"

Rcpp::List allocate_out_list(OGRFeatureDefn *poFDefn, int n_features, bool int64_as_string,
		Rcpp::CharacterVector fid_column) {

	if (fid_column.size() > 1)
		Rcpp::stop("FID column name should be a length 1 character vector"); // #nocov

	int n = poFDefn->GetFieldCount() + poFDefn->GetGeomFieldCount() + fid_column.size();
	Rcpp::List out(n);
	Rcpp::CharacterVector names(n);
	for (int i = 0; i < poFDefn->GetFieldCount(); i++) {
		OGRFieldDefn *poFieldDefn = poFDefn->GetFieldDefn(i);
		switch (poFieldDefn->GetType()) {
			case OFTInteger: {
					if (poFieldDefn->GetSubType() == OFSTBoolean)
						out[i] = Rcpp::LogicalVector(n_features);
					else
						out[i] = Rcpp::IntegerVector(n_features);
				}
				break;
			case OFTDate: {
				Rcpp::NumericVector ret(n_features);
				ret.attr("class") = "Date";
				out[i] = ret;
				} break;
			case OFTDateTime: {
				Rcpp::NumericVector ret(n_features);
				Rcpp::CharacterVector cls(2);
				cls(0) = "POSIXct";
				cls(1) = "POSIXt";
				ret.attr("class") = cls;
				out[i] = ret;
				} break;
			case OFTInteger64: // fall through: converts Int64 -> double
				if (int64_as_string)
					out[i] = Rcpp::CharacterVector(n_features);
				else
					out[i] = Rcpp::NumericVector(n_features);
				break;
			case OFTReal:
				out[i] = Rcpp::NumericVector(n_features);
				break;
			case OFTStringList:
			case OFTRealList:
			case OFTIntegerList:
			case OFTInteger64List:
			case OFTBinary:
				out[i] = Rcpp::List(n_features);
				break;
			case OFTString:
			default:
				out[i] = Rcpp::CharacterVector(n_features);
				break;
		}
		names[i] = poFieldDefn->GetNameRef();
	}

	if (fid_column.size())
		names[ poFDefn->GetFieldCount() ] = fid_column[0];

	for (int i = 0; i < poFDefn->GetGeomFieldCount(); i++) {
		// get the geometry fields:
		OGRGeomFieldDefn *poGFDefn = poFDefn->GetGeomFieldDefn(i);
		if (poGFDefn == NULL)
			Rcpp::stop("GeomFieldDefn error"); // #nocov
		std::string geom = "geometry";
		const char *geom_name = poGFDefn->GetNameRef();
		if (*geom_name == '\0') {
			if (i > 0)
				names[i + poFDefn->GetFieldCount() + fid_column.size()] = geom + std::to_string(i); // c++11; #nocov
			else
				names[i + poFDefn->GetFieldCount() + fid_column.size()] = geom;
		} else
			names[i + poFDefn->GetFieldCount() + fid_column.size()] = geom_name;
		out[i + poFDefn->GetFieldCount() + fid_column.size()] = Rcpp::List(n_features); // ?
	}

	out.attr("names") = names;
	return out;
}

int to_multi_what(std::vector<OGRGeometry *> gv) {
	bool points = false, multipoints = false,
		lines = false, multilines = false,
		polygons = false, multipolygons = false;

	for (unsigned int i = 0; i < gv.size(); i++) {
		// drop Z and M:
		if (gv[i] == NULL)
			break;
		OGRwkbGeometryType gt = OGR_GT_SetModifier(gv[i]->getGeometryType(), 0, 0);
		switch(gt) {
			case wkbPoint: points = true; break;
			case wkbMultiPoint: multipoints = true; break;
			case wkbLineString: lines = true; break;
			case wkbMultiLineString: multilines = true; break;
			case wkbPolygon: polygons = true; break;
			case wkbMultiPolygon: multipolygons = true; break;
			default: return 0; // #nocov
		}
	}
	int sum = points + multipoints + lines + multilines + polygons + multipolygons;
	if (sum == 2) {
		if (points && multipoints)
			return wkbMultiPoint;
		if (lines && multilines)
			return wkbMultiLineString;
		if (!lines && !multilines)
			return wkbMultiPolygon;
	}
	// another mix or single type:
	return 0;
}

size_t count_features(OGRLayer *poLayer) {
	size_t n = 0;
	OGRFeature *poFeature;
	while((poFeature = poLayer->GetNextFeature()) != NULL) {
		n++;
		delete poFeature;
		if (n == INT_MAX)
			Rcpp::stop("Cannot read layer with more than MAX_INT features"); // #nocov
	}
	poLayer->ResetReading ();
	return n;
}

// [[Rcpp::export]]
Rcpp::List CPL_get_layers(Rcpp::CharacterVector datasource, Rcpp::CharacterVector options, bool do_count = false) {

	if (datasource.size() != 1)
		Rcpp::stop("argument datasource should have length 1.\n"); // #nocov
	std::vector <char *> open_options = create_options(options, false);
	GDALDataset *poDS;
	poDS = (GDALDataset *) GDALOpenEx(datasource[0], GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL,
		open_options.data(), NULL);
	if (poDS == NULL) {
		Rcpp::Rcout << "Cannot open data source " << datasource[0] << std::endl;
		Rcpp::stop("Open failed.\n");
	}
	// template from ogrinfo.cpp:
	Rcpp::CharacterVector names(poDS->GetLayerCount());
	Rcpp::List geomtype(poDS->GetLayerCount());
	Rcpp::NumericVector field_count(poDS->GetLayerCount());
	Rcpp::NumericVector feature_count(poDS->GetLayerCount());
	Rcpp::List layer_crs(poDS->GetLayerCount());

	for(int iLayer = 0; iLayer < poDS->GetLayerCount(); iLayer++) {
		OGRLayer *poLayer = poDS->GetLayer(iLayer);
		layer_crs[iLayer] = create_crs(poLayer->GetSpatialRef());
		names(iLayer) = poLayer->GetName();
		int nGeomFieldCount = poLayer->GetLayerDefn()->GetGeomFieldCount();
		if (nGeomFieldCount == 0) {
			Rcpp::CharacterVector fieldtp(1); // #nocov start ; though tested in #334
			fieldtp(0) = NA_STRING;
			geomtype(iLayer) = fieldtp;       // #nocov end
		} else {
			Rcpp::CharacterVector fieldtp(nGeomFieldCount);
			if( nGeomFieldCount > 1 ) {
				for(int iGeom = 0; iGeom < nGeomFieldCount; iGeom ++ ) {
					OGRGeomFieldDefn* poGFldDefn = poLayer->GetLayerDefn()->GetGeomFieldDefn(iGeom);
					fieldtp(iGeom) = OGRGeometryTypeToName(poGFldDefn->GetType());
				}
			} else if (poLayer->GetGeomType() != wkbUnknown)
				fieldtp(0) = OGRGeometryTypeToName(poLayer->GetGeomType());
			geomtype(iLayer) = fieldtp;
		}
		OGRFeatureDefn *poFDefn = poLayer->GetLayerDefn();
		field_count(iLayer) = poFDefn->GetFieldCount();
		feature_count(iLayer) = poLayer->GetFeatureCount();
		if (feature_count(iLayer) < 0 && do_count)
			feature_count(iLayer) = count_features(poLayer);
	}

	Rcpp::List out(6);
	out(0) = names;
	out(1) = geomtype;
	out(2) = poDS->GetDriverName();
	out(3) = feature_count;
	out(4) = field_count;
	out(5) = layer_crs;
	GDALClose(poDS); // close & destroys data source
	out.attr("names") = Rcpp::CharacterVector::create("name", "geomtype", "driver", "features", "fields", "crs");
	out.attr("class") = Rcpp::CharacterVector::create("sf_layers");
	return out;
}

Rcpp::List sf_from_ogrlayer(OGRLayer *poLayer, bool quiet, bool int64_as_string,
		Rcpp::NumericVector toTypeUser, Rcpp::CharacterVector fid_column, bool promote_to_multi = true) {

	double n_d = (double) poLayer->GetFeatureCount();
	if (n_d > INT_MAX)
		Rcpp::stop("Cannot read layer with more than MAX_INT features"); // #nocov
	if (n_d < 0)
		n_d = (double) count_features(poLayer);
	size_t n = (size_t) n_d; // what is List's max length?

	std::vector<OGRFeature *> poFeatureV(n); // full archive
	Rcpp::CharacterVector fids(n);

	OGRFeatureDefn *poFDefn = poLayer->GetLayerDefn();

	std::vector<OGRGeometry *> poGeometryV(n * poFDefn->GetGeomFieldCount());
	// cycles column wise: 2nd el is 1st geometry, 2nd feature

	Rcpp::List out = allocate_out_list(poFDefn, n, int64_as_string, fid_column);

	// read all features:
	poLayer->ResetReading();
	size_t i = 0; // feature counter
	double dbl_max_int64 = pow(2.0, 53);
	bool warn_int64 = false, has_null_geometries = false;
	OGRFeature *poFeature;
	while((poFeature = poLayer->GetNextFeature()) != NULL) {
		// getFID:
		fids[i] = std::to_string(poFeature->GetFID());

		// feature attribute fields:
		for (int iField = 0; iField < poFDefn->GetFieldCount(); iField++ ) {
			OGRFieldDefn *poFieldDefn = poFDefn->GetFieldDefn( iField );
#if (GDAL_VERSION_MINOR >= 2 || GDAL_VERSION_MAJOR > 2)
			int not_NA = poFeature->IsFieldSetAndNotNull(iField);
#else
			int not_NA = poFeature->IsFieldSet(iField);
#endif
			switch(poFieldDefn->GetType()) {
				case OFTInteger: {
					if (poFieldDefn->GetSubType() == OFSTBoolean) {
						Rcpp::LogicalVector lv;
						lv = out[iField];
						if (not_NA) {
							if (poFeature->GetFieldAsInteger(iField))
								lv[i] = true;
							else
								lv[i] = false;
						} else
							lv[i] = NA_LOGICAL;
					} else {
						Rcpp::IntegerVector iv;
						iv = out[iField];
						if (not_NA)
							iv[i] = poFeature->GetFieldAsInteger(iField);
						else
							iv[i] = NA_INTEGER;
					}
					}
					break;
				case OFTInteger64: {
					if (int64_as_string) {
						Rcpp::CharacterVector cv;
						cv = out[iField];
						if (not_NA)
							cv[i] = poFeature->GetFieldAsString(iField);
						else
							cv[i] = NA_STRING;
					} else {
						Rcpp::NumericVector nv;
						nv = out[iField];
						if (not_NA)
							nv[i] = (double) poFeature->GetFieldAsInteger64(iField);
						else
							nv[i] = NA_REAL;
						// OR: poFeature->GetFieldAsString(iField);
						if (nv[i] > dbl_max_int64)
							warn_int64 = true;
						}
					}
					break;
				case OFTDateTime:
				case OFTDate: {
					int Year, Month, Day, Hour, Minute, TZFlag;
					float Second;
					poFeature->GetFieldAsDateTime(iField, &Year, &Month, &Day, &Hour, &Minute,
						&Second, &TZFlag);
					//  POSIXlt: sec   min  hour  mday   mon  year  wday  yday isdst ...
					Rcpp::List dtlst =
						Rcpp::List::create((double) Second, (double) Minute,
						(double) Hour, (double) Day, (double) Month - 1, (double) Year - 1900,
						0.0, 0.0, 0.0);
					dtlst.attr("class") = "POSIXlt";
					if (TZFlag == 100)
						dtlst.attr("tzone") = "UTC";
					Rcpp::NumericVector nv;
					nv = out[iField];
					if (! not_NA) {
						nv[i] = NA_REAL;
						break;
					}
					if (poFieldDefn->GetType() == OFTDateTime) {
						Rcpp::Function as_POSIXct_POSIXlt("as.POSIXct.POSIXlt");
						Rcpp::NumericVector ret = as_POSIXct_POSIXlt(dtlst); // R help me!
						nv[i] = ret[0];
					} else {
						Rcpp::Function as_Date_POSIXlt("as.Date.POSIXlt");
						Rcpp::NumericVector ret = as_Date_POSIXlt(dtlst); // R help me!
						nv[i] = ret[0];
					}
					break;
					}
					break;
				case OFTReal: {
					Rcpp::NumericVector nv;
					nv = out[iField];
					if (not_NA)
						nv[i] = (double) poFeature->GetFieldAsDouble(iField);
					else
						nv[i] = NA_REAL;
					}
					break;
				case OFTStringList: {
					Rcpp::List lv;
					lv = out[iField];
					char **sl = poFeature->GetFieldAsStringList(iField);
					Rcpp::CharacterVector cv(CSLCount(sl));
					for (int j = 0; j < cv.size(); j++)
						cv[j] = sl[j];
					lv[i] = cv;
					}
					break;
				case OFTRealList: {
					// for all *List types, NA is handled by zero-length lists
					Rcpp::List lv; // #nocov start
					lv = out[iField];
					int n;
					const double *dl = poFeature->GetFieldAsDoubleList(iField, &n);
					Rcpp::NumericVector nv(n);
					for (int j = 0; j < nv.size(); j++)
						nv[j] = dl[j];
					lv[i] = nv;
					}
					break; // #nocov end
				case OFTIntegerList: {
					Rcpp::List lv;
					lv = out[iField];
					int n;
					const int *il = poFeature->GetFieldAsIntegerList(iField, &n);
					Rcpp::IntegerVector iv(n);
					for (int j = 0; j < iv.size(); j++)
						iv[j] = il[j];
					lv[i] = iv;
					}
					break;
				case OFTInteger64List: {
					Rcpp::List lv;
					lv = out[iField];
					int n;
					const GIntBig *int64list = poFeature->GetFieldAsInteger64List(iField, &n);
					if (int64_as_string) {
						Rcpp::CharacterVector cv(n);
						for (int j = 0; j < cv.size(); j++) {
							std::stringstream stream;
							stream << int64list[j];
							cv[j] = stream.str();
						}
						lv[i] = cv;
					} else {
						Rcpp::NumericVector nv(n);
						for (int j = 0; j < nv.size(); j++) {
							nv[j] = (double) int64list[j];
							if (nv[j] > dbl_max_int64)
									warn_int64 = true; // #nocov
						}
						lv[i] = nv;
					}
					}
					break;
				case OFTBinary: {
					Rcpp::List lv;
					lv = out[iField];
					int n;
					const GByte *bl = poFeature->GetFieldAsBinary(iField, &n);
					Rcpp::RawVector rv(n);
					for (int j = 0; j < rv.size(); j++)
						rv[j] = bl[j];
					lv[i] = rv;
					}
					break;
				default: // break through: anything else to be converted to string?
				case OFTString: {
					Rcpp::CharacterVector cv;
					cv = out[iField];
					if (not_NA)
						cv[i] = poFeature->GetFieldAsString(iField);
					else
						cv[i] = NA_STRING;
					}
				break;
			}
		}

		// feature geometry:
		for (int iGeom = 0; iGeom < poFDefn->GetGeomFieldCount(); iGeom++ ) {
			poGeometryV[i + n * iGeom] = poFeature->GetGeomFieldRef(iGeom);
			if (poGeometryV[i + n * iGeom] == NULL)
				has_null_geometries = true;
		}

		poFeatureV[i] = poFeature;
		i++;
	} // all read...

	// add feature IDs if needed:
	if (fid_column.size())
		out[ poFDefn->GetFieldCount() ] = fids;

	std::vector<OGRGeometry *> to_be_freed;
	for (int iGeom = 0; iGeom < poFDefn->GetGeomFieldCount(); iGeom++ ) {

		std::vector<OGRGeometry *> poGeom(n);
		for (i = 0; i < n; i++)
			poGeom[i] = poGeometryV[i + n * iGeom];

		int toType = 0, toTypeU = 0;
		if (toTypeUser.size() == poFDefn->GetGeomFieldCount())
			toTypeU = toTypeUser[iGeom];
		else
			toTypeU = toTypeUser[0];
		if (promote_to_multi && toTypeU == 0)
			toType = to_multi_what(poGeom);
		else
			toType = toTypeU;

		if (toType != 0) { // coerce to toType:
			// OGRGeomFieldDefn *poGFDefn = poFDefn->GetGeomFieldDefn(i);
			for (i = 0; i < poFeatureV.size(); i++) {
				OGRGeometry *geom = poFeatureV[i]->StealGeometry(iGeom); // transfer ownership
				if (geom == NULL)
					geom = OGRGeometryFactory::createGeometry((OGRwkbGeometryType) toType); // #nocov
				else if ((geom =
						OGRGeometryFactory::forceTo(geom, (OGRwkbGeometryType) toType, NULL))
						== NULL)
					Rcpp::stop("OGRGeometryFactory::forceTo returned NULL"); // #nocov
				handle_error(poFeatureV[i]->SetGeomFieldDirectly(iGeom, geom));
				poGeom[i] = poFeatureV[i]->GetGeomFieldRef(iGeom);
				if (poGeom[i] == NULL)
					Rcpp::stop("GetGeomFieldRef returned NULL"); // #nocov
			}
		} else if (has_null_geometries) {
			if (! quiet)
				Rcpp::Rcout << "replacing null geometries with empty geometries" << std::endl; // #nocov

			// replace null's with empty:
			OGRwkbGeometryType gt = wkbGeometryCollection;
			for (i = 0; i < poGeom.size(); i++) {
				if (poGeom[i] != NULL) {
					gt = poGeom[i]->getGeometryType(); // first non-NULL
					break;
				}
			}
			for (i = 0; i < poGeom.size(); i++) {
				if (poGeom[i] == NULL) {
					poGeom[i] = OGRGeometryFactory::createGeometry(gt);
					if (poGeom[i] == NULL)
						Rcpp::stop("createGeometry returned NULL"); // #nocov
					else
						to_be_freed.push_back(poGeom[i]);
				}
			}
		}
		if (! quiet && toTypeU != 0 && n > 0)
			Rcpp::Rcout << "converted into: " << poGeom[0]->getGeometryName() << std::endl; // #nocov
		// convert to R:
		Rcpp::List sfc = sfc_from_ogr(poGeom, false); // don't destroy
		OGRGeomFieldDefn *fdfn = poFDefn->GetGeomFieldDefn(iGeom);
		sfc.attr("crs") = create_crs(fdfn->GetSpatialRef()); // overwrite: see #449 for the reason why
		out[iGeom + poFDefn->GetFieldCount() + fid_column.size()] = sfc;
	}

	if (warn_int64)
		Rcpp::Rcout << "Integer64 values larger than " << dbl_max_int64 <<
			" lost significance after conversion to double;" << std::endl <<
			"use argument int64_as_string = TRUE to import them lossless, as character" << std::endl;

	// clean up:
	for (i = 0; i < n; i++)
		OGRFeature::DestroyFeature(poFeatureV[i]);
	for (i = 0; i < to_be_freed.size(); i++)
		OGRGeometryFactory::destroyGeometry(to_be_freed[i]);

	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_read_ogr(Rcpp::CharacterVector datasource, Rcpp::CharacterVector layer,
		Rcpp::CharacterVector query,
		Rcpp::CharacterVector options, bool quiet, Rcpp::NumericVector toTypeUser,
		Rcpp::CharacterVector fid_column_name, Rcpp::CharacterVector drivers,
		Rcpp::CharacterVector wkt_filter,
		bool promote_to_multi = true, bool int64_as_string = false,
		bool dsn_exists = true,
		bool dsn_isdb = false,
		int width = 80) {

	// adapted from the OGR tutorial @ www.gdal.org
	std::vector <char *> open_options = create_options(options, quiet);
	std::vector <char *> drivers_v = create_options(drivers, quiet);
	GDALDataset *poDS;
	poDS = (GDALDataset *) GDALOpenEx( datasource[0], GDAL_OF_VECTOR | GDAL_OF_READONLY, 
		drivers.size() ? drivers_v.data() : NULL, open_options.data(), NULL );
	if( poDS == NULL ) {
		// could not open dsn
		if( dsn_isdb ) {
			Rcpp::stop("Cannot open %s; Check connection parameters.", datasource);
		}
		if( dsn_exists ) {
			Rcpp::stop("Cannot open %s; %s %s",
              datasource,
              "The source could be corrupt or not supported.",
              "See `st_drivers()` for a list of supported formats.");
		}
		Rcpp::stop("Cannot open %s; The file doesn't seem to exist.", datasource);
	}

	if (layer.size() == 0 && Rcpp::CharacterVector::is_na(query[0])) { // no layer specified
		switch (poDS->GetLayerCount()) {
			case 0: { // error:
				Rcpp::stop("No layers in datasource.");
			}
			case 1: { // silent:
				OGRLayer *poLayer = poDS->GetLayer(0);
				layer = Rcpp::CharacterVector::create(poLayer->GetName());
				break;
			}
			default: { // select first layer: message + warning:
				OGRLayer *poLayer = poDS->GetLayer(0);
				layer = Rcpp::CharacterVector::create(poLayer->GetName());
				if (! quiet) { // #nocov start
					Rcpp::Rcout << "Multiple layers are present in data source " << datasource[0] << ", ";
					Rcpp::Rcout << "reading layer `" << layer[0] << "'." << std::endl;
					Rcpp::Rcout << "Use `st_layers' to list all layer names and their type in a data source." << std::endl;
					Rcpp::Rcout << "Set the `layer' argument in `st_read' to read a particular layer." << std::endl;
				} // #nocov end
				Rcpp::Function warning("warning");
				warning("automatically selected the first layer in a data source containing more than one.");
			}
		}
	}

	OGRLayer *poLayer;
	if (! Rcpp::CharacterVector::is_na(query[0])) {
		poLayer = poDS->ExecuteSQL(query[0], NULL, NULL);
		if (poLayer == NULL)
			Rcpp::stop("Query execution failed, cannot open layer.\n"); // #nocov
		if (layer.size())
			Rcpp::warning("argument layer is ignored when query is specified\n"); // #nocov
	} else
		poLayer = 	poDS->GetLayerByName(layer[0]);
	if (poLayer == NULL) {
		Rcpp::Rcout << "Cannot open layer " << layer[0] << std::endl;
		Rcpp::stop("Opening layer failed.\n");
	}

	// set spatial filter?
	if (wkt_filter.size()) {
		char *wkt = wkt_filter[0];
		OGRGeometry *new_geom;
#if GDAL_VERSION_MAJOR <= 2 && GDAL_VERSION_MINOR <= 2
		OGRErr err = OGRGeometryFactory::createFromWkt(&wkt, poLayer->GetSpatialRef(), &new_geom);
#else
		OGRErr err = OGRGeometryFactory::createFromWkt((const char *) wkt, poLayer->GetSpatialRef(), &new_geom);
#endif
		if (err != OGRERR_NONE) {
			Rcpp::Rcout << "Cannot create geometry from: " << wkt_filter[0] << std::endl;
			Rcpp::stop("wkt parse error.\n");
		}
		poLayer->SetSpatialFilter(new_geom);
		OGRGeometryFactory::destroyGeometry(new_geom);
	}

	if (! quiet) {
		if (! Rcpp::CharacterVector::is_na(query[0]))
			Rcpp::Rcout << "Reading query `" << query[0] << "'" << std::endl << "from data source ";
		else
			Rcpp::Rcout << "Reading layer `" << layer[0] << "' from data source ";
		// if (LENGTH(datasource[0]) > (width - (34 + LENGTH(layer[0]))))
		Rcpp::String ds(datasource(0));
		if (layer.size()) { 
			Rcpp::String la(layer(0));
			if (strlen(ds.get_cstring()) > (width - (34 + strlen(la.get_cstring()))))
				Rcpp::Rcout << std::endl << "  ";
		}
		Rcpp::Rcout << "`" << datasource[0] << "' ";
		if (((int) strlen(ds.get_cstring())) > (width - 25))
			Rcpp::Rcout << std::endl << "  ";
		Rcpp::Rcout << "using driver `" << poDS->GetDriverName() << "'" << std::endl;                       // #nocov
	}

	Rcpp::List out = sf_from_ogrlayer(poLayer, quiet, int64_as_string, toTypeUser, fid_column_name,
		promote_to_multi);

	// clean up if SQL was used https://www.gdal.org/classGDALDataset.html#ab2c2b105b8f76a279e6a53b9b4a182e0
	if (! Rcpp::CharacterVector::is_na(query[0]))
		poDS->ReleaseResultSet(poLayer);

	GDALClose(poDS);
	return out;
}
