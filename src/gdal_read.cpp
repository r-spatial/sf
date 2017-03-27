#include <ogrsf_frmts.h>

#include "Rcpp.h"

// #include "wkb.h"
#include "gdal.h"

Rcpp::List allocate_out_list(OGRFeatureDefn *poFDefn, int n_features, bool int64_as_string) {

	int n = poFDefn->GetFieldCount() + poFDefn->GetGeomFieldCount();
	Rcpp::List out(n);
	Rcpp::CharacterVector names(n);
	for (int i = 0; i < poFDefn->GetFieldCount(); i++) {
		OGRFieldDefn *poFieldDefn = poFDefn->GetFieldDefn(i);
		switch (poFieldDefn->GetType()) {
			case OFTInteger:
				out[i] = Rcpp::IntegerVector(n_features);
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
			case OFTString:
				out[i] = Rcpp::CharacterVector(n_features);
				break;
			default:
				throw std::invalid_argument("Unrecognized field type\n"); // #nocov
				break;
		}
		names[i] = poFieldDefn->GetNameRef();
	}

	for (int i = 0; i < poFDefn->GetGeomFieldCount(); i++) {
		// get the geometry fields:
		OGRGeomFieldDefn *poGFDefn = poFDefn->GetGeomFieldDefn(i);
		if (poGFDefn == NULL)
			throw std::range_error("GeomFieldDefn error"); // #nocov
		std::string geom = "geometry";
		const char *geom_name = poGFDefn->GetNameRef();
		if (*geom_name == '\0') {
			if (i > 0)
				names[i + poFDefn->GetFieldCount()] = geom + std::to_string(i); // c++11
			else
				names[i + poFDefn->GetFieldCount()] = geom;
		} else
			names[i + poFDefn->GetFieldCount()] = geom_name;
		out[i + poFDefn->GetFieldCount()] = Rcpp::List(n_features); // ?
	}

	out.attr("names") = names;
	return out;
}

int to_multi_what(std::vector<OGRGeometry *> gv) {
	bool points = false, multipoints = false,
		lines = false, multilines = false, 
		polygons = false, multipolygons = false;

	for (unsigned int i = 0; i < gv.size(); i++) {
		switch(gv[i]->getGeometryType()) {
			case wkbPoint: points = true; break;
			case wkbMultiPoint: multipoints = true; break;
			case wkbLineString: lines = true; break;
			case wkbMultiLineString: multilines = true; break;
			case wkbPolygon: polygons = true; break;
			case wkbMultiPolygon: multipolygons = true; break;
			default: return 0;
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
			throw std::out_of_range("Cannot read layer with more than MAX_INT features"); // #nocov
	}
	poLayer->ResetReading ();
	return n;
}

// [[Rcpp::export]]
Rcpp::List CPL_get_layers(Rcpp::CharacterVector datasource, Rcpp::CharacterVector options, bool do_count = false) {

	if (datasource.size() != 1)
		throw std::invalid_argument("argument datasource should have length 1.\n"); // #nocov
	std::vector <char *> open_options = create_options(options, false);
	GDALDataset *poDS;
	poDS = (GDALDataset *) GDALOpenEx(datasource[0], GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, 
		open_options.data(), NULL);
	if (poDS == NULL) {
		Rcpp::Rcout << "Cannot open data source " << datasource[0] << std::endl;
		throw std::invalid_argument("Open failed.\n");
	}
	// template from ogrinfo.cpp:
	Rcpp::CharacterVector names(poDS->GetLayerCount());
	Rcpp::List geomtype(poDS->GetLayerCount());
	Rcpp::NumericVector field_count(poDS->GetLayerCount());
	Rcpp::NumericVector feature_count(poDS->GetLayerCount());

	for(int iLayer = 0; iLayer < poDS->GetLayerCount(); iLayer++) {
		OGRLayer *poLayer = poDS->GetLayer(iLayer);
		names(iLayer) = poLayer->GetName();
		int nGeomFieldCount = poLayer->GetLayerDefn()->GetGeomFieldCount();
		Rcpp::CharacterVector fieldtp(nGeomFieldCount);
		if( nGeomFieldCount > 1 ) {
			for(int iGeom = 0; iGeom < nGeomFieldCount; iGeom ++ ) {
				OGRGeomFieldDefn* poGFldDefn = poLayer->GetLayerDefn()->GetGeomFieldDefn(iGeom);
				fieldtp(iGeom) = OGRGeometryTypeToName(poGFldDefn->GetType());
			}
		} else if (poLayer->GetGeomType() != wkbUnknown)
			fieldtp(0) = OGRGeometryTypeToName(poLayer->GetGeomType());
		geomtype(iLayer) = fieldtp;
		OGRFeatureDefn *poFDefn = poLayer->GetLayerDefn();
		field_count(iLayer) = poFDefn->GetFieldCount();
		feature_count(iLayer) = poLayer->GetFeatureCount();
		if (feature_count(iLayer) < 0 && do_count)
			feature_count(iLayer) = count_features(poLayer);
	}

	Rcpp::List out(5);
	out(0) = names;
	out(1) = geomtype;
	out(2) = poDS->GetDriverName();
	out(3) = feature_count;
	out(4) = field_count;
	GDALClose(poDS); // close & destroys data source
	out.attr("names") = Rcpp::CharacterVector::create("name", "geomtype", "driver", "features", "fields");
	out.attr("class") = Rcpp::CharacterVector::create("sf_layers");
	return out;
}

// [[Rcpp::export]]
Rcpp::List CPL_read_ogr(Rcpp::CharacterVector datasource, Rcpp::CharacterVector layer, 
		Rcpp::CharacterVector options, bool quiet, Rcpp::NumericVector toTypeUser,
		bool promote_to_multi = true, bool int64_as_string = false) {
	// adapted from the OGR tutorial @ www.gdal.org
	std::vector <char *> open_options = create_options(options, quiet);
	GDALDataset *poDS;
	poDS = (GDALDataset *) GDALOpenEx( datasource[0], GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, 
		open_options.data(), NULL );
	if( poDS == NULL ) {
		Rcpp::Rcout << "Cannot open data source " << datasource[0] << std::endl;
		throw std::invalid_argument("Open failed.\n");
	}

	if (layer.size() == 0) { // no layer specified
		switch (poDS->GetLayerCount()) {
			case 0: { // error:
				throw std::invalid_argument("No layers in datasource.");
			}
			case 1: { // silent:
				OGRLayer *poLayer = poDS->GetLayer(0);
				layer = Rcpp::CharacterVector::create(poLayer->GetName());
				break;
			}
			default: { // select first layer: message + warning:
				OGRLayer *poLayer = poDS->GetLayer(0);
				layer = Rcpp::CharacterVector::create(poLayer->GetName());
				if (! quiet) {
					Rcpp::Rcout << "Multiple layers are present in data source " << datasource[0] << ", ";
					Rcpp::Rcout << "reading layer `" << layer[0] << "'." << std::endl;
					Rcpp::Rcout << "Use `st_layers' to list all layer names and their type in a data source." << std::endl;
					Rcpp::Rcout << "Set the `layer' argument in `st_read' to read a particular layer." << std::endl;
				}
				Rcpp::Function warning("warning");
				warning("automatically selected the first layer in a data source containing more than one.");
			}
		}
	}

	OGRLayer *poLayer = poDS->GetLayerByName(layer[0]);
	if (poLayer == NULL) {
		Rcpp::Rcout << "Cannot open layer " << layer[0] << std::endl;
		throw std::invalid_argument("Opening layer failed.\n");
	}

	double n_d = (double) poLayer->GetFeatureCount();
	if (n_d > INT_MAX)
		throw std::out_of_range("Cannot read layer with more than MAX_INT features"); // #nocov
	if (n_d < 0)
		n_d = (double) count_features(poLayer);
	size_t n = (size_t) n_d; // what is List's max length?

	std::vector<OGRFeature *> poFeatureV(n); // full archive

	if (! quiet)
		Rcpp::Rcout << "Reading layer `" << layer[0] << "' from data source `" << datasource[0] <<
			"' using driver `" << poDS->GetDriverName() << "'" << std::endl;

	OGRFeatureDefn *poFDefn = poLayer->GetLayerDefn();

	std::vector<OGRGeometry *> poGeometryV(n * poFDefn->GetGeomFieldCount());
	// cycles column wise: 2nd el is 1st geometry, 2nd feature

	Rcpp::List out = allocate_out_list(poFDefn, n, int64_as_string);

	// read all features:
	poLayer->ResetReading();
	unsigned int i = 0;
	double dbl_max_int64 = pow(2.0, 53);
	bool warn_int64 = false;
	OGRFeature *poFeature;
	while((poFeature = poLayer->GetNextFeature()) != NULL) {

		// feature attribute fields:
		for (int iField = 0; iField < poFDefn->GetFieldCount(); iField++ ) {
			OGRFieldDefn *poFieldDefn = poFDefn->GetFieldDefn( iField );
			switch(poFieldDefn->GetType()) {
				case OFTInteger: {
					Rcpp::IntegerVector iv;
					iv = out[iField];
					if (poFeature->IsFieldSet(iField))
						iv[i] = poFeature->GetFieldAsInteger(iField);
					else
						iv[i] = NA_INTEGER;
					}
					break;
				case OFTInteger64: {
					if (int64_as_string) {
						Rcpp::CharacterVector cv;
						cv = out[iField];
						if (poFeature->IsFieldSet(iField))
							cv[i] = poFeature->GetFieldAsString(iField);
						else
							cv[i] = NA_STRING;
					} else {
						Rcpp::NumericVector nv;
						nv = out[iField];
						if (poFeature->IsFieldSet(iField))
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
						(double) Hour, (double) Day, (double) Month, (double) Year - 1900, 
						0.0, 0.0, 0.0);
					dtlst.attr("class") = "POSIXlt";
					if (TZFlag == 100)
						dtlst.attr("tzone") = "UTC";
					Rcpp::NumericVector nv;
					nv = out[iField];
					if (! poFeature->IsFieldSet(iField)) {
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
					if (poFeature->IsFieldSet(iField))
						nv[i] = (double) poFeature->GetFieldAsDouble(iField);
					else
						nv[i] = NA_REAL;
					}
					break;
				default: // break through:
				case OFTString: {
					Rcpp::CharacterVector cv;
					cv = out[iField];
					if (poFeature->IsFieldSet(iField))
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
				throw std::invalid_argument("NULL pointer returned by GetGeomFieldRef"); // #nocov
		}

		poFeatureV[i] = poFeature;
		i++;
	} // all read...

	for (int iGeom = 0; iGeom < poFDefn->GetGeomFieldCount(); iGeom++ ) {
		std::vector<OGRGeometry *> poGeom(n);
		for (int i = 0; i < n; i++)
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

		if (toType != 0) { 
			// OGRGeomFieldDefn *poGFDefn = poFDefn->GetGeomFieldDefn(i);
			for (i = 0; i < poFeatureV.size(); i++) {
				OGRErr err = poFeatureV[i]->SetGeomFieldDirectly(
					iGeom,
					OGRGeometryFactory::forceTo(poFeatureV[i]->StealGeometry(iGeom), 
					(OGRwkbGeometryType) toType, NULL) );
				handle_error(err);
				poGeom[i] = poFeatureV[i]->GetGeomFieldRef(iGeom);
			}
		}
		if (! quiet && toTypeUser && n > 0)
			Rcpp::Rcout << "converted into: " << poGeometryV[0]->getGeometryName() << std::endl;
		// convert to R:
		Rcpp::List sfc = sfc_from_ogr(poGeom, false); // don't destroy
		out[iGeom + poFDefn->GetFieldCount()] = sfc;
	}

	if (warn_int64)
		Rcpp::Rcout << "Integer64 values larger than " << dbl_max_int64 << 
			" lost significance after conversion to double;" << std::endl <<
			"use argument int64_as_string = TRUE to import them lossless, as character" << std::endl;

	// clean up:
	for (size_t i = 0; i < n; i++)
		OGRFeature::DestroyFeature(poFeatureV[i]);
	GDALClose(poDS);

	return out;
}
