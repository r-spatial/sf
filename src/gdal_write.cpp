#include <string.h>

#include "Rcpp.h"

#include "ogrsf_frmts.h"
#include "wkb.h"
#include "gdal.h"

std::vector<OGRFieldType> SetupFields(OGRLayer *poLayer, Rcpp::List obj) {
	std::vector<OGRFieldType> ret(obj.size());
	Rcpp::CharacterVector cls = obj.attr("colclasses");
	Rcpp::CharacterVector nm  = obj.attr("names");
	for (int i = 0; i < obj.size(); i++) {
		if (strcmp(cls[i], "character") == 0)
			ret[i] = OFTString;
		else if (strcmp(cls[i], "integer") == 0)
			ret[i] = OFTInteger;
		else if (strcmp(cls[i], "numeric") == 0)
			ret[i] = OFTReal;
		else if (strcmp(cls[i], "Date") == 0)
			ret[i] = OFTDate;
		else if (strcmp(cls[i], "POSIXct") == 0)
			ret[i] = OFTDateTime;
		else { // #nocov start
			Rcpp::Rcout << "Field of type " << nm[i] << " not supported." << std::endl;
			throw std::invalid_argument("Layer creation failed.\n");
		}      // #nocov end
		OGRFieldDefn oField(nm[i], ret[i]);
		if (poLayer->CreateField(&oField) != OGRERR_NONE) { // #nocov start
			Rcpp::Rcout << "Creating field " << nm[i] << " failed." << std::endl;
			throw std::invalid_argument("Layer creation failed.\n");
		} // #nocov end
	}
	return ret;
}

// this is like an unlist -> dbl, but only does the first 6; if we'd do unlist on the POSIXlt
// object, we'd get a character vector...
Rcpp::NumericVector get_dbl6(Rcpp::List in) { 
	Rcpp::NumericVector ret(6);
	for (int i = 0; i < 6; i++) {
		Rcpp::NumericVector x = in(i);
		ret(i) = x(0);
	}
	return ret;
}

void SetFields(OGRFeature *poFeature, std::vector<OGRFieldType> tp, Rcpp::List obj, size_t i = 0) {
	for (size_t j = 0; j < tp.size(); j++) {
		if (j == (size_t) poFeature->GetFieldCount())
			throw std::invalid_argument("Impossible: field count reached\n");
		switch (tp[j]) {
			case OFTString: {
				Rcpp::CharacterVector cv;
				cv = obj[j];
				if (! Rcpp::CharacterVector::is_na(cv[i]))
					poFeature->SetField(j, (const char *) cv[i]);
				} break;
			case OFTInteger: {
				Rcpp::IntegerVector iv;
				iv = obj[j];
				if (! Rcpp::IntegerVector::is_na(iv[i]))
					poFeature->SetField(j, (int) iv[i]);
				} break;
			case OFTReal: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (! Rcpp::NumericVector::is_na(nv[i]))
					poFeature->SetField(j, (double) nv[i]);
				} break;
			case OFTDate: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (Rcpp::NumericVector::is_na(nv[i]))
					break;
				Rcpp::NumericVector nv0(1);
				nv0[0] = nv[i];
				nv0.attr("class") = "Date";
				Rcpp::Function as_POSIXlt_Date("as.POSIXlt.Date");
				Rcpp::Function unlist("unlist");
				Rcpp::NumericVector ret = unlist(as_POSIXlt_Date(nv0)); // use R
				poFeature->SetField(j, 1900 + (int) ret[5], (int) ret[4], (int) ret[3]);
				} break;
			case OFTDateTime: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (Rcpp::NumericVector::is_na(nv[i]))
					break;
				Rcpp::NumericVector nv0(1);
				nv0[0] = nv[i];
				nv0.attr("tzone") = "UTC";
				Rcpp::Function as_POSIXlt_POSIXct("as.POSIXlt.POSIXct");
				Rcpp::NumericVector rd = get_dbl6(as_POSIXlt_POSIXct(nv0)); // use R
				poFeature->SetField(j, 1900 + (int) rd[5], (int) rd[4], 
					(int) rd[3], (int) rd[2], (int) rd[1], 
					(float) rd[0], 100); // nTZFlag 0: unkown; 1: local; 100: GMT
				} break;
			default:
				// we should never get here! // #nocov start
				Rcpp::Rcout << "field with unsupported type ignored" << std::endl; 
				throw std::invalid_argument("Layer creation failed.\n");
				break; // #nocov end
		}
	}
}

// [[Rcpp::export]]
void CPL_write_ogr(Rcpp::List obj, Rcpp::CharacterVector dsn, Rcpp::CharacterVector layer,
	Rcpp::CharacterVector driver, Rcpp::CharacterVector dco, Rcpp::CharacterVector lco,
	Rcpp::List geom, Rcpp::CharacterVector dim, bool quiet = false, bool update = false) {

	// init:
	if (driver.size() != 1 || dsn.size() != 1 || layer.size() != 1) {
		Rcpp::Rcout << "driver, dsn or layer unspecified" << std::endl;
		throw std::invalid_argument("Driver unspecified.\n");
	}

	/* GDALAllRegister(); -- has been done during .onLoad() */
	// get driver:
	GDALDriver *poDriver = GetGDALDriverManager()->GetDriverByName(driver[0]);
	if (poDriver == NULL) {
		Rcpp::Rcout << driver[0] << " driver not available." << std::endl;
		throw std::invalid_argument("Driver not available.\n");
	}  else if (! quiet)
		Rcpp::Rcout << "Writing layer `" << layer[0] << "' to data source `" << dsn[0] <<
			"' using driver `" << driver[0] << "'" << std::endl;

	// open data set:
	std::vector <char *> options = create_options(dco, quiet);
	GDALDataset *poDS; 
	if (!update && (poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, 
			options.data(), NULL)) != NULL) {
		GDALClose(poDS);
		Rcpp::Rcout << "Dataset " <<  dsn[0] << 
			" already exists; remove first, or use update=TRUE to append." << std::endl;
		throw std::invalid_argument("Dataset already exists.\n");
	}

	if (update && (poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_UPDATE, NULL, 
			options.data(), NULL)) != NULL)
		Rcpp::Rcout << "Updating " <<  dsn[0] << std::endl;
	else if ((poDS = poDriver->Create(dsn[0], 0, 0, 0, GDT_Unknown, options.data())) == NULL) {
		Rcpp::Rcout << "Creating dataset " <<  dsn[0] << " failed." << std::endl;
		throw std::invalid_argument("Creation failed.\n");
	}
	Rcpp::CharacterVector clsv = geom.attr("class");
	OGRwkbGeometryType wkbType = (OGRwkbGeometryType) make_type(clsv[0], dim[0], false, NULL, 0);
	// read geometries:
	OGRSpatialReference *sref;
	std::vector<OGRGeometry *> geomv = ogr_from_sfc(geom, &sref);

	// create layer:
	options = create_options(lco, quiet);
	OGRLayer *poLayer = poDS->CreateLayer(layer[0], sref, wkbType, options.data());
	if (sref != NULL)
		sref->Release();
	if (poLayer == NULL)  {
		Rcpp::Rcout << "Creating layer " << layer[0]  <<  " failed." << std::endl;
		GDALClose(poDS);
		throw std::invalid_argument("Layer creation failed.\n");
	}

	// write feature attribute fields & geometries:
	std::vector<OGRFieldType> fieldTypes = SetupFields(poLayer, obj);
	if (! quiet) {
		Rcpp::Rcout << "features:       " << geomv.size() << std::endl;
		Rcpp::Rcout << "fields:         " << fieldTypes.size() << std::endl;
		Rcpp::Rcout << "geometry type:  " << OGRGeometryTypeToName(wkbType) << std::endl;
	}
	for (size_t i = 0; i < geomv.size(); i++) { // create all features & add to layer:
		OGRFeature *poFeature = OGRFeature::CreateFeature(poLayer->GetLayerDefn());
		SetFields(poFeature, fieldTypes, obj, i);
		poFeature->SetGeometryDirectly(geomv[i]);
		if (poLayer->CreateFeature(poFeature) != OGRERR_NONE) {
			Rcpp::Rcout << "Failed to create feature " << i << " in " << layer[0] << std::endl;
			GDALClose(poDS);
			throw std::invalid_argument("Feature creation failed.\n");
		}
		OGRFeature::DestroyFeature(poFeature); // deletes geom[i] as well
	}
	GDALClose(poDS);
}
