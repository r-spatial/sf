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
			Rcpp::stop("Layer creation failed.\n");
		}      // #nocov end
		OGRFieldDefn oField(nm[i], ret[i]);
		if (poLayer->CreateField(&oField) != OGRERR_NONE) { // #nocov start
			Rcpp::Rcout << "Creating field " << nm[i] << " failed." << std::endl;
			Rcpp::stop("Layer creation failed.\n");
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

void SetNull(OGRFeature *poFeature, size_t field) {
#if (GDAL_VERSION_MINOR >= 2 || GDAL_VERSION_MAJOR > 2)
	poFeature->SetFieldNull(field);
#else
	poFeature->UnsetField(field);
#endif
}

void SetFields(OGRFeature *poFeature, std::vector<OGRFieldType> tp, Rcpp::List obj, size_t i = 0) {
	Rcpp::CharacterVector nm  = obj.attr("names");
	for (size_t j = 0; j < tp.size(); j++) {
		if (j == (size_t) poFeature->GetFieldCount())
			Rcpp::stop("Impossible: field count reached\n"); // #nocov
		switch (tp[j]) {
			case OFTString: {
				Rcpp::CharacterVector cv;
				cv = obj[j];
				if (! Rcpp::CharacterVector::is_na(cv[i]))
					poFeature->SetField(nm[j], (const char *) cv[i]);
				else
					SetNull(poFeature, j);
				} break;
			case OFTInteger: {
				Rcpp::IntegerVector iv;
				iv = obj[j];
				if (! Rcpp::IntegerVector::is_na(iv[i]))
					poFeature->SetField(nm[j], (int) iv[i]);
				else
					SetNull(poFeature, j); // #nocov
				} break;
			case OFTReal: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (! Rcpp::NumericVector::is_na(nv[i]))
					poFeature->SetField(nm[j], (double) nv[i]);
				else
					SetNull(poFeature, j);
				} break;
			case OFTDate: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (Rcpp::NumericVector::is_na(nv[i])) {
					SetNull(poFeature, j);
					break;
				}
				Rcpp::NumericVector nv0(1);
				nv0[0] = nv[i];
				nv0.attr("class") = "Date";
				Rcpp::Function as_POSIXlt_Date("as.POSIXlt.Date");
				Rcpp::Function unlist("unlist");
				Rcpp::NumericVector ret = unlist(as_POSIXlt_Date(nv0)); // use R
				poFeature->SetField(nm[j], 1900 + (int) ret[5], (int) ret[4] + 1, (int) ret[3]);
				} break;
			case OFTDateTime: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (Rcpp::NumericVector::is_na(nv[i])) {
					SetNull(poFeature, j);
					break;
				}
				Rcpp::NumericVector nv0(1);
				nv0[0] = nv[i];
				nv0.attr("tzone") = "UTC";
				Rcpp::Function as_POSIXlt_POSIXct("as.POSIXlt.POSIXct");
				Rcpp::NumericVector rd = get_dbl6(as_POSIXlt_POSIXct(nv0)); // use R
				poFeature->SetField(nm[j], 1900 + (int) rd[5], (int) rd[4] + 1,
					(int) rd[3], (int) rd[2], (int) rd[1],
					(float) rd[0], 100); // nTZFlag 0: unkown; 1: local; 100: GMT
				} break;
			default:
				// we should never get here! // #nocov start
				Rcpp::Rcout << "field with unsupported type ignored" << std::endl; 
				Rcpp::stop("Layer creation failed.\n");
				break; // #nocov end
		}
	}
}

// [[Rcpp::export]]
void CPL_write_ogr(Rcpp::List obj, Rcpp::CharacterVector dsn, Rcpp::CharacterVector layer,
	Rcpp::CharacterVector driver, Rcpp::CharacterVector dco, Rcpp::CharacterVector lco,
	Rcpp::List geom, Rcpp::CharacterVector dim, bool quiet = false, bool update = false,
	bool delete_dsn = false, bool delete_layer = false) {

	// init:
	if (driver.size() != 1 || dsn.size() != 1 || layer.size() != 1)
		Rcpp::stop("argument dsn, layer or driver not of length 1.\n");

	/* GDALAllRegister(); -- has been done during .onLoad() */
	// get driver:
	GDALDriver *poDriver = GetGDALDriverManager()->GetDriverByName(driver[0]);
	if (poDriver == NULL) {
		Rcpp::Rcout << "driver `" << driver[0] << "' not available." << std::endl;
		Rcpp::stop("Driver not available.\n");
	}

	// delete data source:
	if (delete_dsn) {
		if (poDriver->Delete(dsn[0]) != CE_None) {
			if (! quiet)
				Rcpp::Rcout << "Deleting source `" << dsn[0] << "' failed" << std::endl;
		} else if (! quiet)
			Rcpp::Rcout << "Deleting source `" << dsn[0] << "' using driver `" << driver[0] << "'" << std::endl;
	}

	// data set:
	std::vector <char *> options = create_options(dco, quiet);
	GDALDataset *poDS; 

	// delete layer:
	if (delete_layer && (poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_UPDATE, NULL, 
				options.data(), NULL)) != NULL) { // don't complain if the layer is not present
		// find & delete layer:
		bool deleted = false;
		for (int iLayer = 0; iLayer < poDS->GetLayerCount(); iLayer++) {
			OGRLayer *poLayer = poDS->GetLayer(iLayer);
			if (poLayer != NULL && EQUAL(poLayer->GetName(), layer[0])) {
				OGRErr err = poDS->DeleteLayer(iLayer);
				if (! quiet) {
					if (err == OGRERR_UNSUPPORTED_OPERATION)
						Rcpp::Rcout << "Deleting layer not supported by driver `" << driver[0] << "'"  // #nocov
							<< std::endl; // #nocov
					else  {
						Rcpp::Rcout << "Deleting layer `" << layer[0] << "' using driver `" << 
							driver[0] << "'" << std::endl;
					}
				}
				deleted = (err == OGRERR_NONE);
				break;
			}
		}
		if (! deleted && ! quiet)
			Rcpp::Rcout << "Deleting layer `" << layer[0] << "' failed" << std::endl;
		GDALClose(poDS);
	}
	
	// update ds:
	if (update && (poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_UPDATE, NULL, 
				options.data(), NULL)) != NULL) {
		if (! quiet)
			Rcpp::Rcout << "Updating layer `" << layer[0] << "' to data source `" << dsn[0] <<
			"' using driver `" << driver[0] << "'" << std::endl;
	} else { // create new ds: 
		// error when it already exists:
		if ((poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, 
					options.data(), NULL)) != NULL) {
			GDALClose(poDS);
			Rcpp::Rcout << "Dataset " <<  dsn[0] << 
				" already exists: remove first, use update=TRUE to append," << std::endl <<  
				"delete_layer=TRUE to delete layer, or delete_dsn=TRUE to remove the entire data source before writing." 
				<< std::endl;
			Rcpp::stop("Dataset already exists.\n");
		}
		// create:
		if ((poDS = poDriver->Create(dsn[0], 0, 0, 0, GDT_Unknown, options.data())) == NULL) {
			Rcpp::Rcout << "Creating dataset " <<  dsn[0] << " failed." << std::endl;
			Rcpp::stop("Creation failed.\n");
		} else if (! quiet)
			Rcpp::Rcout << "Writing layer `" << layer[0] << "' to data source `" << dsn[0] <<
				"' using driver `" << driver[0] << "'" << std::endl;
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
		Rcpp::stop("Layer creation failed.\n");
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
			Rcpp::stop("Feature creation failed.\n");
		}
		OGRFeature::DestroyFeature(poFeature); // deletes geom[i] as well
	}
	GDALClose(poDS);
}
