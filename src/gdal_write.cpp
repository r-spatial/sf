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
			// oField.SetWidth(32); // FIXME: should this be known here???
		else if (strcmp(cls[i], "integer") == 0)
			ret[i] = OFTInteger;
		else if (strcmp(cls[i], "numeric") == 0)
			ret[i] = OFTReal;
		else if (strcmp(cls[i], "Date") == 0)
			ret[i] = OFTDate;
		else if (strcmp(cls[i], "POSIXct") == 0)
			ret[i] = OFTDateTime;
		else {
			Rcpp::Rcout << "Field of type " << nm[i] << " not supported." << std::endl;
			throw std::invalid_argument("Layer creation failed.\n");
		}
		OGRFieldDefn oField(nm[i], ret[i]);
		if (poLayer->CreateField(&oField) != OGRERR_NONE) {
			Rcpp::Rcout << "Creating field " << nm[i] << " failed." << std::endl;
			throw std::invalid_argument("Layer creation failed.\n");
		}
	}
	return(ret);
}

// this is like an unlist -> dbl, but only does the first 6; if we'd do unlist on the POSIXlt
// object, we'd get a character vector...
Rcpp::NumericVector get_dbl6(Rcpp::List in) { 
	Rcpp::NumericVector ret(6);
	for (int i = 0; i < 6; i++) {
		Rcpp::NumericVector x = in(i);
		ret(i) = x(0);
	}
	return(ret);
}

void SetFields(OGRFeature *poFeature, std::vector<OGRFieldType> tp, Rcpp::List obj, size_t i = 0) {
	for (size_t j = 0; j < tp.size(); j++) {
		if (j == (size_t) poFeature->GetFieldCount())
			throw std::invalid_argument("Impossible: field count reached\n");
		switch (tp[j]) {
			case OFTString: {
				Rcpp::CharacterVector cv;
				cv = obj[j];
				poFeature->SetField(j, (const char *) cv[i]);
				} break;
			case OFTInteger: {
				Rcpp::IntegerVector iv;
				iv = obj[j];
				poFeature->SetField(j, (int) iv[i]);
				} break;
			case OFTReal: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				poFeature->SetField(j, (double) nv[i]);
				} break;
			case OFTDate: {
				Rcpp::NumericVector nv;
				nv = obj[j];
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
				Rcpp::NumericVector nv0(1);
				nv0[0] = nv[i];
				Rcpp::Function as_POSIXlt_POSIXct("as.POSIXlt.POSIXct");
				Rcpp::NumericVector rd = get_dbl6(as_POSIXlt_POSIXct(nv0)); // use R
				poFeature->SetField(j, 1900 + (int) rd[5], (int) rd[4], 
					(int) rd[3], (int) rd[2], (int) rd[1], 
					(float) rd[0], 100); // nTZFlag 100: GMT
				} break;
			default:
				// we should never get here!
				Rcpp::Rcout << "field with unsupported type ignored" << std::endl; 
				throw std::invalid_argument("Layer creation failed.\n");
				break;
		}
	}
}

OGRSpatialReference *ref_from_sfc(Rcpp::List sfc) {
	Rcpp::String p4s = sfc.attr("proj4string");
	OGRSpatialReference *sref = new OGRSpatialReference;
	if (p4s != NA_STRING) {
		Rcpp::CharacterVector p4s_cv = sfc.attr("proj4string");
		char *cp = p4s_cv[0];
		if (sref->importFromProj4(cp) != OGRERR_NONE || *cp == '\0') {
			sref->Release();
			throw std::invalid_argument("Object with invalid proj4string.\n");
		}
	}
	return(sref);
}

// [[Rcpp::export]]
void CPL_write_ogr(Rcpp::List obj, Rcpp::CharacterVector dsn, Rcpp::CharacterVector layer,
	Rcpp::CharacterVector driver, Rcpp::CharacterVector dco, Rcpp::CharacterVector lco,
	Rcpp::List geom, Rcpp::CharacterVector dim, bool quiet = false) {

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
		Rcpp::Rcout << "Writing layer " << layer[0] << " to data source " << dsn[0] <<
			" using driver " << driver << std::endl;

	// open data set:
	std::vector <char *> options = create_options(dco, quiet);
	GDALDataset *poDS = poDriver->Create(dsn[0], 0, 0, 0, GDT_Unknown, options.data());
	if (poDS == NULL) {
		Rcpp::Rcout << "Creation of dataset " <<  dsn[0] << " failed." << std::endl;
		throw std::invalid_argument("Creation failed.\n");
	}
	Rcpp::CharacterVector clsv = geom.attr("class");
	OGRwkbGeometryType wkbType = (OGRwkbGeometryType) make_type(clsv[0], dim[0], false, NULL, 0);

	// create layer:
	options = create_options(lco, quiet);
	OGRSpatialReference *sref = ref_from_sfc(geom); // breaks on errror
	OGRLayer *poLayer = poDS->CreateLayer(layer[0], sref, wkbType, options.data());
	if (poLayer == NULL)  {
		sref->Release();
		Rcpp::Rcout << "Creating layer " << layer[0]  <<  " failed." << std::endl;
		GDALClose(poDS);
		throw std::invalid_argument("Layer creation failed.\n");
	}

	// write feature attribute fields & geometries:
	std::vector<OGRFieldType> fieldTypes = SetupFields(poLayer, obj);
	std::vector<OGRGeometry *> geomv = ogr_from_sfc(geom, sref);
	sref->Release();
	if (! quiet) {
		Rcpp::Rcout << "features:       " << geomv.size() << std::endl;
		Rcpp::Rcout << "fields:         " << fieldTypes.size() << std::endl;
		Rcpp::Rcout << "geometry type:  " << geomv[0]->getGeometryName() << std::endl;
	}
	for (size_t i = 0; i < geomv.size(); i++) { // create all features & add to layer:
		OGRFeature *poFeature = OGRFeature::CreateFeature(poLayer->GetLayerDefn());
		SetFields(poFeature, fieldTypes, obj, i);
		poFeature->SetGeometryDirectly(geomv[i]);
		if (poLayer->CreateFeature(poFeature) != OGRERR_NONE) {
			Rcpp::Rcout << "Failed to create feature " << i << " in " << layer[0] << std::endl;
			GDALClose(poDS);
			throw std::invalid_argument("Layer creation failed.\n");
		}
		OGRFeature::DestroyFeature(poFeature); // deletes geom[i] as well
	}
	GDALClose(poDS);
}
