#include <string.h>
#include <stdlib.h> /* malloc! */

#include "Rcpp.h"

#include "ogrsf_frmts.h"
#include "wkb.h"
#include "gdal.h"

std::vector<OGRFieldType> SetupFields(OGRLayer *poLayer, Rcpp::List obj) {
	std::vector<OGRFieldType> ret(obj.size());
	Rcpp::CharacterVector cls = obj.attr("colclasses");
	Rcpp::CharacterVector nm  = obj.attr("names");
	for (int i = 0; i < obj.size(); i++) {
		if (strcmp(cls[i], "character") == 0) {
			ret[i] = OFTString;
    		// oField.SetWidth(32); // FIXME: should be known here???
		} else if (strcmp(cls[i], "integer") == 0) {
			ret[i] = OFTInteger;
		} else if (strcmp(cls[i], "numeric") == 0) {
			ret[i] = OFTReal;
		} else {
        	Rcpp::Rcout << "Field of type " << nm[i] << " ignored." << std::endl;
		}
		// FIXME: do Date -> OFTDate, POSIXct -> OFTDateTime
    	OGRFieldDefn oField(nm[i], ret[i]);
    	if( poLayer->CreateField( &oField ) != OGRERR_NONE ) {
        	Rcpp::Rcout << "Creating field " << nm[i] << " failed." << std::endl;
			throw std::invalid_argument("Layer creation failed.\n");
    	}
	}
	return(ret);
}

void SetFields(OGRFeature *poFeature, std::vector<OGRFieldType> tp, Rcpp::List obj, size_t i = 0) {
	for (size_t j = 0; j < tp.size(); j++) {
		if (j == poFeature->GetFieldCount())
			throw std::invalid_argument("Impossible: field count reached\n");
		switch (tp[j]) {
			case OFTString: {
				Rcpp::CharacterVector cv;
				cv = obj[j];
				poFeature->SetField( j, (const char *) cv[i] );
				} break;
			case OFTInteger: {
				Rcpp::IntegerVector iv;
				iv = obj[j];
				poFeature->SetField( j, (int) iv[i] );
				} break;
			case OFTReal: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				poFeature->SetField( j, (double) nv[i] );
				} break;
			default:
				// FIXME: we shouldn't get here!
				Rcpp::Rcout << "field with unsupported type ignored" << std::endl; 
				break;
		}
	}
}

OGRSpatialReference *ref_from_p4s(Rcpp::List sfc) {
	Rcpp::String p4s = sfc.attr("proj4string");
	OGRSpatialReference *sref = new OGRSpatialReference;
	if (p4s != NA_STRING) {
		Rcpp::CharacterVector p4s_cv = sfc.attr("proj4string");
		char *cp = p4s_cv[0];
		if (sref->importFromProj4(cp) != OGRERR_NONE) {
			sref->Release();
			throw std::invalid_argument("Object with invalid proj4string.\n");
		}
	}
	return(sref);
}

char **layer_creation_options(Rcpp::CharacterVector lco, bool quiet = false) {
	if (lco.size() == 0)
		return(NULL);
	char **ret = (char **) malloc((1 + lco.size()) * sizeof(char *)); // how can I get this garbage collected?
	if (! quiet)
		Rcpp::Rcout << "options:         ";
	int i;
	for (i = 0; i < lco.size(); i++) {
		ret[i] = (char *) (lco[i]);
		if (! quiet)
			Rcpp::Rcout << ret[i] << " ";
	}
	ret[i] = NULL;
	if (! quiet)
		Rcpp::Rcout << std::endl;
	return(ret);
}

// [[Rcpp::export]]
void CPL_write_ogr(Rcpp::List obj, Rcpp::CharacterVector dsn, Rcpp::CharacterVector layer,
	Rcpp::CharacterVector driver, Rcpp::CharacterVector lco,
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
    GDALDataset *poDS = poDriver->Create( dsn[0], 0, 0, 0, GDT_Unknown, NULL );
    if (poDS == NULL) {
        Rcpp::Rcout << "Creation of dataset " <<  dsn[0] << " failed." << std::endl;
		throw std::invalid_argument("Creation failed.\n");
    }
	Rcpp::CharacterVector clsv = geom.attr("class");
	OGRwkbGeometryType wkbType = (OGRwkbGeometryType) make_type(clsv[0], dim[0], false, NULL, 0);

	char **papszOptions = layer_creation_options(lco, quiet);
	// create layer:
	OGRSpatialReference *sref = ref_from_p4s(geom); // breaks on errror
    OGRLayer *poLayer = poDS->CreateLayer( layer[0], sref, wkbType, papszOptions );
    if (poLayer == NULL)  {
        Rcpp::Rcout << "Creating layer " << layer[0]  <<  " failed." << std::endl;
    	GDALClose( poDS );
		if (papszOptions != NULL)
			free(papszOptions);
		throw std::invalid_argument("Layer creation failed.\n");
    }
	if (papszOptions != NULL)
		free(papszOptions);

	// write feature attribute fields & geometries:
	std::vector<OGRFieldType> fieldTypes = SetupFields(poLayer, obj);
	std::vector<OGRGeometry *> geomv = ogr_geometries_from_sfc(geom, sref);
	sref->Release();
	if (! quiet) {
		Rcpp::Rcout << "features:       " << geomv.size() << std::endl;
		Rcpp::Rcout << "fields:         " << fieldTypes.size() << std::endl;
	//  Rcpp::Rcout << "geometry type:  " << OGRGeometryTypeToName(wkbType) << std::endl;
		Rcpp::Rcout << "geometry type:  " << geomv[0]->getGeometryName() << std::endl;
	}
	for (size_t i = 0; i < geomv.size(); i++) { // create all features & add to layer:
        OGRFeature *poFeature = OGRFeature::CreateFeature( poLayer->GetLayerDefn() );
		SetFields(poFeature, fieldTypes, obj, i);
		poFeature->SetGeometryDirectly(geomv[i]);
        if( poLayer->CreateFeature( poFeature ) != OGRERR_NONE ) {
            Rcpp::Rcout << "Failed to create feature " << i << " in " << layer[0] << std::endl;
    		GDALClose( poDS );
			throw std::invalid_argument("Layer creation failed.\n");
        }
        OGRFeature::DestroyFeature( poFeature ); // deletes geom[i] as well
    }
    GDALClose( poDS );
}
