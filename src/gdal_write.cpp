#include <string.h>

#include "Rcpp.h"

#include "ogrsf_frmts.h"
#include "wkb.h"
#include "gdal.h"
#include "gdal_sf_pkg.h"

std::vector<OGRFieldType> SetupFields(OGRLayer *poLayer, Rcpp::List obj, bool update_layer) {
	std::vector<OGRFieldType> ret(obj.size());
	Rcpp::CharacterVector cls = obj.attr("colclasses");
	Rcpp::CharacterVector nm  = obj.attr("names");
	for (int i = 0; i < obj.size(); i++) {
		if (strcmp(cls[i], "character") == 0)
			ret[i] = OFTString;
		else if (strcmp(cls[i], "integer") == 0 || strcmp(cls[i], "logical") == 0)
			ret[i] = OFTInteger;
		else if (strcmp(cls[i], "numeric") == 0)
			ret[i] = OFTReal;
		else if (strcmp(cls[i], "Date") == 0)
			ret[i] = OFTDate;
		else if (strcmp(cls[i], "POSIXct") == 0)
			ret[i] = OFTDateTime;
		else if (strcmp(cls[i], "list") == 0) // list with raw vectors; #1721
			ret[i] = OFTBinary;
		else { // #nocov start
			Rcpp::Rcout << "Field " << nm[i] << " of type " << cls[i] << " not supported." << std::endl;
			Rcpp::stop("Layer creation failed.\n");
		}      // #nocov end
		if (poLayer->FindFieldIndex(nm[i], TRUE) == -1) { // not already present:
			OGRFieldDefn oField(nm[i], ret[i]);
			if (strcmp(cls[i], "logical") == 0)
				oField.SetSubType(OFSTBoolean);
			if (!update_layer && poLayer->CreateField(&oField) != OGRERR_NONE) { // #nocov start
				Rcpp::Rcout << "Creating field " << nm[i] << " failed." << std::endl;
				Rcpp::stop("Layer creation failed.\n");
			} // #nocov end
		} else {
			// otherwise, one could check the type?
			// some type conversion, e.g. int -> char seems to be done by GDAL, see #2202
		}
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
#if GDAL_VERSION_NUM >= 2020000
	poFeature->SetFieldNull(field);
#else
	poFeature->UnsetField(field);
#endif
}

std::vector<int> GetFieldIndex(OGRLayer *poLayer, Rcpp::List obj) {
	std::vector<int> ret(obj.size());
	Rcpp::CharacterVector nm  = obj.attr("names");
	for (int i = 0; i < obj.size(); i++) {
		ret[i] = poLayer->FindFieldIndex(nm[i], TRUE);
		if (ret[i] == -1) {
			Rcpp::Rcout << "Unknown field name `" << nm[i] << 
				"': updating a layer with improper field name(s)?" << std::endl;
			Rcpp::stop("Write error\n");
		}
	}
	return ret;
}

void SetFields(OGRFeature *poFeature, std::vector<OGRFieldType> tp, Rcpp::List obj, size_t i, std::vector<int> fld) {
	for (size_t j = 0; j < tp.size(); j++) {
		if (j >= (size_t) poFeature->GetFieldCount())
			Rcpp::stop("Field count reached: duplicate names present?\n"); // #nocov
		switch (tp[j]) {
			case OFTString: {
				Rcpp::CharacterVector cv;
				cv = obj[j];
				if (! Rcpp::CharacterVector::is_na(cv[i]))
					poFeature->SetField(fld[j], (const char *) cv[i]);
				else
					SetNull(poFeature, fld[j]);
				} break;
			case OFTInteger: {
				const OGRFieldDefn *def = poFeature->GetFieldDefnRef(j);
				if (def->GetSubType() == OFSTBoolean) {
					Rcpp::LogicalVector lv;
					lv = obj[j];
					if (! Rcpp::LogicalVector::is_na(lv[i]))
						poFeature->SetField(fld[j], (int) lv[i]);
					else
						SetNull(poFeature, fld[j]); // #nocov
				} else { // integer:
					Rcpp::IntegerVector iv;
					iv = obj[j];
					if (! Rcpp::IntegerVector::is_na(iv[i]))
						poFeature->SetField(fld[j], (int) iv[i]);
					else
						SetNull(poFeature, fld[j]); // #nocov
				}
				} break;
			case OFTReal: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (! Rcpp::NumericVector::is_na(nv[i]))
					poFeature->SetField(fld[j], (double) nv[i]);
				else
					SetNull(poFeature, fld[j]);
				} break;
			case OFTDate: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (Rcpp::NumericVector::is_na(nv[i])) {
					SetNull(poFeature, fld[j]);
					break;
				}
				Rcpp::NumericVector nv0(1);
				nv0[0] = nv[i];
				nv0.attr("class") = "Date";
				Rcpp::Function as_POSIXlt_Date("as.POSIXlt.Date");
				Rcpp::NumericVector ret = get_dbl6(as_POSIXlt_Date(nv0));
				poFeature->SetField(fld[j], 1900 + (int) ret[5], (int) ret[4] + 1, (int) ret[3]);
				} break;
			case OFTDateTime: {
				Rcpp::NumericVector nv;
				nv = obj[j];
				if (Rcpp::NumericVector::is_na(nv[i])) {
					SetNull(poFeature, fld[j]);
					break;
				}
				Rcpp::NumericVector nv0(1);
				nv0[0] = nv[i];
				nv0.attr("tzone") = "UTC";
				Rcpp::Function as_POSIXlt_POSIXct("as.POSIXlt.POSIXct");
				Rcpp::NumericVector rd = get_dbl6(as_POSIXlt_POSIXct(nv0)); // use R
				poFeature->SetField(fld[j], 1900 + (int) rd[5], (int) rd[4] + 1, // #nocov start
						(int) rd[3], (int) rd[2], (int) rd[1],
						(float) rd[0], 100); // nTZFlag: 0=unkown, 1=local, 100=GMT; #nocov end
				} break;
			case OFTBinary: 
#if GDAL_VERSION_NUM > 3000000
				{
				Rcpp::List lv;
				lv = obj[j];
				Rcpp::RawVector rv;
				rv = lv(0);
				if (rv.size() == 0)
					SetNull(poFeature, fld[j]); // #nocov
				else {
					const void *ptr = &(rv[0]);
					int size = rv.size();
					poFeature->SetField(fld[j], size, ptr);
				}
				} break;
#endif
			default:
				// we should never get here! // #nocov start
				Rcpp::Rcout << "field with unsupported type ignored" << std::endl; 
				Rcpp::stop("Layer creation failed.\n");
				break; // #nocov end
		}
	}
}

// [[Rcpp::export(rng=false)]]
int CPL_write_ogr(Rcpp::List obj, Rcpp::CharacterVector dsn, Rcpp::CharacterVector layer,
	Rcpp::CharacterVector driver, Rcpp::CharacterVector dco, Rcpp::CharacterVector lco,
	Rcpp::List geom, Rcpp::CharacterVector dim, Rcpp::CharacterVector fids,
	Rcpp::CharacterVector ConfigOptions,
	bool quiet, Rcpp::LogicalVector append, bool delete_dsn = false, bool delete_layer = false,
	bool write_geometries = true, int width = 80) {

	// init:
	set_config_options(ConfigOptions);
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

	std::vector <char *> options = create_options(dco, quiet);
	std::vector <char *> drivers = create_options(driver, true);

	// data set:
	GDALDataset *poDS; 

	// delete layer:
	if (delete_layer && (poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_UPDATE, 
				drivers.data(), options.data(), NULL)) != NULL) { // don't complain if the layer is not present
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
	
	// append to ds:
	if (append[0] == TRUE) { // and not NA_LOGICAL:
		poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_UPDATE, 
				drivers.data(), options.data(), NULL);
		if (poDS == NULL) {
			if ((poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, 
						options.data(), NULL)) != NULL) { // exists read-only:
				GDALClose(poDS);
				Rcpp::Rcout << "Cannot append to " <<  dsn[0] << 
					": do you have write permission?" << std::endl;
				Rcpp::stop("Cannot append to existing dataset.\n");
			} else { // doesn't exist: create
				if ((poDS = poDriver->Create(dsn[0], 0, 0, 0, GDT_Unknown, options.data())) == NULL) {
					Rcpp::Rcout << "Creating dataset " <<  dsn[0] << " failed." << std::endl;
					Rcpp::stop("Creation failed.\n");
				}
			}
		}
		if (! quiet)
			Rcpp::Rcout << "Updating layer `" << layer[0] << "' to data source `" << dsn[0] <<
			"' using driver `" << driver[0] << "'" << std::endl;
	} else { // add to existing ds or create new ds: 
		// when append == NA, raise error when it already exists:
		if (!delete_dsn && !delete_layer && append[0] == NA_LOGICAL && 
				(poDS = (GDALDataset *) GDALOpenEx(dsn[0], 
					GDAL_OF_VECTOR | GDAL_OF_READONLY, NULL, options.data(), NULL)) != NULL &&
				poDS->GetLayerByName(layer[0]) != NULL) {
			GDALClose(poDS);
			Rcpp::Rcout << "Layer " << layer[0] << " in dataset " <<  dsn[0] << " already exists:\n" <<
				"use either append=TRUE to append to layer or append=FALSE to overwrite layer" << std::endl;
			Rcpp::stop("Dataset already exists.\n");
		}

		if (delete_dsn || (poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_UPDATE, 
				drivers.data(), options.data(), NULL)) == NULL) {
			// if dsn does not exist, then create:
			if ((poDS = poDriver->Create(dsn[0], 0, 0, 0, GDT_Unknown, options.data())) == NULL) {
				Rcpp::Rcout << "Creating dataset " <<  dsn[0] << " failed." << std::endl;
				Rcpp::stop("Creation failed.\n");
			}
		}
		if (! quiet) {
			Rcpp::Rcout << "Writing layer `" << layer[0] << "' to data source ";
			if (LENGTH(dsn[0]) > width - (44 + LENGTH(layer[0]) + LENGTH(driver[0])))
				Rcpp::Rcout << std::endl << "  ";
			Rcpp::Rcout << "`" << dsn[0] << "' using driver `" << driver[0] << "'" << std::endl;
		}
	}

	// can & do transaction?
	bool can_do_transaction = (poDS->TestCapability(ODsCTransactions) == TRUE); // can?
	bool transaction = false;
	if (can_do_transaction) { // try to start transaction:
		unset_error_handler();
		transaction = (poDS->StartTransaction() == OGRERR_NONE); // do?
		set_error_handler();
		if (! transaction) { // failed: #nocov start
			GDALClose(poDS);
			return 1; // transaction failed!
		} // #nocov end
	}

	// read geometries:
	OGRSpatialReference *sref = NULL;
	std::vector<OGRGeometry *> geomv; 
	OGRwkbGeometryType wkbType;
	if (! write_geometries) { // write an aspatial table, see #1345
		wkbType = wkbNone;
		for (int i = 0; i < geom.size(); i++)
			geomv.push_back(NULL);
	} else {
		Rcpp::CharacterVector clsv = geom.attr("class");
		wkbType = (OGRwkbGeometryType) make_type(clsv[0], dim[0], false, NULL, 0);
		geomv = ogr_from_sfc(geom, &sref);
		sref = handle_axis_order(sref);
	}

	// create layer:
	options = create_options(lco, quiet);
	OGRLayer *poLayer = NULL;
	bool update_layer = false;
	if ((poLayer = poDS->GetLayerByName(layer[0])) != NULL) {
		if (!quiet)
			Rcpp::Rcout << "Updating existing layer " << layer[0] << std::endl;
		update_layer = true;
	} else
		poLayer = poDS->CreateLayer(layer[0], sref, wkbType, options.data());
	if (sref != NULL)
		sref->Release();
	if (poLayer == NULL)  {
		Rcpp::Rcout << "Creating or updating layer " << layer[0] << " failed." << std::endl;
		GDALClose(poDS);
		Rcpp::stop("Write error.\n");
	}

	// write feature attribute fields & geometries:
	std::vector<OGRFieldType> fieldTypes = SetupFields(poLayer, obj, update_layer);
	if (! quiet) {
		Rcpp::Rcout << "Writing " << geomv.size() << " features with " << 
			fieldTypes.size() << " fields";
		if (write_geometries)
			Rcpp::Rcout << " and geometry type " << OGRGeometryTypeToName(wkbType);
		else
			Rcpp::Rcout << " without geometries";
		Rcpp::Rcout << "." << std::endl;
	}

	std::vector<int> fieldIndex = GetFieldIndex(poLayer, obj);

	for (size_t i = 0; i < geomv.size(); i++) { // create all features & add to layer:
		OGRFeature *poFeature = OGRFeature::CreateFeature(poLayer->GetLayerDefn());
		SetFields(poFeature, fieldTypes, obj, i, fieldIndex);
		if (write_geometries)
			poFeature->SetGeometryDirectly(geomv[i]);
		if (fids.size() > (int) i)
			poFeature->SetFID(std::stoll(Rcpp::as<std::string>(fids[i]), NULL, 10));
		if (poLayer->CreateFeature(poFeature) != OGRERR_NONE) {
			Rcpp::Rcout << "Failed to create feature " << i << " in " << layer[0] << std::endl;
		    // delete layer when  failing to  create feature
			OGRErr err = poDS->DeleteLayer(0);
			GDALClose(poDS);
			if (err != OGRERR_NONE) { // #nocov start
			    if (err == OGRERR_UNSUPPORTED_OPERATION)
			        Rcpp::Rcout << "Deleting layer not supported by driver `" << driver[0] << "'" << std::endl;
			    else if (! transaction)
			        Rcpp::Rcout << "Deleting layer `" << layer[0] << "' failed" << std::endl;
			} // #nocov end
			OGRFeature::DestroyFeature(poFeature);
			if (transaction) {
				unset_config_options(ConfigOptions);
				return 1; // try once more, writing to tmp file and copy #nocov
			} else
				Rcpp::stop("Feature creation failed.\n");
		}
		OGRFeature::DestroyFeature(poFeature); // deletes geom[i] as well
	}
	if (transaction && poDS->CommitTransaction() != OGRERR_NONE) { // #nocov start
		poDS->RollbackTransaction();
		GDALClose(poDS);
		Rcpp::stop("CommitTransaction() failed.\n"); 
	} // #nocov end
	GDALClose(poDS);
	unset_config_options(ConfigOptions);
	return 0; // all O.K.
}

// delete a data source, or one or more layers within a data source
// [[Rcpp::export(rng=false)]]
int CPL_delete_ogr(Rcpp::CharacterVector dsn, Rcpp::CharacterVector layer,
	Rcpp::CharacterVector driver, bool quiet = true) {

	// init:
	if (driver.size() != 1 || dsn.size() != 1)
		Rcpp::stop("argument dsn or driver not of length 1.\n");

	/* GDALAllRegister(); -- has been done during .onLoad() */
	// get driver:
	GDALDriver *poDriver = GetGDALDriverManager()->GetDriverByName(driver[0]);
	if (poDriver == NULL) {
		Rcpp::Rcout << "driver `" << driver[0] << "' not available." << std::endl;
		Rcpp::stop("Driver not available.\n");
	}

	// delete data source:
	if (layer.size() == 0) {
		if (poDriver->Delete(dsn[0]) != CE_None)
			Rcpp::Rcout << "Deleting source `" << dsn[0] << "' failed" << std::endl;
		else if (! quiet)
			Rcpp::Rcout << "Deleting source `" << dsn[0] << "' using driver `" << driver[0] << "'" << std::endl;
		return 0;
	} 

	// delete layer(s):

	// data set:
	GDALDataset *poDS = (GDALDataset *) GDALOpenEx(dsn[0], GDAL_OF_VECTOR | GDAL_OF_UPDATE, NULL, NULL, NULL); 
	if (poDS == NULL) {
		Rcpp::Rcout << "Data source `" << dsn[0] << "' not found" << std::endl;
		return 1;
	}

	bool can_do_transaction = (poDS->TestCapability(ODsCTransactions) == TRUE); // can?
	bool transaction = false;
	if (can_do_transaction) { // try to start transaction:
		unset_error_handler();
		transaction = (poDS->StartTransaction() == OGRERR_NONE); // do?
		set_error_handler();
		if (! transaction) { // failed: #nocov start
			GDALClose(poDS);
			Rcpp::Rcout << "On data source `" << dsn[0] << "' cannot start transaction" << std::endl;
			return 1; // transaction failed!
		} // #nocov end
	}

	for (int i = 0; i < layer.size(); i++) { // reverse loop order if inefficient?
		// find & delete layer:
		for (int iLayer = 0; iLayer < poDS->GetLayerCount(); iLayer++) {
			OGRLayer *poLayer = poDS->GetLayer(iLayer);
			if (poLayer != NULL && EQUAL(poLayer->GetName(), layer[i])) {
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
				if (err != OGRERR_NONE)
					Rcpp::Rcout << "Deleting layer `" << layer[i] << "' failed" << std::endl;
			}
		}
	}
	if (transaction && poDS->CommitTransaction() != OGRERR_NONE) { // #nocov start
		poDS->RollbackTransaction();
		Rcpp::Rcout << "CommitTransaction() failed." << std::endl; 
		return 1;
	} // #nocov end
	GDALClose(poDS);
	return 0;
}
