#include <ogrsf_frmts.h>

#include "Rcpp.h"

// #include "wkb.h"
#include "gdal.h"

Rcpp::List allocate_out_list(OGRFeatureDefn *poFDefn, int n_features, const char *geom_name) {
	int n = poFDefn->GetFieldCount() + 1; // last one for features
	Rcpp::List out(n);
	Rcpp::CharacterVector names(poFDefn->GetFieldCount() + 1);
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
            case OFTReal:
				out[i] = Rcpp::NumericVector(n_features);
				break;
			case OFTString:
				out[i] = Rcpp::CharacterVector(n_features);
				break;
			// perhaps FIXME: Time, Binary?
			default:
				throw std::invalid_argument("Unrecognized field type\n");
				break;
		}
		names[i] = poFieldDefn->GetNameRef();
	}
	if (*geom_name == '\0')
		names[poFDefn->GetFieldCount()] = "geometry";
	else
		names[poFDefn->GetFieldCount()] = geom_name;
	out.attr("names") = names;
	return(out);
}

// [[Rcpp::export]]
Rcpp::List CPL_read_ogr(Rcpp::CharacterVector datasource, Rcpp::CharacterVector layer, 
		bool quiet = false, int iGeomField = 0, int toTypeUser = 0) {
	// adapted from the OGR tutorial @ www.gdal.org
    GDALDataset *poDS;
	poDS = (GDALDataset *) GDALOpenEx( datasource[0], GDAL_OF_VECTOR, NULL, NULL, NULL );
    if( poDS == NULL ) {
		Rcpp::Rcout << "Cannot open data source " << datasource[0] << std::endl;
		throw std::invalid_argument("Open failed.\n");
	}
    OGRLayer *poLayer;
	poLayer = poDS->GetLayerByName( layer[0] );
	if (poLayer == NULL) {
		Rcpp::Rcout << "Cannot get layer " << layer[0] << std::endl;
		throw std::invalid_argument("Getting layer failed.\n");
	}
	double n_d = (double) poLayer->GetFeatureCount();
	if (n_d < 0)
		throw std::out_of_range("Cannot read layer with unknown number of features");
	if (n_d > INT_MAX)
		throw std::out_of_range("Cannot read layer with more than MAX_INT features");
	size_t n = (size_t) n_d; // what is List's max length?
	std::vector<OGRGeometry *> poGeometryV(n); // full archive
	std::vector<OGRFeature *> poFeatureV(n); // full archive

	OGRFeatureDefn *poFDefn = poLayer->GetLayerDefn();
	if (! quiet) {
		Rcpp::Rcout << "Reading layer " << layer[0] << " from data source " << datasource[0] <<
			" using driver \"" << poDS->GetDriverName() << "\"" << std::endl;
		Rcpp::Rcout << "features:       " << n << std::endl;
		Rcpp::Rcout << "fields:         " << poFDefn->GetFieldCount() << std::endl;
	}
	// get the geometry field:
	OGRGeomFieldDefn *poGFDefn = poFDefn->GetGeomFieldDefn(iGeomField);
	if (poGFDefn == NULL)
		throw std::range_error("wrong value for iGeomField");
	Rcpp::List out = allocate_out_list(poFDefn, n, poGFDefn->GetNameRef());

	// read all features:
    poLayer->ResetReading();
	int i = 0, lastType = 0, toType = 0;
	double dbl_max_int64 = pow(2.0, 53);
	bool warn_int64 = false;
    OGRFeature *poFeature;
    while( (poFeature = poLayer->GetNextFeature()) != NULL )
    {

		// deal with feature attribute fields:
        int iField;
        for( iField = 0; iField < poFDefn->GetFieldCount(); iField++ ) {
            OGRFieldDefn *poFieldDefn = poFDefn->GetFieldDefn( iField );
			switch(poFieldDefn->GetType()) {
				case OFTInteger: {
					Rcpp::IntegerVector iv;
					iv = out[iField];
					iv[i] = poFeature->GetFieldAsInteger(iField);
					}
					break;
				case OFTInteger64: {
					Rcpp::NumericVector nv;
					nv = out[iField];
					nv[i] = (double) poFeature->GetFieldAsInteger64(iField);
					if (nv[i] > dbl_max_int64)
						warn_int64 = true;
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
					Rcpp::NumericVector nv;
					nv = out[iField];
					if (poFieldDefn->GetType() == OFTDateTime) {
						Rcpp::Function as_POSIXct_POSIXlt("as.POSIXct.POSIXlt");
						Rcpp::NumericVector ret = as_POSIXct_POSIXlt(dtlst);
						nv[i] = ret[0];
					} else {
						Rcpp::Function as_Date_POSIXlt("as.Date.POSIXlt");
						Rcpp::NumericVector ret = as_Date_POSIXlt(dtlst);
						nv[i] = ret[0];
					}
					break;
					}
					break;
				case OFTReal: {
					Rcpp::NumericVector nv;
					nv = out[iField];
					nv[i] = (double) poFeature->GetFieldAsDouble(iField);
					}
					break;
				default: // break through:
				case OFTString: {
					Rcpp::CharacterVector cv;
					cv = out[iField];
					cv[i] = poFeature->GetFieldAsString(iField);
					}
					break;
			}
        }

		// deal with feature geometry
		poGeometryV[i] = poFeature->GetGeomFieldRef(iGeomField);
		if (toTypeUser == 0) { // do something auto clever; check for a mix of types:
			if (lastType == 0)
				lastType = poGeometryV[i]->getGeometryType(); // init at feature 0.
			else {
				if (lastType != poGeometryV[i]->getGeometryType()) { // a mix:
					if (lastType > poGeometryV[i]->getGeometryType())
						toType = lastType;
					else
						toType = poGeometryV[i]->getGeometryType();
					lastType = toType;
				}
			}
		}
		// poGeometryV[i] = poFeature->StealGeometry();
		poFeatureV[i] = poFeature; // so we can delete once converted
		i++;
    }
	// TRY to deal with mixed types -- this does not always work, and there seems no way to catch failure:
	if (toTypeUser || toType) {
		if (toTypeUser)
			toType = toTypeUser;
		for (i = 0; i < poFeatureV.size(); i++) {
			poFeatureV[i]->SetGeometryDirectly(
				OGRGeometryFactory::forceTo(poFeatureV[i]->StealGeometry(), 
				(OGRwkbGeometryType) toType, NULL) );
			poGeometryV[i] = poFeatureV[i]->GetGeomFieldRef(iGeomField);
		}
	}
	if (! quiet) {
		if (toType)
			Rcpp::Rcout << "converted into: " << poGeometryV[0]->getGeometryName() << std::endl;
		else
			Rcpp::Rcout << "geometry type:  " << poGeometryV[0]->getGeometryName() << std::endl;
	}
	// convert to R:
	Rcpp::List sfc = sfc_from_geometries(poGeometryV, false); // don't destroy
	OGRSpatialReference *ref = poLayer->GetSpatialRef();	
	if (ref == NULL) // try from Geometry
		ref = poGeometryV[0]->getSpatialReference();
	if (ref != NULL) {
		Rcpp::CharacterVector proj4string = p4s_from_spatial_reference(ref);
		sfc.attr("proj4string") = proj4string;
		if (! quiet)
			Rcpp::Rcout << "proj4string:    " << proj4string[0] << std::endl;
	} 
	if (warn_int64)
		Rcpp::Rcout << "Integer64 values larger than " << dbl_max_int64 << 
			" lost significance after conversion to double" << std::endl;
	sfc.attr("class") = "sfc";
	out[ poFDefn->GetFieldCount() ] = sfc;

	// clean up:
    for (size_t i = 0; i < n; i++)
		OGRFeature::DestroyFeature( poFeatureV[i] );
    GDALClose( poDS ); // close & destroys data source

	return(out);
}
