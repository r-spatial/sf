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
            case OFTInteger64: // fall through: Int64 -> double
            case OFTReal:
				out[i] = Rcpp::NumericVector(n_features);
				break;
			case OFTString:
				out[i] = Rcpp::CharacterVector(n_features);
				break;
			// TODO: OFTDate, Time, DateTime, Binary?
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
		bool quiet = false, int iGeomField = 0) {
	// adapted from the OGR tutorial @ www.gdal.org
    GDALDataset     *poDS;
    poDS = (GDALDataset*) GDALOpenEx( datasource[0], GDAL_OF_VECTOR, NULL, NULL, NULL );
    if( poDS == NULL ) {
		Rcpp::Rcout << "Cannot open data source " << datasource[0] << std::endl;
		throw std::invalid_argument("Open failed.\n");
	}
    OGRLayer  *poLayer = poDS->GetLayerByName( layer[0] );
	if (poLayer == NULL) {
		Rcpp::Rcout << "Cannot open layer " << layer[0] << std::endl;
		throw std::invalid_argument("Open failed.\n");
	}
	double n_d = (double) poLayer->GetFeatureCount();
	if (n_d < 0)
		throw std::out_of_range("Cannot read layer with unknown number of features");
	if (n_d > INT_MAX)
		throw std::out_of_range("Cannot read layer with more than MAX_INT features");
	size_t n = (size_t) n_d; // what is List's max length?
    OGRFeature *poFeature;
	std::vector<OGRGeometry *> poGeometryV(n); // full archive
	std::vector<OGRFeature *> poFeatureV(n); // full archive

	OGRFeatureDefn *poFDefn = poLayer->GetLayerDefn();
	if (! quiet) {
		Rcpp::Rcout << "Reading layer " << layer[0] << " from data source " << datasource[0] <<
			" using driver " << poDS->GetDriverName() << std::endl;
		Rcpp::Rcout << "features:    " << n << std::endl;
		Rcpp::Rcout << "fields:      " << poFDefn->GetFieldCount() << std::endl;
	}
	OGRGeomFieldDefn *poGFDefn = poFDefn->GetGeomFieldDefn(iGeomField);
	if (poGFDefn == NULL)
		throw std::range_error("wrong value for iGeomField");
	Rcpp::List out = allocate_out_list(poFDefn, n, poGFDefn->GetNameRef());
	/*
	if (type >= 0)
		poFDefn->SetGeomType((OGRwkbGeometryType) type);
	*/

    poLayer->ResetReading();
	int i = 0;
	double dbl_max_int64 = pow(2.0, 53);
	bool warn_int64 = false;
    while( (poFeature = poLayer->GetNextFeature()) != NULL )
    {

        int iField;
        for( iField = 0; iField < poFDefn->GetFieldCount(); iField++ )
        {
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
				case OFTReal: {
					Rcpp::NumericVector nv;
					nv = out[iField];
					nv[i] = (double) poFeature->GetFieldAsDouble(iField);
					}
					break;
				case OFTString: {
					Rcpp::CharacterVector cv;
					cv = out[iField];
					cv[i] = poFeature->GetFieldAsString(iField);
					}
					break;
				// handle other types...
				default:
					break;
			}
        }
		// poFeature->SetGeometryDirectly(OGRGeometryFactory::forceToMultiPolygon(
		//	poFeature->GetGeometryRef()));
		poGeometryV[i] = poFeature->GetGeomFieldRef(iGeomField);
		// poGeometryV[i] = poFeature->StealGeometry();
		poFeatureV[i] = poFeature; // so we can delete once converted
		i++;
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
			Rcpp::Rcout << "proj4string: " << proj4string[0] << std::endl;
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
