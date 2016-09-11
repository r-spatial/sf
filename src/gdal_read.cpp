#include <ogrsf_frmts.h>

#include "Rcpp.h"

// #include "wkb.h"
#include "gdal.h"

Rcpp::List AllocateOutList(OGRFeatureDefn *poFDefn, int n_features) {
	int n = poFDefn->GetFieldCount() + 1; // last one for features
	Rcpp::List out(n);
	Rcpp::CharacterVector names(poFDefn->GetFieldCount() + 1);
	for (int i = 0; i < poFDefn->GetFieldCount(); i++) {
		OGRFieldDefn *poFieldDefn = poFDefn->GetFieldDefn(i);
        switch (poFieldDefn->GetType()) {
			case OFTInteger:
				out[i] = Rcpp::IntegerVector(n_features);
				break;
            case OFTInteger64: // fall through, but warn if > 2^53!
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
	names[poFDefn->GetFieldCount()] = "geometry";
	out.attr("names") = names;
	return(out);
}

// [[Rcpp::export]]
Rcpp::List Read_OGR(Rcpp::CharacterVector datasource, Rcpp::CharacterVector layer)
{
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
	int n = (int) n_d; // what is List's max length?
    OGRFeature *poFeature;
	std::vector<OGRGeometry *> poGeometryV(n); // full archive
	std::vector<OGRFeature *> poFeatureV(n); // full archive

	OGRFeatureDefn *poFDefn = poLayer->GetLayerDefn();
	Rcpp::List out = AllocateOutList(poFDefn, n);

    poLayer->ResetReading();
	int i = 0;
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
					// check overflow?
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
		poGeometryV[i] = poFeature->GetGeometryRef();
		poFeatureV[i] = poFeature; // so we can delete once converted
		i++;
    }
	// convert to R:
	// OGRSpatialReference *ref = poLayer->GetSpatialRef();	
	// char *cp = NULL;
	//Rcpp::CharacterVector proj4string = proj4stringFromSpatialReference(ref, &cp);
	out[ poFDefn->GetFieldCount() ] = SfcFromOGRGeometries(poGeometryV, false); // don't destroy

	//ret.attr("proj4string") = proj4string;
	//CPLFree(cp);

	// clean up:
    for (int i = 0; i < n; i++)
		OGRFeature::DestroyFeature( poFeatureV[i] );
    GDALClose( poDS ); // close & destroys data source

	return(out);
}
