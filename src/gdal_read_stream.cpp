
#include <ogrsf_frmts.h>

#define RCPP_DEFAULT_INCLUDE_CALL false
#include "Rcpp.h"

#if GDAL_VERSION_NUM >= GDAL_COMPUTE_VERSION(3,6,0)

#include <ogr_recordbatch.h>
#include "gdal_read.h"

class GDALStreamWrapper {
public:
    static void Make(struct ArrowArrayStream* stream, Rcpp::List shelter,
                     struct ArrowArrayStream* stream_out) {
        stream_out->get_schema = &get_schema_wrap;
        stream_out->get_next = &get_next_wrap;
        stream_out->get_last_error = &get_last_error_wrap;
        stream_out->release = &release_wrap;
        stream_out->private_data = new GDALStreamWrapper(stream, shelter);
    }

    ~GDALStreamWrapper() {
        stream_.release(&stream_);
        GDALDataset* poDS = (GDALDataset*)R_ExternalPtrAddr(shelter_[0]);
        GDALClose(poDS);
        R_SetExternalPtrAddr(shelter_[0], nullptr);
    }

private:
    // The parent stream as returned from GDAL
    struct ArrowArrayStream stream_;
    Rcpp::List shelter_;

    GDALStreamWrapper(struct ArrowArrayStream* stream, Rcpp::List shelter):
        shelter_(shelter) {
        memcpy(&stream_, stream, sizeof(struct ArrowArrayStream));
        stream->release = nullptr;
    }

    int get_schema(struct ArrowSchema* out) {
        return stream_.get_schema(&stream_, out);
    }

    int get_next(struct ArrowArray* out) {
        return stream_.get_next(&stream_, out);
    }

    const char* get_last_error() {
        return stream_.get_last_error(&stream_);
    }

    static int get_schema_wrap(struct ArrowArrayStream* stream, struct ArrowSchema* out) {
        return reinterpret_cast<GDALStreamWrapper*>(stream->private_data)->get_schema(out);
    }

    static int get_next_wrap(struct ArrowArrayStream* stream, struct ArrowArray* out) {
        return reinterpret_cast<GDALStreamWrapper*>(stream->private_data)->get_next(out);
    }

    static const char* get_last_error_wrap(struct ArrowArrayStream* stream) {
        return reinterpret_cast<GDALStreamWrapper*>(stream->private_data)->get_last_error();
    }

    static void release_wrap(struct ArrowArrayStream* stream) {
        delete reinterpret_cast<GDALStreamWrapper*>(stream->private_data);
        stream->release = nullptr;
    }
};

#endif

// [[Rcpp::export(rng=false)]]
Rcpp::List CPL_read_gdal_stream(
        Rcpp::RObject stream_xptr,
        Rcpp::CharacterVector datasource, Rcpp::CharacterVector layer,
		Rcpp::CharacterVector query,
		Rcpp::CharacterVector options, bool quiet, Rcpp::CharacterVector drivers,
		Rcpp::CharacterVector wkt_filter,
		bool dsn_exists,
		bool dsn_isdb,
        Rcpp::CharacterVector fid_column,
		int width) {

#if GDAL_VERSION_NUM >= GDAL_COMPUTE_VERSION(3,6,0)

    const char* array_stream_options[] = {"INCLUDE_FID=NO", nullptr};
    if (fid_column.size() == 1) {
        array_stream_options[0] = "INCLUDE_FID=YES";
    }

    Rcpp::List prep = CPL_ogr_layer_setup(datasource, layer, query, options,
							quiet,  drivers,
							wkt_filter,
							dsn_exists, dsn_isdb, width);
	OGRLayer* poLayer = (OGRLayer*)R_ExternalPtrAddr(prep[1]);
    auto stream_out = reinterpret_cast<struct ArrowArrayStream*>(
        R_ExternalPtrAddr(stream_xptr));

    OGRFeatureDefn* poFDefn = poLayer->GetLayerDefn();
    Rcpp::CharacterVector geom_field_name(poFDefn->GetGeomFieldCount());
    Rcpp::CharacterVector geom_field_crs(poFDefn->GetGeomFieldCount());

    for (int i = 0; i < poFDefn->GetGeomFieldCount(); i++) {
        OGRGeomFieldDefn* poGFDefn = poFDefn->GetGeomFieldDefn(i);
		if (poGFDefn == nullptr) {
			Rcpp::stop("GeomFieldDefn error"); // #nocov
        }

        const char* name = poGFDefn->GetNameRef();
        if (strlen(name) == 0) {
            name = "geometry";
        }

        const OGRSpatialReference* crs = poGFDefn->GetSpatialRef();
        Rcpp::String wkt_str = NA_STRING;
        if (crs != nullptr) {
            char* wkt_out;
            crs->exportToWkt(&wkt_out);
            wkt_str = wkt_out;
            CPLFree(wkt_out);
        }

        geom_field_name[i] = name;
        geom_field_crs[i] = wkt_str;
    }

    struct ArrowArrayStream stream_temp;
    if (!poLayer->GetArrowStream(&stream_temp, array_stream_options)) {
        Rcpp::stop("Failed to open ArrayStream from Layer");
    }

    GDALStreamWrapper::Make(&stream_temp, prep, stream_out);

    // The reported feature count is incorrect if there is a query
    double num_features;
    if (query.size() == 0) {
        num_features = (double) poLayer->GetFeatureCount(false);
    } else {
        num_features = -1;
    }

    return Rcpp::List::create(
        geom_field_name,
        geom_field_crs,
        Rcpp::NumericVector::create(num_features));

#else

    Rcpp::stop("read_stream() requires GDAL >= 3.6");

#endif

}
