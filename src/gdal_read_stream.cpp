
#include <ogrsf_frmts.h>

#define RCPP_DEFAULT_INCLUDE_CALL false
#include "Rcpp.h"

#if (GDAL_VERSION_MAJOR >= 3 && GDAL_VERSION_MINOR >= 6)

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

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_read_gdal_stream(
        Rcpp::RObject stream_xptr,
        Rcpp::CharacterVector datasource, Rcpp::CharacterVector layer,
		Rcpp::CharacterVector query,
		Rcpp::CharacterVector options, bool quiet, Rcpp::CharacterVector drivers,
		Rcpp::CharacterVector wkt_filter,
		bool dsn_exists = true,
		bool dsn_isdb = false,
		int width = 80) {
    Rcpp::List prep = CPL_ogr_layer_setup(datasource, layer, query, options,
							quiet,  drivers,
							wkt_filter,
							dsn_exists, dsn_isdb, width);
    OGRDataSource* poDS = (OGRDataSource*)(R_ExternalPtrAddr(prep[0]));
	OGRLayer* poLayer = (OGRLayer*)R_ExternalPtrAddr(prep[1]);
    auto stream_out = reinterpret_cast<struct ArrowArrayStream*>(
        R_ExternalPtrAddr(stream_xptr));

    OGRSpatialReference* crs = poLayer->GetSpatialRef();
    char* wkt_out;
    crs->exportToWkt(&wkt_out);
    std::string wkt_str(wkt_out);
    CPLFree(wkt_out);

    struct ArrowArrayStream stream_temp;
    if (!poLayer->GetArrowStream(&stream_temp, nullptr)) {
        Rcpp::stop("Failed to open ArrayStream from Layer");
    }

    GDALStreamWrapper::Make(&stream_temp, prep, stream_out);
    return Rcpp::CharacterVector::create(wkt_str);
}

#else

Rcpp::RObject CPL_read_gdal_stream(Rcpp::CharacterVector datasource, Rcpp::CharacterVector layer,
		Rcpp::CharacterVector query,
		Rcpp::CharacterVector options, bool quiet, Rcpp::CharacterVector drivers,
		Rcpp::CharacterVector wkt_filter,
		bool dsn_exists = true,
		bool dsn_isdb = false,
		int width = 80) {
    Rcpp::stop("read_stream() requires GDAL >= 3.6");
}

#endif
