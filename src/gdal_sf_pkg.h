#ifndef NO_GDAL_CPP_HEADERS
Rcpp::CharacterVector p4s_from_spatial_reference(OGRSpatialReference *ref);
Rcpp::List sfc_from_ogr(std::vector<OGRGeometry *>, bool destroy);
std::vector<OGRGeometry *> ogr_from_sfc(Rcpp::List sfc, OGRSpatialReference **sref);
Rcpp::List create_crs(const OGRSpatialReference *ref, bool set_input = true);
#endif

void handle_error(OGRErr err);
std::vector<char *> create_options(Rcpp::CharacterVector lco, bool quiet = true);
Rcpp::CharacterVector charpp2CV(CSLConstList cp);
