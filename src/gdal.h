Rcpp::CharacterVector p4s_from_spatial_reference(OGRSpatialReference *ref);
Rcpp::List sfc_from_ogr(std::vector<OGRGeometry *>, bool destroy);
std::vector<OGRGeometry *> ogr_from_sfc(Rcpp::List sfc, OGRSpatialReference **sref);
std::vector<char *> create_options(Rcpp::CharacterVector lco, bool quiet);
void handle_error(OGRErr err);
