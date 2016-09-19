Rcpp::CharacterVector p4s_from_spatial_reference(OGRSpatialReference *ref);
Rcpp::List sfc_from_geometries(std::vector<OGRGeometry *>, bool destroy);
std::vector<OGRGeometry *> ogr_geometries_from_sfc(Rcpp::List sfc);
