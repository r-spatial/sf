Rcpp::CharacterVector p4s_from_spatial_reference(OGRSpatialReference *ref, char **cp);
Rcpp::List sfc_from_geometries(std::vector<OGRGeometry *>, bool destroy);
