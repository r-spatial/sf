Rcpp::CharacterVector proj4stringFromSpatialReference(OGRSpatialReference *ref, char **cp);
Rcpp::List SfcFromOGRGeometries(std::vector<OGRGeometry *>, bool destroy);
