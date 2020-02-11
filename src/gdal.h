void set_error_handler(void);
void unset_error_handler(void);
OGRSpatialReference *handle_axis_order(OGRSpatialReference *sr);
Rcpp::List create_crs(const OGRSpatialReference *ref, bool set_input);
