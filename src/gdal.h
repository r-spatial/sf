#ifndef SF_GDAL_H_
#define SF_GDAL_H_
void set_error_handler(void);
void unset_error_handler(void);
OGRSpatialReference *handle_axis_order(OGRSpatialReference *sr);
Rcpp::List create_crs(const OGRSpatialReference *ref, bool set_input);
Rcpp::CharacterVector wkt_from_spatial_reference(const OGRSpatialReference *srs);
int srid_from_crs(Rcpp::List crs);
void   set_config_options(Rcpp::CharacterVector ConfigOptions);
void unset_config_options(Rcpp::CharacterVector ConfigOptions);
#endif // SF_GDAL_H_
