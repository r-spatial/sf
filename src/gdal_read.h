Rcpp::List sf_from_ogrlayer(OGRLayer *poLayer, bool quiet, bool int64_as_string, 
		Rcpp::NumericVector toTypeUser, bool promote_to_multi);
Rcpp::List CPL_read_gdal(Rcpp::CharacterVector fname, Rcpp::CharacterVector options, Rcpp::CharacterVector driver,
		bool read_data, Rcpp::NumericVector NA_value, Rcpp::List RasterIO_parameters);
