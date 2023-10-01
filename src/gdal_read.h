
Rcpp::List sf_from_ogrlayer(OGRLayer *poLayer, bool quiet, bool int64_as_string,
		Rcpp::NumericVector toTypeUser, Rcpp::CharacterVector fid_column, bool promote_to_multi);

Rcpp::List CPL_ogr_layer_setup(Rcpp::CharacterVector datasource, Rcpp::CharacterVector layer,
		Rcpp::CharacterVector query,
		Rcpp::CharacterVector options, bool quiet, Rcpp::CharacterVector drivers,
		Rcpp::CharacterVector wkt_filter,
		bool dsn_exists,
		bool dsn_isdb,
		int width);

Rcpp::List CPL_read_gdal(Rcpp::CharacterVector fname, Rcpp::CharacterVector options, Rcpp::CharacterVector driver,
		bool read_data, Rcpp::NumericVector NA_value, Rcpp::List RasterIO_parameters);
