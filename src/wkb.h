Rcpp::List CPL_read_wkb(Rcpp::List wkb_list, bool EWKB, int endian);
Rcpp::List CPL_write_wkb(Rcpp::List sfc, bool EWKB, int endian, Rcpp::CharacterVector dim, 
	double precision);
int native_endian(void);
