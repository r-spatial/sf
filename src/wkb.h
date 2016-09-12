Rcpp::List CPL_read_wkb(Rcpp::List wkb_list, bool EWKB, int endian, bool debug);
Rcpp::List CPL_write_wkb(Rcpp::List sfc, bool EWKB, int endian, Rcpp::CharacterVector dim, 
	bool debug, double precision);
int native_endian(void);
