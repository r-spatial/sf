Rcpp::List ReadWKB(Rcpp::List wkb_list, bool EWKB, int endian, bool debug);
Rcpp::List WriteWKB(Rcpp::List sfc, bool EWKB, int endian, Rcpp::CharacterVector dim, 
	bool debug, double precision);
int native_endian(void);
