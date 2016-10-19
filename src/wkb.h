Rcpp::List CPL_read_wkb(Rcpp::List wkb_list, bool EWKB, int endian);
Rcpp::List CPL_write_wkb(Rcpp::List sfc, bool EWKB, int endian, Rcpp::CharacterVector dim, double precision);
Rcpp::List CPL_hex_to_raw(Rcpp::CharacterVector cx);
int native_endian(void);
unsigned int make_type(const char *cls, const char *dim, bool EWKB, int *tp, int srid);
