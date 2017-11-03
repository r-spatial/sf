#include "../inst/include/wkb.h"

// [[Rcpp::export]]
Rcpp::List CPL_hex_to_raw(Rcpp::CharacterVector cx) {
	return sf::hex_to_raw(cx);
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_raw_to_hex(Rcpp::RawVector raw) {
	return sf::raw_to_hex(raw);
}

// [[Rcpp::export]]
Rcpp::List CPL_read_wkb(Rcpp::List wkb_list, bool EWKB = false, bool spatialite = false, int endian = 0) {
	return sf::read_wkb(wkb_list, EWKB, spatialite, endian);
}

// [[Rcpp::export]]
Rcpp::List CPL_write_wkb(Rcpp::List sfc, bool EWKB = false, int endian = 0,
                         Rcpp::CharacterVector dim = "XY", double precision = 0.0) {
	return sf::write_wkb(sfc, EWKB, endian, dim, precision);
}

