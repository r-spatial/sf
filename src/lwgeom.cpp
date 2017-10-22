#include <Rcpp.h>

#ifdef HAVE_LWGEOM

#include <string.h>
#include "wkb.h"

extern "C" {
#include <liblwgeom.h>
}

using namespace Rcpp; // for _ to work

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_lwgeom_version(bool b = false) {
	return lwgeom_version();
}

// in
std::vector<LWGEOM *> lwgeom_from_sfc(Rcpp::List sfc) {
	double precision = sfc.attr("precision");
	std::vector<LWGEOM *> lwgeom_v(sfc.size()); // return
	Rcpp::List wkblst = CPL_write_wkb(sfc, true, native_endian(), get_dim_sfc(sfc, NULL), precision);
	for (int i = 0; i < wkblst.size(); i++) {
		Rcpp::RawVector rv = wkblst[i];
		const uint8_t *wkb = &(rv[0]); 
		lwgeom_v[i] = lwgeom_from_wkb(wkb, rv.size(),
			LW_PARSER_CHECK_MINPOINTS & LW_PARSER_CHECK_ODD & LW_PARSER_CHECK_CLOSURE);
	}
	return lwgeom_v;
}

// out
Rcpp::List sfc_from_lwgeom(std::vector<LWGEOM *> lwgeom_v) {
	Rcpp::List wkblst(lwgeom_v.size()); 
	for (int i = 0; i < wkblst.size(); i++) {
		size_t size;
		const uint8_t *wkb = lwgeom_to_wkb(lwgeom_v[i], WKB_EXTENDED, &size);
		lwgeom_free(lwgeom_v[i]);
		Rcpp::RawVector raw(size);
		memcpy(&(raw[0]), wkb, size);
		lwfree((void *) wkb);
		wkblst[i] = raw;
	}
	return CPL_read_wkb(wkblst, true, false, native_endian());
}

// [[Rcpp::export]]
Rcpp::List CPL_make_valid(Rcpp::List sfc) {

	std::vector<LWGEOM *> lwgeom_v = lwgeom_from_sfc(sfc);
	for (int i = 0; i < lwgeom_v.size(); i++) {
		// do the trick:
		LWGEOM *lwg_ret = lwgeom_make_valid(lwgeom_v[i]);
		lwgeom_free(lwgeom_v[i]);
		lwgeom_v[i] = lwg_ret;
	}
	return sfc_from_lwgeom(lwgeom_v);
}

// [[Rcpp::export]]
Rcpp::List CPL_split(Rcpp::List sfc, Rcpp::List blade) {

	std::vector<LWGEOM *> lwgeom_in = lwgeom_from_sfc(sfc);
	std::vector<LWGEOM *> lwgeom_blade = lwgeom_from_sfc(blade);
	for (int i = 0; i < lwgeom_in.size(); i++) {
		LWGEOM *lwg_ret = lwgeom_split(lwgeom_in[i], lwgeom_blade[0]);
		lwgeom_free(lwgeom_in[i]);
		lwgeom_in[i] = lwg_ret;
	}
	sfc_from_lwgeom(lwgeom_blade); // free
	return sfc_from_lwgeom(lwgeom_in);
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_geohash(Rcpp::List sfc, int prec) {

	Rcpp::CharacterVector chr(sfc.size()); // return
	std::vector<LWGEOM *> lwgeom_v = lwgeom_from_sfc(sfc);
	for (int i = 0; i < lwgeom_v.size(); i++) {
		chr(i) = lwgeom_geohash(lwgeom_v[i], prec);
		lwgeom_free(lwgeom_v[i]);
	}
	return chr;
}

// [[Rcpp::export]]
Rcpp::List CPL_lwgeom_transform(Rcpp::List sfc, Rcpp::CharacterVector p4s) {
	if (p4s.size() != 2)
		Rcpp::stop("st_lwgeom_transform: p4s needs to be a length 2 character vector\n"); // #nocov
	std::vector<LWGEOM *> lwgeom_v = lwgeom_from_sfc(sfc);
	projPJ src = lwproj_from_string(p4s[0]);
	if (src == NULL)
		Rcpp::stop("st_lwgeom_transform: wrong source proj4string\n"); // #nocov
	projPJ target = lwproj_from_string(p4s[1]);
	if (target == NULL)
		Rcpp::stop("st_lwgeom_transform: wrong target proj4string\n"); // #nocov
	for (int i = 0; i < lwgeom_v.size(); i++) {
		// in-place transformation w/o GDAL:
		if (lwgeom_transform(lwgeom_v[i], src, target) != LW_SUCCESS) {
			Rcpp::Rcout << "Failed on geometry " << i + 1 << std::endl; // #nocov
			Rcpp::stop("st_lwgeom_transform failed\n"); // #nocov
		}
	}
	pj_free(src);
	pj_free(target);
	Rcpp::List ret = sfc_from_lwgeom(lwgeom_v); // frees lwgeom_v
	Rcpp::List crs = Rcpp::List::create(
		_["epsg"] = NA_INTEGER, 
		_["proj4string"] = CharacterVector::create(p4s[1]));
	crs.attr("class") = "crs";
	ret.attr("crs") = crs;
	ret.attr("class") = "sfc";
	return ret;
}

#else
// #nocov start

Rcpp::CharacterVector CPL_lwgeom_version(bool b = false) {
	return NA_STRING;
}

Rcpp::List CPL_make_valid(Rcpp::List sfc) {
	Rcpp::stop("st_make_valid requires compilation against liblwgeom\n");
	return sfc;
}

Rcpp::List CPL_split(Rcpp::List sfc, Rcpp::List blade) {
	Rcpp::stop("st_split requires compilation against liblwgeom\n");
	return sfc;
}

Rcpp::CharacterVector CPL_geohash(Rcpp::List sfc, int prec) {
	Rcpp::stop("st_make_valid requires compilation against liblwgeom\n");
	return NA_STRING;
}

Rcpp::List CPL_lwgeom_transform(Rcpp::List sfc, Rcpp::CharacterVector p4s) {
	Rcpp::stop("st_transform with use_gdal = FALSE requires compilation against liblwgeom\n");
	return sfc;
}

// #nocov end
#endif
