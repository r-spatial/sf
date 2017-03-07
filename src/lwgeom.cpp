#include <Rcpp.h>

#include <string.h>

#include "wkb.h"

#ifdef HAVE_LWGEOM

extern "C" {
#include <liblwgeom.h>
}

// [[Rcpp::export]]
Rcpp::CharacterVector CPL_lwgeom_version(bool b = false) {
	return lwgeom_version();
}

// [[Rcpp::export]]
Rcpp::List CPL_make_valid(Rcpp::List sfc) {

	double precision = sfc.attr("precision");
	Rcpp::List wkblst = CPL_write_wkb(sfc, true, native_endian(), get_dim_sfc(sfc, NULL), precision);
	for (int i = 0; i < wkblst.size(); i++) {
		Rcpp::RawVector rv = wkblst[i];
		const uint8_t *wkb = &(rv[0]); 
		LWGEOM *lwg = lwgeom_from_wkb(wkb, rv.size(),
			LW_PARSER_CHECK_MINPOINTS & LW_PARSER_CHECK_ODD & LW_PARSER_CHECK_CLOSURE);
		// do the trick:
		LWGEOM *lwg_ret = lwgeom_make_valid(lwg);
		lwgeom_free(lwg);
		size_t size;
		wkb = lwgeom_to_wkb(lwg_ret, WKB_EXTENDED, &size);
		lwgeom_free(lwg_ret);
		Rcpp::RawVector raw(size);
		memcpy(&(raw[0]), wkb, size);
		lwfree((void *) wkb);
		wkblst[i] = raw;
	}
	return CPL_read_wkb(wkblst, true, native_endian());
}

#else

Rcpp::CharacterVector CPL_lwgeom_version(bool b = false) {
	return NA_STRING;
}

Rcpp::List CPL_make_valid(Rcpp::List sfc) {
	Rcpp::stop("st_make_valid requires compiling against liblwgeom\n"); // #nocov
	return sfc;
}

#endif
