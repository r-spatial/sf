# see https://docs.google.com/presentation/d/1Hl4KapfAENAOf4gv-pSngKwvS_jwNVHRPZTTDzXXn6Q/view?pli=1#slide=id.i0
# and the libs2 package:
# https://github.com/r-spatial/libs2

load_libs2 = function() {
	if (! requireNamespace("libs2", quietly = TRUE))
		stop('package libs2 required, please install it first')
}


# from libs2 to sf:
#' @export
#' @name s2
st_as_sfc.wk_wkb = function(x, ..., crs = NA_crs_) {
	class(x) = "WKB"
	st_set_crs(st_as_sfc(x, ...), crs)
}

st_as_sfc_s2 = function(x, ...) {
	load_libs2()
	st_as_sfc(libs2::as_wkb(x), ...)
}

#' @name s2
#' @param x object of the respective class
#' @param ... passed on; might contain named argument crs
#' @export
st_as_sfc.s2latlng = st_as_sfc_s2

#' @name s2
#' @export
st_as_sfc.s2polyline = st_as_sfc_s2

#' @name s2
#' @export
st_as_sfc.s2polygon = function(x, ...) {
	load_libs2()
	st_cast(st_as_sfc(libs2::as_wkb(x), ...))
}

#' @name s2
#' @export
st_as_sfc.s2geography = function(x, ...) {
	load_libs2()
	st_cast(st_as_sfc(libs2::s2_asbinary(x, ...)))
}

#' functions for spherical geometry, using libs2 package
#' 
#' functions for spherical geometry, using the libs2 package based on the google s2geometry.io library
#' @name s2 
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @export
#' @details \code{st_as_s2} converts an \code{sf} POLYGON object into a form readable by \code{s2}.
#' @examples
#' m = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
#' m1 = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,0), c(-1,-1))
#' m0 = m[5:1,]
#' mp = st_multipolygon(list(
#'	list(m, 0.8 * m0, 0.01 * m1 + 0.9),
#'	list(0.7* m, 0.6*m0), 
#'	list(0.5 * m0), 
#'	list(m+2),
#'	list(m+4,(.9*m0)+4)
#'	))
#' sf = st_sfc(mp, mp, crs = 'EPSG:4326')
#' s2 = st_as_s2(sf)
st_as_s2 = function(x, ...) UseMethod("st_as_s2")

#' @name s2
#' @export
st_as_s2.sf = function(x, ...) st_as_s2(st_geometry(x), ...)

#' @export
st_as_s2.sfc = function(x, ...) {
	libs2::s2geography(structure(st_as_binary(x), class = "wk_wkb"))
}
