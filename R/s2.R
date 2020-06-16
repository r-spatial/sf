# see https://docs.google.com/presentation/d/1Hl4KapfAENAOf4gv-pSngKwvS_jwNVHRPZTTDzXXn6Q/view?pli=1#slide=id.i0
# and the r-spatial/s2 package:
# https://github.com/r-spatial/s2

# from s2 to sf:
#' @export
#' @param crs coordinate reference system; object of class \code{crs}
#' @param ... passed on
#' @name s2
st_as_sfc.wk_wkb = function(x, ..., crs = NA_crs_) {
	class(x) = "WKB"
	st_set_crs(st_as_sfc(x, ...), crs)
}


#' @name s2
#' @export
#' @param endian integer; 0 or 1: defaults to the endian of the native machine
st_as_sfc.s2_geography = function(x, ..., crs = NA_crs_, endian = match(.Platform$endian, c("big", "little")) - 1L) {
	if (! requireNamespace("s2", quietly = TRUE))
		stop('package s2 required, please install it first')
	st_cast(st_as_sfc(s2::s2_as_binary(x, endian = endian), ..., crs = crs))
}

#' functions for spherical geometry, using s2 package
#' 
#' functions for spherical geometry, using the s2 package based on the google s2geometry.io library
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

#' @name s2
#' @param oriented logical; if \code{FALSE}, polygons that
#' cover more than half of the globe are inverted; if \code{TRUE}, no reversal
#' takes place and it is assumed that the inside of the polygon is to the
#' left of the polygon's path.
#' @export
st_as_s2.sfc = function(x, ..., oriented = FALSE) {
	if (! requireNamespace("s2", quietly = TRUE))
		stop('package s2 required, please install it first')
	s2::as_s2_geography(st_as_binary(x), ..., oriented = oriented)
}
