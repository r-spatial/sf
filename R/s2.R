# see https://docs.google.com/presentation/d/1Hl4KapfAENAOf4gv-pSngKwvS_jwNVHRPZTTDzXXn6Q/view?pli=1#slide=id.i0
# and the r-spatial/s2 package:
# https://github.com/r-spatial/s2

#' @export
#' @param ... passed on
#' @param use_s2 logical; if \code{TRUE}, use the s2 spherical geometry package
#' for geographical coordinate operations
#' @name s2
#' @return \code{sf_use_s2} returns the value of this variable before (re)setting it,
#' invisibly if \code{use_s2} is not missing.
sf_use_s2 = function(use_s2) {
	ret_val = getOption("sf_use_s2", default = TRUE)
	if (! missing(use_s2)) {
		stopifnot(is.logical(use_s2), length(use_s2)==1, !is.na(use_s2))
		if (ret_val != use_s2)
			message(paste0("Spherical geometry (s2) switched ", ifelse(use_s2, "on", "off")))
		options(sf_use_s2 = use_s2)
		invisible(ret_val)
	} else
		ret_val
}

#' @name st_as_sfc
#' @export
#' @param crs coordinate reference system to be assigned; object of class \code{crs}
#' @param endian integer; 0 or 1: defaults to the endian of the native machine
st_as_sfc.s2_geography = function(x, ..., crs = st_crs(4326),
		endian = match(.Platform$endian, c("big", "little")) - 1L) {
	st_cast(st_as_sfc(s2::s2_as_binary(x, endian = endian), ..., crs = crs))
}

#' @name st_as_sf
#' @param crs coordinate reference system to be assigned; object of class \code{crs}
#' @export
st_as_sf.s2_geography = function(x, ..., crs = st_crs(4326)) {
	st_sf(geometry = st_as_sfc(x, ..., crs = crs))
}

# dynamically exported in tidyverse.R
as_s2_geography.sfg <- function(x, ..., oriented = getOption("s2_oriented", FALSE)) {
	b = structure(list(st_as_binary(x)), class = "WKB")
	s2::as_s2_geography(b, ..., oriented = oriented)
}

# dynamically exported in tidyverse.R
as_s2_geography.sfc <- function(x, ..., oriented = getOption("s2_oriented", FALSE)) {
	st_as_s2.sfc(x, ..., oriented = oriented)
}

# dynamically exported in tidyverse.R
as_s2_geography.sf <- function(x, ..., oriented = getOption("s2_oriented", FALSE)) {
	st_as_s2.sf(x, ..., oriented = oriented)
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
#' @param rebuild logical; call \link[s2]{s2_rebuild} on the geometry (think of this as a \code{st_make_valid} on the sphere)
#' @export
st_as_s2.sfc = function(x, ..., oriented = getOption("s2_oriented", FALSE), rebuild = FALSE) {
	if (!is.na(st_crs(x)) && !st_is_longlat(x))
		x = st_transform(x, ifelse(st_axis_order(), "OGC:CRS84", "EPSG:4326"))
	if (length(x) && nchar(class(x[[1]])[1]) > 2) { # Z, M, ZM:
		message("st_as_s2(): dropping Z and/or M coordinate")
		x = st_zm(x)
	}
	if (rebuild)
		s2::s2_rebuild(s2::as_s2_geography(st_as_binary(x), ..., oriented = oriented, check = FALSE))
	else
		s2::as_s2_geography(st_as_binary(x), ..., oriented = oriented)
}
