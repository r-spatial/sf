#' retrieve coordinate reference system from object
#'
#' retrieve coordinate reference system from object
#' @param x object of class \link{sf} or \link{sfc}
#' @param ... ignored
#' @export
st_crs = function(x, ...) UseMethod("st_crs")

#' @export
st_crs.sf = function(x, ...) st_crs(st_geometry(x, ...))

#' @export
st_crs.sfc = function(x, ...) {
	attr(x, "proj4string")
}

#' set or replace coordinate reference system from object
#'
#' set or replace retrieve coordinate reference system from object
#' @param x object of class \link{sf} or \link{sfc}
#' @param value either proj4string (character) or epsg value (numeric)
#' @details in case a coordinate reference system is replaced, no transformation takes
#' place and a warning is raised to stress this. epsg values are either read from proj4strings
#' that contain \code{+init=epsg:...} or set to 4326 in case the proj4string contains +proj=longlat
#' and +datum=WGS84, literally
#' @export
`st_crs<-` = function(x, value) UseMethod("st_crs<-")

#' @export
`st_crs<-.sf` = function(x, value) {
	check_replace(st_geometry(x))
	col = attr(x, "sf_column")
	if (is.numeric(value)) {
		attr(x[[col]], "epsg") = value
		value = CPL_proj4string_from_epsg(value)
	}
	if (!is.character(value))
		stop("crs should be either numeric (epsg code) or character (proj4string)")
	attr(x[[col]], "proj4string") = value
	x
}

#' @export
`st_crs<-.sfc` = function(x, value) {
	check_replace(x)
	if (is.numeric(value)) {
		attr(x, "epsg") = value
		value = CPL_proj4string_from_epsg(value)
	}
	if (!is.character(value))
		stop("crs should be either numeric (epsg code) or character (proj4string)")
	attr(x, "proj4string") = value
	x
}

check_replace = function(x) {
	if (!is.na(attr(x, "epsg")) && !is.na(attr(x, "proj4string")))
		warning("st_crs: replacing crs does not reproject data; use st_transform for that")
}
