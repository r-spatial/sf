#' retrieve coordinate reference system from object
#'
#' retrieve coordinate reference system from sf or sfc object.
#' @name crs
#' @param x object of class \link{sf} or \link{sfc}
#' @param ... ignored
#' @export
st_crs = function(x, ...) UseMethod("st_crs")

#' @name crs
#' @export
st_crs.sf = function(x, ...) st_crs(st_geometry(x, ...))

#' @name crs
#' @export
st_crs.sfc = function(x, ...) {
	attr(x, "proj4string")
}

#' set or replace coordinate reference system from object
#'
#' set or replace retrieve coordinate reference system from object
#' @name crs
#' @param value either proj4string (character) or epsg value (numeric)
#' @details in case a coordinate reference system is replaced, no transformation takes
#' place and a warning is raised to stress this. epsg values are either read from proj4strings
#' that contain \code{+init=epsg:...} or set to 4326 in case the proj4string contains +proj=longlat
#' and +datum=WGS84, literally
#' @export
`st_crs<-` = function(x, value) UseMethod("st_crs<-")

#' @name crs
#' @examples
#'  sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
#'  sf = st_sf(a = 1:2, geom = sfc)
#'  st_crs(sf) = 4326
#'  st_geometry(sf)
#' @export
`st_crs<-.sf` = function(x, value) {
	check_replace(st_geometry(x))
	st_crs(x[[ attr(x, "sf_column") ]]) = value
	x
}

#' @name crs
#' @examples
#'  sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
#'  st_crs(sfc) = 4326
#'  sfc
#' @export
`st_crs<-.sfc` = function(x, value) {
	# init:
	if (!(is.numeric(value) || is.character(value)))
		stop("crs should be either numeric (epsg code) or character (proj4string)")
	attr(x, "proj4string") = NA_character_
	attr(x, "epsg") = NA_integer_
	if (! is.na(value)) {
		check_replace(x)
		if (is.numeric(value)) {
			value = as.integer(value)
			attr(x, "epsg") = value
			value = CPL_proj4string_from_epsg(value)
		} else 
			attr(x, "epsg") = epsgFromProj4(value)
		attr(x, "proj4string") = value
	}
	x
}

check_replace = function(x) {
	epsg = attr(x, "epsg")
	proj4string = attr(x, "proj4string")
	if (is.null(epsg) && is.null(proj4string))
		return();
	if (!is.na(epsg) || !is.na(proj4string))
		warning("st_crs: replacing crs does not reproject data; use st_transform for that")
}

epsgFromProj4 = function(x) { # grep EPSG code out of proj4string, or argue about it:
	if (is.null(x))
		return(NA_integer_)
	spl = strsplit(x, " ")[[1]]
	w = grep("+init=epsg:", spl)
	if (length(w) == 1)
		as.numeric(strsplit(spl[w], "+init=epsg:")[[1]][2])
	else {
		if (length(grep("+proj=longlat", x)) == 1 && 
			length(grep("+datum=WGS84",  x)) == 1)
			4326
		else
			NA_integer_
	}
}
