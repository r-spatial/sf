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
st_crs.sfc = function(x, ...)
	structure(list(epsg = attr(x, "epsg"), proj4string = attr(x, "proj4string")), class = "crs")

#' @export
st_crs.default = function(x, ...) 
	structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs")

#' @export
#' @method is.na crs
is.na.crs = function(x) { is.na(x$epsg) && is.na(x$proj4string) }

#' set or replace coordinate reference system from object
#'
#' set or replace retrieve coordinate reference system from object
#' @name crs
#' @param value one of (i) character: a valid proj4string (ii) integer, a valid epsg value (numeric), or (iii) a list containing named elements proj4string (character) and/or epsg (integer) with (i) and (ii).
#' @details in case a coordinate reference system is replaced, no transformation takes
#' place and a warning is raised to stress this. epsg values are either read from proj4strings
#' that contain \code{+init=epsg:...} or set to 4326 in case the proj4string contains +proj=longlat
#' and +datum=WGS84, literally
#' 
#' If both epsg and proj4string are provided, they are assumed to be consistent. In processing them, the epsg code, if not missing valued, is used and the proj4string is derived from it by a call to GDAL (which in turn will call PROJ.4). Warnings are raised when epsg is not consistent with a proj4string that is already present.
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
	trim <- function (x) gsub("^\\s+|\\s+$", "", x)
	# init:
	if (!is.na(value) && !(is.numeric(value) || is.character(value) || is.list(value)))
		stop("crs should be either numeric (epsg), character (proj4string), or list")
	check_replace(x, value)
	attr(x, "proj4string") = NA_character_
	attr(x, "epsg") = NA_integer_
	if (is.list(value)) { # try to get value from the attribute list:
		if (!is.null(value$epsg) && !is.na(value$epsg))
			value = value$epsg
		else if (!is.null(value$proj4string) && !is.na(value$proj4string))
			value = value$proj4string
		else 
			value = NA_integer_
	}
	if (! is.na(value)) {
		if (is.numeric(value)) {
			value = as.integer(value)
			if (value == 0) {
				attr(x, "epsg") = NA_integer_
				value = NA_character_
			} else {
				attr(x, "epsg") = value
				value = CPL_proj4string_from_epsg(value)
			}
		} else 
			attr(x, "epsg") = epsgFromProj4(value)
		attr(x, "proj4string") = trim(value)
	}
	x
}

check_replace = function(x, value) {
	trim <- function (x) gsub("^\\s+|\\s+$", "", x)
	if (is.na(value) || is.list(value))
		return()
	epsg = attr(x, "epsg")
	proj4string = attr(x, "proj4string")
	if (is.null(epsg) && is.null(proj4string))
		return()
	if (!is.null(epsg) && is.na(epsg) && is.numeric(value) && !is.na(proj4string)
			&& proj4string ==  trim(CPL_proj4string_from_epsg(value)))
		return() # the epsg is "additional" info, but matches the already present proj4string
	if (!is.null(value) && !is.na(value) && is.character(value) 
		&& !is.null(proj4string) && !is.na(proj4string) && trim(value) == trim(proj4string))
		return() # trying to replace proj4string with identical value
	if (!is.na(epsg) || !is.na(proj4string))  # possibly warn:
		warning("st_crs: replacing crs does not reproject data; use st_transform for that")
}

epsgFromProj4 = function(x) { # grep EPSG code out of proj4string, or argue about it:
	if (is.null(x) || !is.character(x))
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

#' assert whether simple feature coordinates are longlat degrees
#' 
#' assert whether simple feature coordinates are longlat degrees
#' @param x object of class \link{sf} or \link{sfc}
#' @return TRUE if \code{+proj=longlat} is part of the proj4string, NA if this string is missing, FALSE otherwise
#' @export
st_is_longlat = function(x) {
	crs = st_crs(x)
	if (is.na(crs))
		NA
	else
		length(grep("+proj=longlat", crs$proj4string)) > 0
}
