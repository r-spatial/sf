#' Retrieve coordinate reference system from object
#'
#' Retrieve coordinate reference system from sf or sfc object
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

#' Set or replace coordinate reference system from object
#'
#' Set or replace retrieve coordinate reference system from object
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
	trim = function(x) gsub("^\\s+|\\s+$", "", x)
	
	# Make sure crs exists
	if (is.null(attr(x, "proj4string")))
		attr(x, "proj4string") = NA_character_
	if (is.null(attr(x, "epsg")))
		attr(x, "epsg") = NA_integer_

	if (attr(x,"epsg") == 0)
		attr(x, "epsg") = NA_integer_

	attr(x, "proj4string") = trim(attr(x, "proj4string"))
	start_crs = st_crs(x)
	
	if (is.na(value)) {
		# Do nothing
	} else if (is.numeric(value) && value != 0) { # epsg
		attr(x, "proj4string") = trim(CPL_proj4string_from_epsg(value))
		attr(x, "epsg") = as.integer(value)
	} else if (is.character(value)) { # proj4string
		attr(x, "proj4string") = trim(value)
		attr(x, "epsg") = epsg_from_proj4string(value)
	} else if (class(value) == "crs") { # crs
		attr(x, "proj4string") = trim(value$proj4string)
		attr(x, "epsg") = value$epsg
	} else {
		stop("crs should be either integer (epsg), character (proj4string), or crs object")
	}

	end_crs = st_crs(x)

	# Warn on replacement
	if ( (!is.na(start_crs$epsg) && start_crs$epsg != end_crs$epsg) ||
		 (!is.na(start_crs$proj4string) && start_crs$proj4string != end_crs$proj4string) )
	{
		warning("st_crs: replacing crs does not reproject data; use st_transform for that")
	}

	x
}


crs_from_list = function(x) {
	trim = function(x) gsub("^\\s+|\\s+$", "", x)

	if (is.null(x$proj4string) && is.null(x$epgs)) {
		structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs")
	} else if ( is.null(x$proj4string) && !is.null(x$epgs)) {
		proj4string = trim(as.character(x$proj4string))
		epsg = epsg_from_proj4string(proj4string)
		structure(list(epsg = epsg, proj4string = proj4string), class = "crs")
	} else if (!is.null(x$proj4string) &&  is.null(x$epgs)) {
		epsg = as.integer(x$epsg)
		proj4string = trim(CPL_proj4string_from_epsg(epsg))
		structure(list(epsg = epsg, proj4string = proj4string), class = "crs")
	} else if (!is.null(x$proj4string) && !is.null(x$epgs)) {
		proj4string = trim(as.character(x$proj4string))
		epsg = as.integer(x$epsg)
		structure(list(epsg = epsg, proj4string = proj4string), class = "crs")
	}
}


epsg_from_proj4string = function(x) { # grep EPSG code out of proj4string, or argue about it:
	if (is.null(x) || !is.character(x))
		return(NA_integer_)
	
	epsg = sub(".*\\+init=epsg:([0-9]+).*","\\1",x)
	if (epsg != x) {
		as.numeric(epsg)
	} else if (grepl("+proj=longlat", x) && grepl("+datum=WGS84",  x)) {
		4326
	} else {
	 	NA_integer_
	}
}

#' Assert whether simple feature coordinates are longlat degrees
#' 
#' Assert whether simple feature coordinates are longlat degrees
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
