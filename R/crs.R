# this function establishes whether two crs objects are semantically identical. This is
# the case when: (1) they are completely identical, or (2) they have identical proj4string
# but one of them has a missing epsg ID.
#' @export
Ops.crs <- function(e1, e2) {
	if (nargs() == 1)
		stop(paste("unary", .Generic, "not defined for \"crs\" objects"))

	cmp <- switch(.Generic, "==" =, "!=" = TRUE, FALSE)
	if (!cmp)
		stop(paste("operation", .Generic, "not supported for crs objects"))
	if (.Generic == "!=")
		!(e1 == e2)
	else { # "==": check semantic equality
		if (isTRUE(all.equal(e1, e2))) # includes both are NA_crs_
			TRUE
		else if (is.na(e1) || is.na(e2)) # only one of them is NA_crs_
			FALSE
		else if (e1$proj4string == e2$proj4string && (is.na(e1$epsg) || is.na(e2$epsg)))
			TRUE
		else
			FALSE
	}
}

#' Retrieve coordinate reference system from object
#'
#' Retrieve coordinate reference system from sf or sfc object
#' @name st_crs
#' @param x numeric, character, or object of class \link{sf} or \link{sfc}
#' @param ... ignored
#' @export
#' @return if \code{x} is numeric, return \code{crs} object for SRID \code{x}; if \code{x} is character, return \code{crs} object for proj4string \code{x}; if \code{wkt} is given, return \code{crs} object for well-known-text representation \code{wkt}; if \code{x} is of class \code{sf} or \code{sfc}, return its \code{crs} object.
#' @details the *crs functions create, get, set or replace the \code{crs} attribute of a simple feature geometry
#' list-column. This attribute is of class \code{crs}, and is a list consisting of epsg (integer epsg
#' code) and proj4string (character). 
#' Two objects of class \code{crs} are semantically identical when: (1) they are completely identical, or 
#' (2) they have identical proj4string but one of them has a missing epsg ID. As a consequence, equivalent 
#' but different proj4strings, e.g. \code{ "+proj=longlat +datum=WGS84" } and \code{ "+datum=WGS84 +proj=longlat" },
#' are considered different.
#' The operators \code{==} and \code{!=} are overloaded for \code{crs} objects to establish semantical identity.
#' @return object of class \code{crs}, which is a list with elements epsg (length-1 integer) and 
#' proj4string (length-1 character).
st_crs = function(x, ...) UseMethod("st_crs")

#' @name st_crs
#' @export
st_crs.sf = function(x, ...) st_crs(st_geometry(x, ...))

#' @name st_crs
#' @export
st_crs.numeric = function(x, ...) make_crs(x)

#' @name st_crs
#' @export
#' @param wkt character well-known-text representation of the crs
st_crs.character = function(x, ..., wkt) {
	if (missing(wkt))
		make_crs(x)
	else
		make_crs(wkt, wkt = TRUE)
}

#' @name st_crs
#' @export
st_crs.sfc = function(x, ...) attr(x, "crs")

#' @name st_crs
#' @export
st_crs.bbox = function(x, ...) attr(x, "crs")

#' @name st_crs
#' @export
st_crs.crs = function(x, ...) x

#' @export
st_crs.default = function(x, ...) NA_crs_

#' Set or replace coordinate reference system from object
#'
#' Set or replace retrieve coordinate reference system from object
#' @name st_crs
#' @param value one of (i) character: a valid proj4string (ii) integer, a valid epsg value (numeric), or (iii) a list containing named elements proj4string (character) and/or epsg (integer) with (i) and (ii).
#' @details in case a coordinate reference system is replaced, no transformation takes
#' place and a warning is raised to stress this. epsg values are either read from proj4strings
#' that contain \code{+init=epsg:...} or set to 4326 in case the proj4string contains +proj=longlat
#' and +datum=WGS84, literally
#' 
#' If both epsg and proj4string are provided, they are assumed to be consistent. In processing them, the epsg code, if not missing valued, is used and the proj4string is derived from it by a call to GDAL (which in turn will call PROJ.4). Warnings are raised when epsg is not consistent with a proj4string that is already present.
#' @export
`st_crs<-` = function(x, value) UseMethod("st_crs<-")

#' @name st_crs
#' @examples
#' sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
#' sf = st_sf(a = 1:2, geom = sfc)
#' st_crs(sf) = 4326
#' st_geometry(sf)
#' @export
`st_crs<-.sf` = function(x, value) {
	st_crs(x[[ attr(x, "sf_column") ]]) = value
	x
}

valid_proj4string = function(p4s) {
	stopifnot(is.character(p4s))
	structure(CPL_proj_is_valid(p4s), names = c("valid", "result"))
}

# return crs object from crs, integer, or character string
make_crs = function(x, wkt = FALSE) {
	if (wkt)
		CPL_crs_from_wkt(x)
	else if (is.na(x))
		NA_crs_
	else if (inherits(x, "crs"))
		x
	else if (is.numeric(x)) 
		CPL_crs_from_epsg(as.integer(x))
	else if (is.character(x)) {
		is_valid = valid_proj4string(x) 
		if (! is_valid$valid)
			stop(paste0("invalid crs: ", x, ", reason: ", is_valid$result))
		CPL_crs_from_proj4string(x)
	} else
		stop(paste("cannot create a crs from an object of class", class(x)))
}

#' @name st_crs
#' @examples
#' sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
#' st_crs(sfc) = 4326
#' sfc
#' @export
`st_crs<-.sfc` = function(x, value) {

	if (is.null(attr(x, "crs")))
		start_crs = NA_crs_
	else
		start_crs = st_crs(x)

	end_crs = make_crs(value)

	if (!is.na(start_crs) && !is.na(end_crs) && start_crs != end_crs)
		warning("st_crs<- : replacing crs does not reproject data; use st_transform for that")

	attr(x, "crs") = end_crs
	x
}

#' @name st_crs
#' @examples
#' sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
#' library(dplyr)
#' x = sfc %>% st_set_crs(4326) %>% st_transform(3857)
#' x
#' @export
st_set_crs = function(x, value) {
	st_crs(x) = value
	x
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

crs_parameters = function(x) {
	stopifnot(!is.na(x))
	ret = structure(CPL_crs_parameters(x$proj4string), 
		names = c("SemiMajor", "InvFlattening", "units_gdal", "IsVertical", "WktPretty", "Wkt"))
	ret$SemiMajor = ret$SemiMajor * make_unit("m")
	ret$ud_unit = switch(ret$units_gdal,
		"Meter"                = make_unit("m"),
		"Foot_US"              = make_unit("US_survey_foot"),
		"Foot (International)" = make_unit("ft"),
		"degree"               = make_unit("arc_degree"),
		stop("unknown unit: please file an issue at http://github.com/edzer/sfr/"))
	ret
}

#' @name st_as_text
#' @param pretty logical; if TRUE, print human-readable well-known-text representation of a coordinate reference system
#' @export
st_as_text.crs = function(x, ..., pretty = FALSE) {
	if (pretty)
		crs_parameters(x)$WktPretty
	else
		crs_parameters(x)$Wkt
}


#' @name st_crs
#' @details
#' \code{NA_crs_} is the \code{crs} object with missing values for epsg and proj4string.
#' @export
NA_crs_ = structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs")

#' @name st_crs
#' @export
#' @method is.na crs
is.na.crs = function(x) {
  is.na(x$epsg) && is.na(x$proj4string) 
}

