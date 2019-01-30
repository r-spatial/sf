#  alternative, but more limiting from sp/R/CRS-methods.R, https://github.com/edzer/sp/pull/31 @hughjonesd
#  (no longer used)
#identicalCRS1 = function(x, y) {
#  args_x <- strsplit(x, " +")[[1]]
#  args_y <- strsplit(y, " +")[[1]]
#  setequal(args_x, args_y)
#}

# this function establishes whether two crs objects are semantically identical. This is
# the case when: (1) they are completely identical, or (2) they have identical proj4string
# but one of them has a missing epsg ID.
#' @export
Ops.crs <- function(e1, e2) {
	if (nargs() == 1)
		stop(paste("unary", .Generic, "not defined for \"crs\" objects"), call. = FALSE)

	cmp <- switch(.Generic, "==" =, "!=" = TRUE, FALSE)
	if (!cmp)
		stop(paste("operation", .Generic, "not supported for crs objects"), call. = FALSE)
	if (.Generic == "!=")
		!(e1 == e2)
	else { # "==": check semantic equality
		if (isTRUE(all.equal(e1, e2))) # includes both are NA_crs_
			TRUE
		else if (is.na(e1) || is.na(e2)) # only one of them is NA_crs_
			FALSE
		else CPL_crs_equivalent(e1$proj4string, e2$proj4string) # use GDAL's srs1->IsSame(srs2)
	}
}

#' Retrieve coordinate reference system from object
#'
#' Retrieve coordinate reference system from sf or sfc object
#' @name st_crs
#' @param x numeric, character, or object of class \link{sf} or \link{sfc}
#' @param ... ignored
#' @param valid default TRUE. This allows to create crs without checking against
#' the local proj4 database. It can be used to synchronize crs with a remote
#' database, but avoid it as much as possible.
#' @param proj4text character. Must be used in conjunction with \code{valid = FALSE}.
#' @export
#' @return If \code{x} is numeric, return \code{crs} object for SRID \code{x}; if \code{x} is character, return \code{crs} object for proj4string \code{x}; if \code{wkt} is given, return \code{crs} object for well-known-text representation \code{wkt}; if \code{x} is of class \code{sf} or \code{sfc}, return its \code{crs} object.
#' @details The *crs functions create, get, set or replace the \code{crs} attribute of a simple feature geometry
#' list-column. This attribute is of class \code{crs}, and is a list consisting of \code{epsg} (integer EPSG
#' code) and \code{proj4string} (character).
#' Two objects of class \code{crs} are semantically identical when: (1) they are completely identical, or
#' (2) they have identical proj4string but one of them has a missing EPSG ID. As a consequence, equivalent
#' but different proj4strings, e.g. \code{ "+proj=longlat +datum=WGS84" } and \code{ "+datum=WGS84 +proj=longlat" },
#' are considered different.
#' The operators \code{==} and \code{!=} are overloaded for \code{crs} objects to establish semantical identity.
#' @return Object of class \code{crs}, which is a list with elements \code{epsg} (length-1 integer) and
#' \code{proj4string} (length-1 character).
st_crs = function(x, ...) UseMethod("st_crs")

#' @name st_crs
#' @export
st_crs.sf = function(x, ...) st_crs(st_geometry(x), ...)

#' @name st_crs
#' @export
st_crs.numeric = function(x, proj4text = "", valid = TRUE, ...) {
    if (!valid)
        return(structure(list(epsg = x, proj4string = proj4text), class = "crs"))
    if (proj4text != "")
        warning("`proj4text` is not used to validate crs. Remove `proj4text` ",
                "argument or set `valid = FALSE` to stop warning.")
    make_crs(x)
}

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
#' @param parameters logical; \code{FALSE} by default; if \code{TRUE} return a list of coordinate reference system parameters, with named elements \code{SemiMajor}, \code{InvFlattening}, \code{units_gdal}, \code{IsVertical}, \code{WktPretty}, and \code{Wkt}
#' @export
st_crs.sfc = function(x, ..., parameters = FALSE) {
	crs = attr(x, "crs")
	if (parameters) {
		if (is.na(crs))
			list()
		else
			crs_parameters(crs)
	} else
		crs
}

#' @name st_crs
#' @export
st_crs.bbox = function(x, ...) {
	if (is.null(attr(x, "crs")))
		NA_crs_
	else
		attr(x, "crs")
}

#' @name st_crs
#' @export
st_crs.crs = function(x, ...) x

#' @export
st_crs.default = function(x, ...) NA_crs_

#' Set or replace coordinate reference system from object
#'
#' Set or replace retrieve coordinate reference system from object
#' @name st_crs
#' @param value one of (i) character: a valid proj4string (ii) integer, a valid EPSG value (numeric), or (iii) a list containing named elements \code{proj4string} (character) and/or \code{epsg} (integer) with (i) and (ii).
#' @details In case a coordinate reference system is replaced, no transformation takes
#' place and a warning is raised to stress this. EPSG values are either read from proj4strings
#' that contain \code{+init=epsg:...} or set to 4326 in case the proj4string contains +proj=longlat
#' and +datum=WGS84, literally.
#'
#' If both \code{epsg} and \code{proj4string} are provided, they are assumed to be consistent. In processing them, the EPSG code, if not missing valued, is used and the proj4string is derived from it by a call to GDAL (which in turn will call PROJ.4). Warnings are raised when \code{epsg} is not consistent with a proj4string that is already present.
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
			stop(paste0("invalid crs: ", x, ", reason: ", is_valid$result), call. = FALSE)
		u = `$.crs`(list(proj4string = x), "units")
		crs = CPL_crs_from_proj4string(x)
		if (! is.null(u) && crs$units != u) # gdal converts unrecognized units into m...
			stop(paste0("units ", u, " not recognized: older GDAL version?"), call. = FALSE) # nocov
		crs
	} else
		stop(paste("cannot create a crs from an object of class", class(x)), call. = FALSE)
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
		warning("st_crs<- : replacing crs does not reproject data; use st_transform for that", call. = FALSE)

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
	else {
		ret = isTRUE(crs$proj == "longlat")
		if (ret && inherits(x, c("sf", "sfc", "stars"))) {
			bb = st_bbox(x)
			# check for potentially meaningless value range:
			if (all(!is.na(unclass(bb))) && 
					(bb["xmin"] < -180 || bb["xmax"] > 360 || bb["ymin"] < -90 || bb["ymax"] > 90))
				warning("bounding box has potentially an invalid value range for longlat data")
		}
		ret
	}
}

# a = "b" => a is the proj.4 unit (try: cs2cs -lu); "b" is the udunits2 unit
udunits_from_proj = list(
#   PROJ.4     UDUNITS
	`km` =    as_units("km"),
	`m` =      as_units("m"),
	`dm` =     as_units("dm"),
	`cm` =     as_units("cm"),
	`mm` =     as_units("mm"),
	`kmi` =    as_units("nautical_mile"),
	`in` =     as_units("in"),
	`ft` =     as_units("ft"),
	`yd` =     as_units("yd"),
	`mi` =     as_units("mi"),
	`fath` =   as_units("fathom"),
	`ch` =     as_units("chain"),
	`link` =   as_units("link", check_is_valid = FALSE), # not (yet) existing; set in .onLoad()
 	`us-in` =  as_units("us_in", check_is_valid = FALSE),
	`us-ft` =  as_units("US_survey_foot"),
	`us-yd` =  as_units("US_survey_yard"),
	`us-ch` =  as_units("chain"),
	`us-mi` =  as_units("US_survey_mile"),
	`ind-yd` = as_units("ind_yd", check_is_valid = FALSE),
	`ind-ft` = as_units("ind_ft", check_is_valid = FALSE),
	`ind-ch` = as_units("ind_ch", check_is_valid = FALSE)
)

crs_parameters = function(x) {
	stopifnot(!is.na(x))
	ret = structure(CPL_crs_parameters(x$proj4string),
		names = c("SemiMajor", "SemiMinor", "InvFlattening", "units_gdal", 
			"IsVertical", "WktPretty", "Wkt"))
	units(ret$SemiMajor) = as_units("m")
	units(ret$SemiMinor) = as_units("m")
	ret$ud_unit = if (isTRUE(st_is_longlat(x)))
			as_units("arc_degree")
		else if (is.null(x$units))
			as_units("m")
		else {
			if (is.character(udunits_from_proj[[x$units]]))
				as_units(udunits_from_proj[[x$units]])
			else
				udunits_from_proj[[x$units]]
		}
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
#' \code{NA_crs_} is the \code{crs} object with missing values for \code{epsg} and \code{proj4string}.
#' @export
NA_crs_ = structure(list(epsg = NA_integer_, proj4string = NA_character_), class = "crs")

#' @name st_crs
#' @export
#' @method is.na crs
is.na.crs = function(x) {
  is.na(x$epsg) && is.na(x$proj4string)
}

#' @name st_crs
#' @param name element name; \code{epsg} or \code{proj4string}, or one of \code{proj4strings} named components without the \code{+}; see examples
#' @export
#' @examples
#' st_crs("+init=epsg:3857")$epsg
#' st_crs("+init=epsg:3857")$proj4string
#' st_crs("+init=epsg:3857 +units=km")$b     # numeric
#' st_crs("+init=epsg:3857 +units=km")$units # character
#' @export
`$.crs` = function(x, name) {
	if (is.numeric(name) || name %in% names(x))
		x[[name]]
	else {
		tryNum = function(x) { n = suppressWarnings(as.numeric(x)); if (is.na(n)) x else n }
		p4s = strsplit(x$proj4string, " ")[[1]]
		p4s2 = strsplit(p4s, "=")
		vals = lapply(p4s2, function(x) if (length(x) == 1) TRUE else tryNum(x[2]))
		names(vals) = substring(sapply(p4s2, function(x) x[1]), 2)
		vals[[name]]
	}
}

#' @export
print.crs = function(x, ...) {
  cat("Coordinate Reference System:")
  if (is.na(x)) {
    cat(" NA\n")
  } else {
    cat("\n")
    if (is.na(x$epsg))
       cat("  No EPSG code\n")
    else
       cat("  EPSG:", x$epsg, "\n")
    if (is.na(x$proj4string))
      stop("  invalid crs: please report an issue") # nocov
    else
      cat("  proj4string: \"", x$proj4string, "\"\n", sep = "")
  }
}

#' @export
st_crs.Raster = function(x, ...) {
	st_crs(x@crs@projargs) # nocov
}

#' @export
st_crs.Spatial = function(x, ...) {
	if (! requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	st_crs(sp::proj4string(x)) # nocov
}
