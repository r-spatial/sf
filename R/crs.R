#  alternative, but more limiting from sp/R/CRS-methods.R, https://github.com/edzer/sp/pull/31 @hughjonesd
#  (no longer used)
#identicalCRS1 = function(x, y) {
#  args_x <- strsplit(x, " +")[[1]]
#  args_y <- strsplit(y, " +")[[1]]
#  setequal(args_x, args_y)
#}

# this function establishes whether two crs objects are semantically identical. This is
# the case when: (1) they are completely identical (including NA), or (2) GDAL considers
# them equivalent
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
		else
			isTRUE(try(CPL_crs_equivalent(e1, e2), silent = TRUE)) # use GDAL's srs1->IsSame(srs2)
	}
}

#' Retrieve coordinate reference system from object
#'
#' Retrieve coordinate reference system from sf or sfc object
#' @name st_crs
#' @param x numeric, character, or object of class \link{sf} or \link{sfc}
#' @param ... ignored
#' @export
#' @return If \code{x} is numeric, return \code{crs} object for EPSG:\code{x};
#' if \code{x} is character, return \code{crs} object for \code{x};
#' if \code{x} is of class \code{sf} or \code{sfc}, return its \code{crs} object.
#' @details The *crs functions create, get, set or replace the \code{crs} attribute
#' of a simple feature geometry list-column. This attribute is of class \code{crs},
#' and is a list consisting of \code{input} (user input, e.g. "EPSG:4326" or "WGS84"
#' or a proj4string), and \code{wkt}, an automatically generated wkt2 representation of the crs.
#' If \code{x} is identical to the wkt2 representation, and the CRS has a name, this name
#' is used for the \code{input} field.
#'
#' Comparison of two objects of class \code{crs} uses the GDAL function
#' \code{OGRSpatialReference::IsSame}.
#' @return Object of class \code{crs}, which is a list with elements \code{input} (length-1 character)
#' and \code{wkt} (length-1 character).
#' Elements may be \code{NA} valued; if all elements are \code{NA} the CRS is missing valued, and coordinates are
#' assumed to relate to an arbitrary Cartesian coordinate system.
st_crs = function(x, ...) UseMethod("st_crs")

#' @name st_crs
#' @export
st_crs.sf = function(x, ...) st_crs(st_geometry(x), ...)

#' @name st_crs
#' @export
st_crs.numeric = function(x, ...) {
	if (!is.finite(x))
		NA_crs_
	else
    	make_crs(paste0("EPSG:", x))
}


#' @name st_crs
#' @export
st_crs.character = function(x, ...) {
	if (is.na(x))
		NA_crs_
	else {
		crs = make_crs(x)
		if (is.na(crs))
			stop(paste("invalid crs:", x))
		# if we input wkt2, and CRS has a name, use it:
		if (identical(x, crs$wkt) && !identical(crs$Name, "unknown"))
			crs$input = crs$Name
		crs
	}
}

fix_crs = function(x) {
	if (all(c("epsg", "proj4string") %in% names(x))) {
		message("old-style crs object detected; please recreate object with a recent sf::st_crs()")
		x = unclass(x)
		if (!is.na(x$epsg))
			st_crs(x$epsg)
		else
			st_crs(x$proj4string)
	} else
		x
}


#' @name st_crs
#' @param parameters logical; \code{FALSE} by default; if \code{TRUE} return a list of coordinate reference system parameters, with named elements \code{SemiMajor}, \code{InvFlattening}, \code{units_gdal}, \code{IsVertical}, \code{WktPretty}, and \code{Wkt}
#' @export
st_crs.sfc = function(x, ..., parameters = FALSE) {
	crs = fix_crs(attr(x, "crs"))
	if (parameters) {
		p = crs_parameters(crs)
		p$ud_unit = crs_ud_unit(crs)
		p
	} else
		crs
}

#' @name st_crs
#' @export
st_crs.bbox = function(x, ...) {
	crs = attr(x, "crs")
	if (is.null(crs))
		NA_crs_
	else
		crs
}

#' @name st_crs
#' @export
st_crs.CRS = function(x, ...) {
	if (is.null(comment(x)) || CPL_proj_version() < "6.0.0" || 
			CPL_gdal_version() < "3.0.0")
		st_crs(x@projargs)
	else {
		ret = st_crs(comment(x))
		name = ret$Name
		ret$input = if (name == "unknown")
				x@projargs
			else
				name
		ret
	}
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
#' @param value one of (i) character: a string accepted by GDAL, (ii) integer, a valid EPSG value (numeric), or (iii) an object of class \code{crs}.
#' @details In case a coordinate reference system is replaced, no transformation takes
#' place and a warning is raised to stress this.
#'
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

# return crs object from crs, integer, or character string
make_crs = function(x) {

	if (inherits(x, "CRS")) {
		x = if (is.null(comment(x)) || (CPL_proj_version() < "6.0.0" || 
                    CPL_gdal_version() < "3.0.0"))

				x@projargs
			else
				comment(x) # WKT2
	}
	if (is.numeric(x) && !is.na(x))
		x = paste0("EPSG:", x)
	# return:
	if (is.na(x))
		NA_crs_
	else if (inherits(x, "crs"))
		x
	else if (is.character(x)) {
		if (grepl("+init=epsg:", x) &&
				compareVersion(sf_extSoftVersion()[["proj.4"]], "6.0.0") >= 0 &&
				compareVersion(sf_extSoftVersion()[["proj.4"]], "6.3.1") < 0) { # nocov start FIXME:
			x = strsplit(x, " ")[[1]]
			if (length(x) > 1)
				warning(paste("the following proj4string elements are ignored:",
					paste(x[-1], collapse = " "), "; remove the +init=epsg:XXXX to undo this"))
			x = paste0("EPSG:", as.integer(substr(x[1], 12, 20))) # nocov end
		}
		CPL_crs_from_input(x)
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

	if (is.na(end_crs) && !is.na(start_crs) && isTRUE(st_is_longlat(start_crs)) && any(st_is_full(x)))
		stop("To set the crs to NA, first remove the full polygons; see: st_is_full()")

	structure(x, crs = end_crs)
}

#' @export
`st_crs<-.bbox` = function(x, value) {
	structure(x, crs = make_crs(value))
}


#' @name st_crs
#' @examples
#' sfc = st_sfc(st_point(c(0,0)), st_point(c(1,1)))
#' sfc %>% st_set_crs(4326) %>% st_transform(3857)
#' @export
st_set_crs = function(x, value) {
	st_crs(x) = value
	x
}

#' Assert whether simple feature coordinates are longlat degrees
#'
#' Assert whether simple feature coordinates are longlat degrees
#' @param x object of class \link{sf} or \link{sfc}, or otherwise an object of a class that has an \link{st_crs} method returning a \code{crs} object
#' @return `TRUE` if `x` has geographic coordinates, `FALSE` if it has projected coordinates, or `NA` if \code{is.na(st_crs(x))}.
#' @export
st_is_longlat = function(x) {
	crs = st_crs(x)
	if (is.na(crs))
		NA
	else {
		ret = crs_parameters(crs)$IsGeographic
		if (ret && inherits(x, c("sf", "sfc", "stars")) && !is.null(attr(x, "bbox"))) {
			bb = st_bbox(x)
			# check for potentially meaningless value range:
			eps = sqrt(.Machine$double.eps)
			if (all(!is.na(unclass(bb))) &&
					(bb["xmin"] < (-180-eps) || bb["xmax"] > (360+eps) || bb["ymin"] < (-90-eps) || bb["ymax"] > (90+eps)))
				warning("bounding box has potentially an invalid value range for longlat data")
		}
		ret
	}
}

# a = "b" => a is the proj.4 unit (try: cs2cs -lu); "b" is the udunits2 unit
udunits_from_proj = list(
#   PROJ.4     UDUNITS
	`km` =     as_units("km"),
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
	`ind-ch` = as_units("ind_ch", check_is_valid = FALSE),
	`kilometre` =  as_units("km"),
	`metre` =  as_units("m"),
	`decimetre` =     as_units("dm"),
	`centimetre` =     as_units("cm"),
	`millimetre` =     as_units("mm"),
	`nautical mile` = as_units("nautical_mile"),
	`Statute mile` = as_units("mi"),
 	`US survey inch` =  as_units("us_in", check_is_valid = FALSE),
	`US survey foot` =  as_units("US_survey_foot"),
	`US survey yard` =  as_units("US_survey_yard"),
	`US survey chain` =  as_units("chain"),
	`US survey mile` =  as_units("US_survey_mile"),
	`Indian yard (1937)` = as_units("ind_yd", check_is_valid = FALSE),
	`Indian foot (1937)` = as_units("ind_ft", check_is_valid = FALSE),
	`Indian chain` = as_units("ind_ch", check_is_valid = FALSE)
)

crs_ud_unit = function(x) {
	stopifnot(inherits(x, "crs"))
	if (is.na(x))
		return(NULL)

	x = crs_parameters(x)
	if (!is.null(x$units_gdal)) {
		u = udunits_from_proj[[x$units_gdal]]
		if (is.null(u)) {
			u = try(as_units(x$units_gdal), silent = TRUE)
			if (inherits(u, "try-error"))
				u = NULL
		}
		u
	} else
		NULL #2049
}

crs_parameters = function(x) {
	stopifnot(inherits(x, "crs"))
	if (is.na(x))
		list()
	else {
		ret = CPL_crs_parameters(x)
		units(ret$SemiMajor) = as_units("m")
		units(ret$SemiMinor) = as_units("m")
		ret
	}
}

epsg = function(x) {
	if (is.na(x))
		NA_integer_
	else if (grepl("^EPSG:", x[["input"]]))
	# else if (startsWith(x[["input"]], "EPSG:"))
		as.integer(gsub("^EPSG:(\\d+)\\b.*$", "\\1", x[["input"]]))
	else
		crs_parameters(x)[["epsg"]]
}

proj4string = function(x) {
	if (is.na(x))
		NA_character_
	else
		crs_parameters(x)[["proj4string"]]
}


#' @name st_as_text
#' @param projjson logical; if TRUE, return projjson form (requires GDAL 3.1 and PROJ 6.2), else return well-known-text form
#' @param pretty logical; if TRUE, print human-readable well-known-text representation of a coordinate reference system
#' @export
st_as_text.crs = function(x, ..., projjson = FALSE, pretty = FALSE) {
	if (is.na(x))
		NA_character_
	else if (projjson) {
		if (compareVersion(sf_extSoftVersion()["GDAL"], "3.1.0") == -1 || compareVersion(sf_extSoftVersion()["proj.4"], "6.2.0") == -1)
			stop("ProjJson requires GDAL >= 3.1.0 and PROJ >= 6.2.0")
		crs_parameters(x)$ProjJson
	} else { # wkt:
		if (pretty)
			crs_parameters(x)$WktPretty
		else
			crs_parameters(x)$Wkt
	}
}


#' @name st_crs
#' @details
#' \code{NA_crs_} is the \code{crs} object with missing values for \code{input} and \code{wkt}.
#' @export
NA_crs_ = structure(
	list(input = NA_character_,
		wkt = NA_character_),
	class = "crs")

#' @name st_crs
#' @export
#' @method is.na crs
is.na.crs = function(x) {
	identical(x, NA_crs_)
}

#' @name st_crs
#' @param name element name
#' @export
#' @examples
#' st_crs("EPSG:3857")$input
#' st_crs(3857)$proj4string
#' @details the `$` method for `crs` objects retrieves named elements
#' using the GDAL interface; named elements include
#' `SemiMajor`, `SemiMinor`, `InvFlattening`, `IsGeographic`,
#' `units_gdal`, `IsVertical`, `WktPretty`, `Wkt`,
#' `Name`, `proj4string`, `epsg`, `yx`, 
#' `ud_unit`, and `axes` (this may be subject to changes in future GDAL versions).
#'
#' Note that not all valid CRS have a corresponding `proj4string`. 
#'
#' `ud_unit` returns a valid \link[units]{units} object or `NULL` if units are missing.
#' @export
`$.crs` = function(x, name) {

	if (!is.null(x[["proj4string"]])) { # old-style object:
		warning("CRS uses proj4string, which is deprecated.")
		x = st_crs(x[["proj4string"]]) # FIXME: should this be only for some transition period? Add test?
	}
	if (name == "ud_unit")
		crs_ud_unit(x)
	else if (is.na(x))
		NA_character_
	else if (is.numeric(name) || name %in% names(x))
		x[[name]]
	else {
		p = crs_parameters(x)
		if (name %in% names(p))
			p[[name]]
		else {
			tryNum = function(x) { n = suppressWarnings(as.numeric(x)); if (is.na(n)) x else n }
			p4s = strsplit(p$proj4string, " ")[[1]]
			p4s2 = strsplit(p4s, "=")
			vals = lapply(p4s2, function(x) if (length(x) == 1) TRUE else tryNum(x[2]))
			names(vals) = substring(sapply(p4s2, function(x) x[1]), 2)
			vals[[name]]
		}
	}
}

#' @export
print.crs = function(x, ...) {
  cat("Coordinate Reference System:")
  if (is.na(x)) {
    cat(" NA\n")
  } else {
    cat("\n")
    if (is.na(x$input))
       cat("  No user input\n")
    else
       cat("  User input:", x$input, "\n")

	# print wkt:
    if (!is.na(x$wkt))
      cat("  wkt:\n", x$wkt, "\n", sep = "")
  }
}

#' @name st_crs
#' @export
#' @details format.crs returns NA if the crs is missing valued, or else
#' the name of a crs if it is different from "unknown", or
#' else the user input if it was set, or else its "proj4string" representation;
format.crs = function(x, ...) {
	if (is.na(x))
		NA_character_
	else {
		p = crs_parameters(x)
		if (p$Name == "unknown") {
			if (x$input == "unknown")
				x$proj4string
			else
				x$input
		} else
			x$Name
	}
}


#' @export
st_crs.Raster = function(x, ...) {
	crsobj <- raster::crs(x)
	st_crs(crsobj) # nocov
}

#' @export
st_crs.Spatial = function(x, ...) {
	if (! requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	st_crs(x@proj4string) # nocov
}

#' @name st_crs
#' @param authority_compliant logical; specify whether axis order should be
#' handled compliant to the authority; if omitted, the current value is printed.
#' @details
#' \code{st_axis_order} can be used to get and set the axis order: \code{TRUE}
#' indicates axes order according to the authority
#' (e.g. EPSG:4326 defining coordinates to be latitude,longitude pairs), \code{FALSE}
#' indicates the usual GIS (display) order (longitude,latitude). This can be useful
#' when data are read, or have to be written, with coordinates in authority compliant order.
#' The return value is the current state of this (\code{FALSE}, by default).
#' @return \code{st_axis_order} returns the (logical) current value if called without
#' argument, or (invisibly) the previous value if it is being set.
#' @export
#' @examples
#' pt = st_sfc(st_point(c(0, 60)), crs = 4326)
#' # st_axis_order() only has effect in GDAL >= 2.5.0:
#' st_axis_order() # query default: FALSE means interpret pt as (longitude latitude)
#' st_transform(pt, 3857)[[1]]
#' old_value = FALSE
#' if (compareVersion(sf_extSoftVersion()["GDAL"], "2.5.0") >= 0)
#'    (old_value = st_axis_order(TRUE))
#' # now interpret pt as (latitude longitude), as EPSG:4326 prescribes:
#' st_axis_order() # query current value
#' st_transform(pt, 3857)[[1]]
#' st_axis_order(old_value) # set back to old value
st_axis_order = function(authority_compliant = logical(0)) {
	ret = CPL_axis_order_authority_compliant(authority_compliant)
	if (length(authority_compliant))
		invisible(ret)
	else
		ret
}
