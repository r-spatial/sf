#' @name valid
#' @param NA_on_exception logical; if TRUE, for polygons that would otherwise raise a GEOS error (exception, e.g. for a POLYGON having more than zero but less than 4 points, or a LINESTRING having one point) return an \code{NA} rather than raising an error, and suppress warning messages (e.g. about self-intersection); if FALSE, regular GEOS errors and warnings will be emitted.
#' @param reason logical; if \code{TRUE}, return a character with, for each geometry, the reason for invalidity, \code{NA} on exception, or \code{"Valid Geometry"} otherwise.
#' @return \code{st_is_valid} returns a logical vector indicating for each geometries of \code{x} whether it is valid. \code{st_make_valid} returns an object with a topologically valid geometry.
#' @export
#' @examples
#' p1 = st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))")
#' st_is_valid(p1)
#' st_is_valid(st_sfc(st_point(0:1), p1[[1]]), reason = TRUE)
st_is_valid = function(x, ...) UseMethod("st_is_valid")

#' @export
#' @name valid
st_is_valid.sfc = function(x, ..., NA_on_exception = TRUE, reason = FALSE) {
	if (sf_use_s2() && isTRUE(st_is_longlat(x))) {
		if (reason) {
			r = s2::s2_is_valid_detail(x)
			r$reason[r$is_valid] = "Valid Geometry"
			r$reason
		} else
			s2::s2_is_valid(x)
	} else if (reason) {
		if (NA_on_exception) {
			ret = rep(NA_character_, length(x))
			not_na = !is.na(st_is_valid(x, reason = FALSE))
			ret[not_na] = CPL_geos_is_valid_reason(x[not_na])
			ret
		} else
			CPL_geos_is_valid_reason(x)
	} else
		CPL_geos_is_valid(x, as.logical(NA_on_exception))
}

#' @export
#' @name valid
st_is_valid.sf = function(x, ...) {
	st_is_valid(st_geometry(x), ...)
}

#' @name valid
#' @export
st_is_valid.sfg = function(x, ...) {
	st_is_valid(st_geometry(x), ...)
}

#' Check validity or make an invalid geometry valid
#'
#' Checks whether a geometry is valid, or makes an invalid geometry valid
#' @name valid
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @return Object of the same class as \code{x}
#' @details For projected geometries, \code{st_make_valid} uses the \code{lwgeom_makevalid} method also used by the PostGIS command \code{ST_makevalid} if the GEOS version linked to is smaller than 3.8.0, and otherwise the version shipped in GEOS; for geometries having ellipsoidal coordinates \code{s2::s2_rebuild} is being used.
#' @examples
#' library(sf)
#' x = st_sfc(st_polygon(list(rbind(c(0,0),c(0.5,0),c(0.5,0.5),c(0.5,0),c(1,0),c(1,1),c(0,1),c(0,0)))))
#' suppressWarnings(st_is_valid(x))
#' y = st_make_valid(x)
#' st_is_valid(y)
#' y %>% st_cast()
#' @export
st_make_valid = function(x, ...) UseMethod("st_make_valid")

#' @export
#' @name valid
st_make_valid.sfg = function(x, ...) {
	st_make_valid(st_geometry(x), ...)[[1]]
}

#' @name valid
#' @export
#' @param ... passed on to \link[s2]{s2_options}
#' @param oriented logical; only relevant if \code{st_is_longlat(x)} is \code{TRUE}; see \link{s2}
#' @param s2_options only relevant if \code{st_is_longlat(x)} is \code{TRUE}; options for \link[s2]{s2_rebuild}, see \link[s2]{s2_options} and Details.
#' @param geos_method character; either "valid_linework" (Original method, combines all rings into a set of noded lines and then extracts valid polygons from that linework) or "valid_structure" (Structured method, first makes all rings valid then merges shells and subtracts holes from shells to generate valid result. Assumes that holes and shells are correctly categorized.) (requires GEOS >= 3.10.1)
#' @param geos_keep_collapsed logical; When this parameter is not set to \code{FALSE}, the "valid_structure" method will keep any component that has collapsed into a lower dimensionality. For example, a ring collapsing to a line, or a line collapsing to a point (requires GEOS >= 3.10.1)
#' @details if \code{s2_options} is not specified and \code{x} has a non-zero precision set, then this precision value will be used as the value in \code{s2_snap_precision}, passed on to \code{s2_options}, rather than the 1e7 default.
st_make_valid.sfc = function(x, ..., oriented = FALSE, s2_options = s2::s2_options(snap = s2::s2_snap_precision(1e7), ...),
							 geos_method = "valid_structure", geos_keep_collapsed = TRUE) {
	crs = st_crs(x)
	if (sf_use_s2() && isTRUE(st_is_longlat(x))) {
		stopifnot(missing(geos_method), missing(geos_keep_collapsed))
		s2 = s2::as_s2_geography(st_as_binary(st_set_precision(x, 0.0)), oriented = oriented, check = FALSE)
		if (st_precision(x) != 0 && missing(s2_options))
			s2_options = s2::s2_options(snap = s2::s2_snap_precision(st_precision(x)), ...)
		s2 = s2::s2_rebuild(s2, s2_options)
		st_as_sfc(s2, crs = crs)
	} else if (compareVersion(CPL_geos_version(), "3.8.0") == -1) {
		stopifnot(missing(geos_method), missing(geos_keep_collapsed))
		if (!requireNamespace("lwgeom", quietly = TRUE))
			stop("package lwgeom required, please install it first") # nocov
		st_sfc(lwgeom::lwgeom_make_valid(x), crs = crs)
	} else
		st_sfc(CPL_geos_make_valid(x, geos_method, geos_keep_collapsed), crs = crs)
}

#' @export
st_make_valid.sf = function(x, ...) {
	st_set_geometry(x, st_make_valid(st_geometry(x)))
}
