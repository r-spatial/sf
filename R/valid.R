#' @name geos_query
#' @param NA_on_exception logical; if TRUE, for polygons that would otherwise raise a GEOS error (exception, e.g. for a POLYGON having more than zero but less than 4 points, or a LINESTRING having one point) return an \code{NA} rather than raising an error, and suppress warning messages (e.g. about self-intersection); if FALSE, regular GEOS errors and warnings will be emitted.
#' @param reason logical; if \code{TRUE}, return a character with, for each geometry, the reason for invalidity, \code{NA} on exception, or \code{"Valid Geometry"} otherwise.
#' @return \code{st_is_valid} returns a logical vector indicating for each geometries of \code{x} whether it is valid.
#' @export
#' @examples
#' p1 = st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))")
#' st_is_valid(p1)
#' st_is_valid(st_sfc(st_point(0:1), p1[[1]]), reason = TRUE)
st_is_valid = function(x, NA_on_exception = TRUE, reason = FALSE) {
	if (reason) {
		if (NA_on_exception) {
			g = st_geometry(x)
			ret = rep(NA_character_, length(g))
			not_na = !is.na(st_is_valid(g))
			ret[not_na] = st_is_valid(g[not_na], FALSE, TRUE)
			ret
		} else
			CPL_geos_is_valid_reason(st_geometry(x))
	} else if (! NA_on_exception)
		CPL_geos_is_valid(st_geometry(x), as.logical(NA_on_exception))
	else {
		x = st_geometry(x)
		ret = vector("logical", length(x))
		for (i in seq_along(x))
			ret[i] = CPL_geos_is_valid(x[i], as.logical(NA_on_exception))
		ret
	}
}

#' Make an invalid geometry valid
#' @name valid
#' @param x object of class \code{sfg}, \code{sfg} or \code{sf}
#' @return Object of the same class as \code{x}
#' @details \code{st_make_valid} uses the \code{lwgeom_makevalid} method also used by the PostGIS command \code{ST_makevalid}. It is only available if the package was linked against liblwgeom, which is currently not the case for the binary CRAN distributions; see the package source code repository for instructions how to install liblwgeom. The example below shows how to run-time check the availability of liblwgeom.
#' @examples
#' x = st_sfc(st_polygon(list(rbind(c(0,0),c(0.5,0),c(0.5,0.5),c(0.5,0),c(1,0),c(1,1),c(0,1),c(0,0)))))
#' if (!is.na(sf_extSoftVersion()["lwgeom"])) {
#'   suppressWarnings(st_is_valid(x))
#'   y = st_make_valid(x)
#'   st_is_valid(y)
#'   y %>% st_cast()
#' }
#' @export
st_make_valid = function(x) UseMethod("st_make_valid")

#' @export
st_make_valid.sfg = function(x) {
	st_make_valid(st_geometry(x))[[1]]
}

#' @export
st_make_valid.sfc = function(x) {
	st_sfc(CPL_make_valid(x), crs = st_crs(x))
}

#' @export
st_make_valid.sf = function(x) {
	st_set_geometry(x, st_make_valid(st_geometry(x)))
}
