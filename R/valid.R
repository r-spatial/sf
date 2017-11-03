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
