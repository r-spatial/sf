#' Shift or re-center geographical coordinates for a Pacific view
#'
#' @description
#'   All longitudes < 0 are added to 360, to avoid for instance parts of Alaska
#'   being represented on the far left and right of a plot because they have
#'   values straddling 180 degrees. In general, using a projected
#'   coordinate reference system is to be preferred, but this method permits a
#'   geographical coordinate reference system to be used. This is the sf
#'   equivalent of \code{\link[sp]{recenter}} in the sp package and
#'   ST_ShiftLongitude in PostGIS.
#'
#' @param x object of class sf or sfc
#' @param ... ignored
#'
#' @export
st_shift_longitude = function(x) {
	ll = st_is_longlat(x)
	if (!ll | is.na(ll))
		stop("'st_shift_longitude' requires non-projected geographic coordinates",
			 call. = FALSE)

	UseMethod("st_shift_longitude")
}

#' @name st_shift_longitude
#' @export
#' @examples
#' ## sfc
#' pt1 = st_point(c(-170, 50))
#' pt2 = st_point(c(170, 50))
#' (sfc = st_sfc(pt1, pt2))
#' sfc = st_set_crs(sfc, 4326)
#' st_shift_longitude(sfc)
#'
st_shift_longitude.sfc = function(x, ...) {
	xcrs = st_crs(x)
	g = (x + c(360, 90)) %% c(360) - c(0, 90)
	st_wrap_dateline(st_set_crs(g - c(180, 0), xcrs)) + c(180, 0)
	st_set_crs(g, xcrs)
}

#' @name st_shift_longitude
#' @export
#' @examples
#' ## sf
#' d = st_as_sf(data.frame(id = 1:2, geometry = sfc))
#' st_shift_longitude(d)
st_shift_longitude.sf = function(x, ...) {
	st_geometry(x) = st_shift_longitude(st_geometry(x))
	return(x)
}
