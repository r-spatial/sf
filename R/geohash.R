#' compute geohash from (average) coordinates (requires lwgeom)
#'
#' compute geohash from (average) coordinates (requires lwgeom)
#' 
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param precision integer; precision (length) of geohash returned; when omitted, precision 10 is taken.
#' @export
#' @details see \url{http://geohash.org/} or \url{https://en.wikipedia.org/wiki/Geohash}.
#' in case a geometry contains more than one point, the geohash for the average of the points in the geometry is returned.
#' @return character vector with geohashes
#' @examples
#' if (!is.na(sf_extSoftVersion()["lwgeom"])) {
#'  st_geohash(st_sfc(st_point(c(1.5,3.5)), st_point(c(0,90))), 2)
#'  st_geohash(st_sfc(st_point(c(1.5,3.5)), st_point(c(0,90))), 10)
#' }
st_geohash = function(x, precision = 0) {
	CPL_geohash(st_geometry(x), precision)
}
