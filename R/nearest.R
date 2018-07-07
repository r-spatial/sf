#' get nearest points between pairs of geometries
#' 
#' get nearest points between pairs of geometries
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param y object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param ... ignored
#' @return an \link{sfc} object with all two-point \code{LINESTRING} geometries of point pairs from the first to the second geometry, of length x * y, with y cycling fastest. See examples for ideas how to convert these to \code{POINT} geometries.
#' @examples
#' r = sqrt(2)/10
#' pt1 = st_point(c(.1,.1))
#' pt2 = st_point(c(.9,.9))
#' pt3 = st_point(c(.9,.1))
#' b1 = st_buffer(pt1, r)
#' b2 = st_buffer(pt2, r)
#' b3 = st_buffer(pt3, r)
#' (ls0 = st_nearest_points(b1, b2)) # sfg
#' (ls = st_nearest_points(st_sfc(b1), st_sfc(b2, b3))) # sfc
#' plot(b1, xlim = c(-.2,1.2), ylim = c(-.2,1.2), col = NA, border = 'green')
#' plot(st_sfc(b2, b3), add = TRUE, col = NA, border = 'blue')
#' plot(ls, add = TRUE, col = 'red')
#' 
#' nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"))
#' plot(st_geometry(nc))
#' ls = st_nearest_points(nc[1,], nc)
#' plot(ls, col = 'red', add = TRUE)
#' pts = st_cast(ls, "POINT") # gives all start & end points
#' # starting, "from" points, corresponding to x:
#' plot(pts[seq(1, 200, 2)], add = TRUE, col = 'blue')
#' # ending, "to" points, corresponding to y:
#' plot(pts[seq(2, 200, 2)], add = TRUE, col = 'green')
#' 
#' @export
st_nearest_points = function(x, y, ...) UseMethod("st_nearest_points")

#' @export
st_nearest_points.sfc = function(x, y, ..., of_nearest_geometry = FALSE) {
	if (of_nearest_geometry)
		y = st_sfc(CPL_geos_nearest_feature(x, y))
	st_sfc(CPL_geos_nearest_points(x, st_geometry(y), of_nearest_geometry))
}
 
#' @export
st_nearest_points.sfg = function(x, y, ...) {
	st_nearest_points(st_geometry(x), st_geometry(y), ...)
}

#' @export
st_nearest_points.sf = function(x, y, ...) {
	st_nearest_points(st_geometry(x), st_geometry(y), ...)
}
