
#' jitter geometries
#' @param x object of class \code{sf} or \code{sfc}
#' @param amount numeric; amount of jittering applied; if missing, the amount is set to factor * the bounding box diagonal; units of coordinates.
#' @param factor numeric; fractional amount of jittering to be applied
#' @details jitters coordinates with an amount such that `code{runif(1, -amount, amount)} is added to the coordinates. x- and y-coordinates are jittered independently but all coordinates of a single geometry are jittered with the same amount, meaning that the geometry shape does not change. For longlat data, a latitude correction is made such that jittering in East and North directions are identical in distance in the center of the bounding box of \code{x}.
#' @examples
#' nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"))
#' pts = st_centroid(st_geometry(nc))
#' plot(pts)
#' plot(st_jitter(pts, .05), add = TRUE, col = 'red')
#' plot(st_geometry(nc))
#' plot(st_jitter(st_geometry(nc), factor = .01), add = TRUE, col = '#ff8888')
#' @export
st_jitter = function(x, amount, factor = 0.002) {      
	stopifnot(inherits(x, "sf") || inherits(x, "sfc"))
	bb = st_bbox(x)
	if (missing(amount))
		amount = factor * sqrt(diff(bb[c(3,1)])^2 + diff(bb[c(4,2)])^2)
	ay = amount
	ax = if (isTRUE(st_is_longlat(x)))
		amount * cos(pi * mean(bb[c(2,4)]) / 180)
	else
		amount
    f = function(z, amount_x, amount_y) {
		st_point(c(runif(1L, -amount_x, amount_x),
		  runif(1L, -amount_y, amount_y)))
	}
	geom = st_geometry(x)
	geom = st_set_crs(geom + lapply(geom, f, amount_x = ax, amount_y = ay), st_crs(x))
	if (inherits(x, "sf"))
		st_set_geometry(x, geom)
	else
		geom
}
