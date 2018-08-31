
#' crop an sf object to a specific rectangle
#' @param x object of class \code{sf} or \code{sfc}
#' @param y numeric vector with named elements \code{xmin}, \code{ymin}, \code{xmax} and \code{ymax}, or object of class \code{bbox}, or object for which there is an \link{st_bbox} method to convert it to a \code{bbox} object
#' @param ... ignored
#' @details
#' setting arguments \code{xmin}, \code{ymin}, \code{xmax} and \code{ymax} implies that argument \code{y} gets ignored.
#' @export
#' @examples
#' box = c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
#' pol = st_sfc(st_buffer(st_point(c(.5, .5)), .6))
#' pol_sf = st_sf(a=1, geom=pol)
#' plot(st_crop(pol, box))
#' plot(st_crop(pol_sf, st_bbox(box)))
#' # alternative:
#' plot(st_crop(pol, xmin = 0, ymin = 0, xmax = 1, ymax = 1))
#' @export
st_crop = function(x, y, ...) UseMethod("st_crop")

#' @export
#' @name st_crop
#' @param xmin minimum x extent of cropping area
#' @param ymin minimum y extent of cropping area
#' @param xmax maximum x extent of cropping area
#' @param ymax maximum y extent of cropping area
st_crop.sfc = function(x, y, ..., xmin, ymin, xmax, ymax) {
	if (!missing(xmin) && !missing(ymin) && !missing(xmax) && !missing(ymax))
		y = c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
	if (! inherits(y, "bbox"))
		y = st_bbox(y)
	if (is.na(st_crs(y)))
		y = structure(y, crs = st_crs(x))
	st_intersection(x, st_as_sfc(y))
}

#' @export
#' @name st_crop
st_crop.sf = function(x, y, ...) {
	st_crop.sfc(x, y, ...)
}
