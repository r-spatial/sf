#' get nearest points between pairs of geometries
#' 
#' get nearest points between pairs of geometries
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param y object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param pairwise logical; if \code{FALSE} (default) return nearest points between all pairs, if \code{TRUE}, return nearest points between subsequent pairs.
#' @param ... ignored
#' @seealso \link{st_nearest_feature} for finding the nearest feature
#' @return an \link{sfc} object with all two-point \code{LINESTRING} geometries of point pairs from the first to the second geometry, of length x * y, with y cycling fastest. See examples for ideas how to convert these to \code{POINT} geometries.
#' @details in case \code{x} lies inside \code{y}, when using S2, the end points 
#' are on polygon boundaries, when using GEOS the end point are identical to \code{x}.
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
#' @name st_nearest_points
st_nearest_points.sfc = function(x, y, ..., pairwise = FALSE) {
	stopifnot(st_crs(x) == st_crs(y))
	longlat = isTRUE(st_is_longlat(x))
	if (longlat && sf_use_s2()) {
		ret = if (pairwise)
				s2::s2_minimum_clearance_line_between(x, y)
			else
				do.call(c, lapply(x, s2::s2_minimum_clearance_line_between, y))
		st_as_sfc(ret, crs = st_crs(x))
	} else {
		if (longlat)
			message_longlat("st_nearest_points")
		st_sfc(CPL_geos_nearest_points(x, st_geometry(y), pairwise), crs = st_crs(x))
	}
}
 
#' @export
#' @name st_nearest_points
st_nearest_points.sfg = function(x, y, ...) {
	st_nearest_points(st_geometry(x), st_geometry(y), ...)
}

#' @export
#' @name st_nearest_points
st_nearest_points.sf = function(x, y, ...) {
	st_nearest_points(st_geometry(x), st_geometry(y), ...)
}

#' get index of nearest feature
#' 
#' get index of nearest feature
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param y object of class \code{sfg}, \code{sfc} or \code{sf}; if missing, features in \code{x} will be compared to all remaining features in \code{x}.
#' @param ... ignored
#' @param check_crs logical; should \code{x} and \code{y} be checked for CRS equality?
#' @param longlat logical; does \code{x} have ellipsoidal coordinates?
#' @return for each feature (geometry) in \code{x} the index of the nearest feature (geometry) in 
#' set \code{y}, or in the remaining set of \code{x} if \code{y} is missing;
#' empty geometries result in \code{NA} indexes
#' @seealso \link{st_nearest_points} for finding the nearest points for pairs of feature geometries
#' @export
#' @examples
#' ls1 = st_linestring(rbind(c(0,0), c(1,0)))
#' ls2 = st_linestring(rbind(c(0,0.1), c(1,0.1)))
#' ls3 = st_linestring(rbind(c(0,1), c(1,1)))
#' (l = st_sfc(ls1, ls2, ls3))
#' 
#' p1 = st_point(c(0.1, -0.1))
#' p2 = st_point(c(0.1, 0.11))
#' p3 = st_point(c(0.1, 0.09))
#' p4 = st_point(c(0.1, 0.9))
#' 
#' (p = st_sfc(p1, p2, p3, p4))
#' try(st_nearest_feature(p, l))
#' try(st_nearest_points(p, l[st_nearest_feature(p,l)], pairwise = TRUE))
#' 
#' r = sqrt(2)/10
#' b1 = st_buffer(st_point(c(.1,.1)), r)
#' b2 = st_buffer(st_point(c(.9,.9)), r)
#' b3 = st_buffer(st_point(c(.9,.1)), r)
#' circles = st_sfc(b1, b2, b3)
#' plot(circles, col = NA, border = 2:4)
#' pts = st_sfc(st_point(c(.3,.1)), st_point(c(.6,.2)), st_point(c(.6,.6)), st_point(c(.4,.8)))
#' plot(pts, add = TRUE, col = 1)
#' # draw points to nearest circle:
#' nearest = try(st_nearest_feature(pts, circles))
#' if (inherits(nearest, "try-error")) # GEOS 3.6.1 not available
#'   nearest = c(1, 3, 2, 2)
#' ls = st_nearest_points(pts, circles[nearest], pairwise = TRUE)
#' plot(ls, col = 5:8, add = TRUE)
#' # compute distance between pairs of nearest features:
#' st_distance(pts, circles[nearest], by_element = TRUE)
st_nearest_feature = function(x, y, ..., check_crs = TRUE, longlat = isTRUE(st_is_longlat(x))) {

	if (missing(y)) { # https://github.com/r-spatial/s2/issues/111#issuecomment-835306261
		longlat = force(longlat) # evaluate only once
		x = st_geometry(x)
		ind <- vapply(
			seq_along(x),
			function(i) st_nearest_feature(x[i], x[-i], check_crs = FALSE, longlat = longlat),
			integer(1)
		)
		ifelse(ind >= seq_along(x), ind + 1, ind)
	} else {
		if (check_crs)
			stopifnot(st_crs(x) == st_crs(y))
		if (longlat && sf_use_s2())
			s2::s2_closest_feature(x, y)
		else {
			if (longlat)
				message_longlat("st_nearest_feature")
			CPL_geos_nearest_feature(st_geometry(x), st_geometry(y))
		}
	}
}
