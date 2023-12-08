
# unary, returning geometries

#' Geometric unary operations on simple feature geometry sets
#'
#' Geometric unary operations on simple feature geometries. These are all generics, with methods for \code{sfg}, \code{sfc} and \code{sf} objects, returning an object of the same class. All operations work on a per-feature basis, ignoring all other features.
#' @name geos_unary
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param dist numeric; buffer distance for all, or for each of the elements in \code{x}; in case
#' \code{dist} is a \code{units} object, it should be convertible to \code{arc_degree} if
#' \code{x} has geographic coordinates, and to \code{st_crs(x)$units} otherwise
#' @param nQuadSegs integer; number of segments per quadrant (fourth of a circle), for all or per-feature; see details
#' @param endCapStyle character; style of line ends, one of 'ROUND', 'FLAT', 'SQUARE'; see details
#' @param joinStyle character; style of line joins, one of 'ROUND', 'MITRE', 'BEVEL'; see details
#' @param mitreLimit numeric; limit of extension for a join if \code{joinStyle} 'MITRE' is used (default 1.0, minimum 0.0); see details
#' @param singleSide logical; if \code{TRUE}, single-sided buffers are returned for linear geometries,
#' in which case negative \code{dist} values give buffers on the right-hand side, positive on the left; see details
#' @param ... passed on to \code{s2_buffer_cells}
#' @return an object of the same class of \code{x}, with manipulated geometry.
#' @export
#' @details \code{st_buffer} computes a buffer around this geometry/each geometry. If any of \code{endCapStyle},
#' \code{joinStyle}, or \code{mitreLimit} are set to non-default values ('ROUND', 'ROUND', 1.0 respectively) then
#' the underlying 'buffer with style' GEOS function is used.
#' If a negative buffer returns empty polygons instead of shrinking, set st_use_s2() to FALSE
#' See \href{https://postgis.net/docs/ST_Buffer.html}{postgis.net/docs/ST_Buffer.html} for details.
#' 
#' \code{nQuadSegs}, \code{endCapsStyle}, \code{joinStyle}, \code{mitreLimit} and \code{singleSide} only
#' work when the GEOS back-end is used: for projected coordinates or when \code{sf_use_s2()} is set
#' to \code{FALSE}.
#' @examples
#'
#' ## st_buffer, style options (taken from rgeos gBuffer)
#' l1 = st_as_sfc("LINESTRING(0 0,1 5,4 5,5 2,8 2,9 4,4 6.5)")
#' op = par(mfrow=c(2,3))
#' plot(st_buffer(l1, dist = 1, endCapStyle="ROUND"), reset = FALSE, main = "endCapStyle: ROUND")
#' plot(l1,col='blue',add=TRUE)
#' plot(st_buffer(l1, dist = 1, endCapStyle="FLAT"), reset = FALSE, main = "endCapStyle: FLAT")
#' plot(l1,col='blue',add=TRUE)
#' plot(st_buffer(l1, dist = 1, endCapStyle="SQUARE"), reset = FALSE, main = "endCapStyle: SQUARE")
#' plot(l1,col='blue',add=TRUE)
#' plot(st_buffer(l1, dist = 1, nQuadSegs=1), reset = FALSE, main = "nQuadSegs: 1")
#' plot(l1,col='blue',add=TRUE)
#' plot(st_buffer(l1, dist = 1, nQuadSegs=2), reset = FALSE, main = "nQuadSegs: 2")
#' plot(l1,col='blue',add=TRUE)
#' plot(st_buffer(l1, dist = 1, nQuadSegs= 5), reset = FALSE, main = "nQuadSegs: 5")
#' plot(l1,col='blue',add=TRUE)
#' par(op)
#'
#'
#' l2 = st_as_sfc("LINESTRING(0 0,1 5,3 2)")
#' op = par(mfrow = c(2, 3))
#' plot(st_buffer(l2, dist = 1, joinStyle="ROUND"), reset = FALSE, main = "joinStyle: ROUND")
#' plot(l2, col = 'blue', add = TRUE)
#' plot(st_buffer(l2, dist = 1, joinStyle="MITRE"), reset = FALSE, main = "joinStyle: MITRE")
#' plot(l2, col= 'blue', add = TRUE)
#' plot(st_buffer(l2, dist = 1, joinStyle="BEVEL"), reset = FALSE, main = "joinStyle: BEVEL")
#' plot(l2, col= 'blue', add=TRUE)
#' plot(st_buffer(l2, dist = 1, joinStyle="MITRE" , mitreLimit=0.5), reset = FALSE,
#'    main = "mitreLimit: 0.5")
#' plot(l2, col = 'blue', add = TRUE)
#' plot(st_buffer(l2, dist = 1, joinStyle="MITRE",mitreLimit=1), reset = FALSE,
#'    main = "mitreLimit: 1")
#' plot(l2, col = 'blue', add = TRUE)
#' plot(st_buffer(l2, dist = 1, joinStyle="MITRE",mitreLimit=3), reset = FALSE,
#'    main = "mitreLimit: 3")
#' plot(l2, col = 'blue', add = TRUE)
#' par(op)
st_buffer = function(x, dist, nQuadSegs = 30,
					 endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1.0, singleSide = FALSE, ...)
	UseMethod("st_buffer")

#' @export
st_buffer.sfg = function(x, dist, nQuadSegs = 30,
						 endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1.0, singleSide = FALSE, ...)
	get_first_sfg(st_buffer(st_sfc(x), dist, nQuadSegs = nQuadSegs, endCapStyle = endCapStyle,
		joinStyle = joinStyle, mitreLimit = mitreLimit, singleSide = singleSide, ...))

.process_style_opts = function(endCapStyle, joinStyle, mitreLimit, singleSide) {
	styls = list(with_styles = FALSE, endCapStyle = NA, joinStyle = NA, mitreLimit = NA)
	if (endCapStyle == "ROUND" && joinStyle == "ROUND" && mitreLimit == 1
			&& all(singleSide == FALSE))
		return(styls)
	ecs = match(endCapStyle, c("ROUND", "FLAT", "SQUARE"))
	js = match(joinStyle, c("ROUND", "MITRE", "BEVEL"))
	if (is.na(mitreLimit) || !mitreLimit > 0) stop("mitreLimit must be > 0")
	if (is.na(ecs)) stop("endCapStyle must be 'ROUND', 'FLAT', or 'SQUARE'")
	if (is.na(js))  stop("joinStyle must be 'ROUND', 'MITRE', or 'BEVEL'")
	if (anyNA(singleSide)) stop("singleSide should be TRUE or FALSE")
	styls$with_styles = TRUE
	styls$endCapStyle = ecs
	styls$joinStyle = js
	styls$mitreLimit = mitreLimit
	styls
}
#' @export
st_buffer.sfc = function(x, dist, nQuadSegs = 30,
						 endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1.0,
						 singleSide = FALSE, ...) {
	longlat = isTRUE(st_is_longlat(x))
	if (longlat && sf_use_s2()) {
#		if (!missing(nQuadSegs) || !missing(endCapStyle) || !missing(joinStyle) ||
#				!missing(mitreLimit) || !missing(singleSide))
#			warning("all buffer style parameters are ignored; set st_use_s2(FALSE) first to use them")
		if (inherits(dist, "units")) {
			if (!inherits(try(units(dist) <- as_units("rad"), silent = TRUE), "try-error"))
				return(st_as_sfc(s2::s2_buffer_cells(x, dist, radius = 1, ...),
					crs = st_crs(x)))
			units(dist) = as_units("m") # make sure has dimension length, possibly convert
			dist = drop_units(dist)
		}
		st_as_sfc(s2::s2_buffer_cells(x, dist, ...), crs = st_crs(x))
	} else {
		if (longlat) {
			warning("st_buffer does not correctly buffer longitude/latitude data")
			if (inherits(dist, "units"))
				units(dist) = as_units("arc_degrees")
			else
				message("dist is assumed to be in decimal degrees (arc_degrees).")
		} else if (inherits(dist, "units")) {
			if (is.na(st_crs(x)))
				stop("x does not have a crs set: can't convert units")
			if (is.null(st_crs(x)$units))
				stop("x has a crs without units: can't convert units")
			if (!is.null(st_crs(x)$ud_unit))
				units(dist) = st_crs(x)$ud_unit
		}
		dist = rep(dist, length.out = length(x))
		nQ = rep(nQuadSegs, length.out = length(x))
		styles = .process_style_opts(endCapStyle, joinStyle, mitreLimit, singleSide)
		if (styles$with_styles) {
			endCapStyle = rep(styles$endCapStyle, length.out = length(x))
			joinStyle = rep(styles$joinStyle, length.out = length(x))
			mitreLimit = rep(styles$mitreLimit, length.out = length(x))
			singleSide = rep(as.logical(singleSide), length.out = length(x))
			if (any(endCapStyle == 2) && any(st_geometry_type(x) == "POINT" | st_geometry_type(x) == "MULTIPOINT"))
				stop("Flat capstyle is incompatible with POINT/MULTIPOINT geometries") # nocov
			if (any(dist < 0) && any(st_dimension(x) < 1))
				stop("Negative dist values may only be used with 1-D or 2-D geometries") # nocov

			st_sfc(CPL_geos_op("buffer_with_style", x, dist, nQ, numeric(0), logical(0),
				endCapStyle = endCapStyle, joinStyle = joinStyle, mitreLimit = mitreLimit,
				singleside = singleSide))
		} else
			st_sfc(CPL_geos_op("buffer", x, dist, nQ, numeric(0), logical(0)))
	}
}

#' @export
st_buffer.sf = function(x, dist, nQuadSegs = 30,
						endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1.0,
						singleSide = FALSE, ...) {
	st_set_geometry(x, st_buffer(st_geometry(x), dist, nQuadSegs,
							   endCapStyle = endCapStyle, joinStyle = joinStyle, mitreLimit = mitreLimit,
							   singleSide = singleSide, ...))
}

#' @name geos_unary
#' @export
#' @details \code{st_boundary} returns the boundary of a geometry
st_boundary = function(x)
	UseMethod("st_boundary")

#' @export
st_boundary.sfg = function(x)
	get_first_sfg(st_boundary(st_sfc(x)))

#' @export
st_boundary.sfc = function(x)
	st_sfc(CPL_geos_op("boundary", x, numeric(0), integer(0), numeric(0), logical(0)))

#' @export
st_boundary.sf = function(x) {
	st_set_geometry(x, st_boundary(st_geometry(x)))
}

#' @name geos_unary
#' @export
#' @details \code{st_convex_hull} creates the convex hull of a set of points
#' @seealso \link[grDevices]{chull} for a more efficient algorithm for calculating the convex hull
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' nc_g = st_geometry(nc)
#' plot(st_convex_hull(nc_g))
#' plot(nc_g, border = grey(.5), add = TRUE)
st_convex_hull = function(x)
	UseMethod("st_convex_hull")

#' @export
st_convex_hull.sfg = function(x)
	get_first_sfg(st_convex_hull(st_sfc(x)))

#' @export
st_convex_hull.sfc = function(x)
	st_sfc(CPL_geos_op("convex_hull", x, numeric(0), integer(0), numeric(0), logical(0)))

#' @export
st_convex_hull.sf = function(x) {
	st_set_geometry(x, st_convex_hull(st_geometry(x)))
}

#' @name geos_unary
#' @export
#' @details \code{st_concave_hull} creates the concave hull of a geometry
#' @param ratio numeric; fraction convex: 1 returns the convex hulls, 0 maximally concave hulls
#' @param allow_holes logical; if \code{TRUE}, the resulting concave hull may have holes
#' @examples
#' set.seed(131)
#' if (compareVersion(sf_extSoftVersion()[["GEOS"]], "3.11.0") > -1) {
#'  pts = cbind(runif(100), runif(100))
#'  m = st_multipoint(pts)
#'  co = sf:::st_concave_hull(m, 0.3)
#'  coh = sf:::st_concave_hull(m, 0.3, allow_holes = TRUE)
#'  plot(co, col = 'grey')
#'  plot(coh, add = TRUE, border = 'red')
#'  plot(m, add = TRUE)
#' }
st_concave_hull = function(x, ratio, ..., allow_holes)
	UseMethod("st_concave_hull")

#' @export
st_concave_hull.sfg = function(x, ratio, ..., allow_holes = FALSE)
	get_first_sfg(st_concave_hull(st_sfc(x), ratio, allow_holes))

#' @export
st_concave_hull.sfc = function(x, ratio, ..., allow_holes = FALSE) {
	stopifnot(!missing(ratio), ratio >= 0, ratio <= 1.0, is.logical(allow_holes), !is.na(allow_holes))
	st_sfc(CPL_geos_op("concave_hull", x, ratio, integer(0), numeric(0), allow_holes))
}

#' @export
st_concave_hull.sf = function(x, ratio, ..., allow_holes = FALSE) {
	st_set_geometry(x, st_concave_hull(st_geometry(x), ratio, allow_holes))
}

#' @name geos_unary
#' @export
#' @details \code{st_simplify} simplifies lines by removing vertices. 
#' @param preserveTopology logical; carry out topology preserving
#'   simplification? May be specified for each, or for all feature geometries.
#'   Note that topology is preserved only for single feature geometries, not for
#'   sets of them. If not specified (i.e. the default), then it is internally
#'   set equal to \code{FALSE} when the input data is specified with projected
#'   coordinates or \code{sf_use_s2()} returns \code{FALSE}. Ignored in all the
#'   other cases (with a warning when set equal to \code{FALSE}) since the
#'   function implicitly calls \code{s2::s2_simplify} which always preserve
#'   topological relationships (per single feature).
#' @param dTolerance numeric; tolerance parameter, specified for all or for each
#'   feature geometry. If you run \code{st_simplify}, the input data is
#'   specified with long-lat coordinates and \code{sf_use_s2()} returns
#'   \code{TRUE}, then the value of \code{dTolerance} must be specified in
#'   meters.
#' @examples
#'
#' # st_simplify examples:
#' op = par(mfrow = c(2, 3), mar = rep(0, 4))
#' plot(nc_g[1])
#' plot(st_simplify(nc_g[1], dTolerance = 1e3)) # 1000m
#' plot(st_simplify(nc_g[1], dTolerance = 5e3)) # 5000m
#' nc_g_planar = st_transform(nc_g, 2264) # planar coordinates, US foot
#' plot(nc_g_planar[1])
#' plot(st_simplify(nc_g_planar[1], dTolerance = 1e3)) # 1000 foot
#' plot(st_simplify(nc_g_planar[1], dTolerance = 5e3)) # 5000 foot
#' par(op)
#'
st_simplify = function(x, preserveTopology, dTolerance = 0.0)
	UseMethod("st_simplify")

#' @export
st_simplify.sfg = function(x, preserveTopology, dTolerance = 0.0)
	get_first_sfg(st_simplify(st_sfc(x), preserveTopology, dTolerance = dTolerance))

#' @export
st_simplify.sfc = function(x, preserveTopology, dTolerance = 0.0) {
	ll = isTRUE(st_is_longlat(x))
	if (ll && sf_use_s2()) {
		if (!missing(preserveTopology) && isFALSE(preserveTopology))
			warning("argument preserveTopology cannot be set to FALSE when working with ellipsoidal coordinates since the algorithm behind st_simplify always preserves topological relationships")
		st_as_sfc(s2::s2_simplify(x, dTolerance), crs = st_crs(x))
	} else {
		if (missing(preserveTopology)) {
			preserveTopology = FALSE
		}
		stopifnot(mode(preserveTopology) == 'logical')
		if (ll)
			warning("st_simplify does not correctly simplify longitude/latitude data, dTolerance needs to be in decimal degrees")

		st_sfc(CPL_geos_op("simplify", x, numeric(0), integer(0),
			preserveTopology = rep(preserveTopology, length.out = length(x)),
			dTolerance = rep(dTolerance, length.out = length(x))))
	}
}

#' @export
st_simplify.sf = function(x, preserveTopology, dTolerance = 0.0) {
	st_set_geometry(x, st_simplify(st_geometry(x), preserveTopology, dTolerance))
}

#' @name geos_unary
#' @export
#' @param bOnlyEdges logical; if TRUE, return lines, else return polygons
#' @details \code{st_triangulate} triangulates set of points (not constrained). \code{st_triangulate} requires GEOS version 3.4 or above
st_triangulate = function(x, dTolerance = 0.0, bOnlyEdges = FALSE)
	UseMethod("st_triangulate")

#' @export
st_triangulate.sfg = function(x, dTolerance = 0.0, bOnlyEdges = FALSE)
	get_first_sfg(st_triangulate(st_sfc(x), dTolerance, bOnlyEdges = bOnlyEdges))

#' @export
st_triangulate.sfc = function(x, dTolerance = 0.0, bOnlyEdges = FALSE) {
	if (compareVersion(CPL_geos_version(), "3.4.0") > -1) { # >= ; see https://github.com/r-spatial/sf/issues/1653
		if (isTRUE(st_is_longlat(x)))
			warning("st_triangulate does not correctly triangulate longitude/latitude data")
		st_sfc(CPL_geos_op("triangulate", x, numeric(0), integer(0),
			dTolerance = rep(as.double(dTolerance), length.out = length(x)), logical(0),
			bOnlyEdges = as.integer(bOnlyEdges)))
	} else
		stop("for triangulate, GEOS version 3.4.0 or higher is required")
}

#' @export
st_triangulate.sf = function(x, dTolerance = 0.0, bOnlyEdges = FALSE) {
	st_set_geometry(x, st_triangulate(st_geometry(x), dTolerance, bOnlyEdges))
}

#' @name geos_unary
#' @export
#' @details \code{st_triangulate_constrained} returns the constrained delaunay triangulation of polygons; requires GEOS version 3.10 or above
#' @examples
#' if (compareVersion(sf_extSoftVersion()[["GEOS"]], "3.10.0") > -1) {
#'  pts = rbind(c(0,0), c(1,0), c(1,1), c(.5,.5), c(0,1), c(0,0))
#'  po = st_polygon(list(pts))
#'  co = st_triangulate_constrained(po)
#'  tr = st_triangulate(po)
#'  plot(po, col = NA, border = 'grey', lwd = 15)
#'  plot(tr, border = 'green', col = NA, lwd = 5, add = TRUE)
#'  plot(co, border = 'red', col = 'NA', add = TRUE)
#' }
st_triangulate_constrained = function(x)
	UseMethod("st_triangulate_constrained")

#' @export
st_triangulate_constrained.sfg = function(x)
	get_first_sfg(st_triangulate_constrained(st_sfc(x)))

#' @export
st_triangulate_constrained.sfc = function(x) {
	if (compareVersion(CPL_geos_version(), "3.10.0") > -1) { # >= ; see https://github.com/r-spatial/sf/issues/1653
		if (isTRUE(st_is_longlat(x)))
			warning("st_triangulate does not correctly triangulate longitude/latitude data")
		st_sfc(CPL_geos_op("triangulate_constrained", x, numeric(0), integer(0), numeric(0), logical(0)))
	} else
		stop("for triangulate_constrained, GEOS version 3.10.0 or higher is required")
}

#' @export
st_triangulate_constrained.sf = function(x) {
	st_set_geometry(x, st_triangulate_constrained(st_geometry(x)))
}

#' @name geos_unary
#' @export
#' @details \code{st_inscribed_circle} returns the maximum inscribed circle for polygon geometries. 
#' For \code{st_inscribed_circle}, if \code{nQuadSegs} is 0 a 2-point LINESTRING is returned with the
#' center point and a boundary point of every circle, otherwise a circle (buffer) is returned where
#' \code{nQuadSegs} controls the number of points per quadrant to approximate the circle.
#' \code{st_inscribed_circle} requires GEOS version 3.9 or above
#' @examples
#' if (compareVersion(sf_extSoftVersion()[["GEOS"]], "3.9.0") > -1) {
#'   nc_t = st_transform(nc, 'EPSG:2264')
#'   x = st_inscribed_circle(st_geometry(nc_t))
#'   plot(st_geometry(nc_t), asp = 1, col = grey(.9))
#'   plot(x, add = TRUE, col = '#ff9999')
#' }
st_inscribed_circle = function(x, dTolerance, ...)
	UseMethod("st_inscribed_circle")

#' @export
st_inscribed_circle.sfg = function(x, dTolerance, ...) {
	get_first_sfg(st_inscribed_circle(st_sfc(x), dTolerance, ...))
}

#' @export
st_inscribed_circle.sfc = function(x, dTolerance = sqrt(st_area(st_set_crs(x, NA_crs_)))/1000, ..., nQuadSegs = 30) {
	if (compareVersion(CPL_geos_version(), "3.9.0") > -1) { # >=
		if (isTRUE(st_is_longlat(x)))
			warning("st_inscribed_circle does not work correctly for longitude/latitude data")
		nQ = rep(nQuadSegs, length.out = length(x))
		ret = st_sfc(CPL_geos_op("inscribed_circle", x, nQ, integer(0),
			dTolerance = rep(as.double(dTolerance), length.out = length(x)), logical(0),
			bOnlyEdges = as.integer(FALSE)))
		if (any(nQuadSegs > 0)) {
			pts = st_cast(ret, "POINT")
			idx = seq(1, length(pts) * 2, by = 2)
			ret = st_buffer(pts[idx], st_length(st_set_crs(ret, NA_crs_)), nQuadSegs = nQuadSegs)
		} 
		ret
	} else
		stop("for st_inscribed_circle, GEOS version 3.9.0 or higher is required")
}

#' @export
st_inscribed_circle.sf = function(x, dTolerance, ...) {
	st_set_geometry(x, st_inscribed_circle(st_geometry(x), dTolerance), ...)
}

#' @name geos_unary
#' @details \code{st_minimum_rotated_rectangle} returns the minimum
#' rotated rectangular POLYGON which encloses the input geometry. The
#' rectangle has width equal to the minimum diameter, and a longer
#' length. If the convex hill of the input is degenerate (a line or
#' point) a linestring or point is returned.
#' @export
st_minimum_rotated_rectangle = function(x, ...)
	UseMethod("st_minimum_rotated_rectangle")

#' @export
st_minimum_rotated_rectangle.sfg = function(x, ...) {
	get_first_sfg(st_minimum_rotated_rectangle(st_sfc(x), ...))
}

#' @export
st_minimum_rotated_rectangle.sfc = function(x, ...) {
	if (compareVersion(CPL_geos_version(), "3.9.0") > -1) { # >=
		if (isTRUE(st_is_longlat(x)))
			warning("st_minimum_rotated_rectangle does not work correctly for longitude/latitude data")
		st_sfc(CPL_geos_op("minimum_rotated_rectangle", x, 0L, integer(0),
			dTolerance = 0., logical(0), bOnlyEdges = as.integer(FALSE)))
	} else
		stop("for st_minimum_rotated_rectangle, GEOS version 3.9.0 or higher is required")
}

#' @export
st_minimum_rotated_rectangle.sf = function(x, dTolerance, ...) {
	st_set_geometry(x, st_minimum_rotated_rectangle(st_geometry(x)), ...)
}


#' @name geos_unary
#' @export
#' @param envelope object of class \code{sfc} or \code{sfg} containing a \code{POLYGON} with the envelope for a voronoi diagram; this only takes effect when it is larger than the default envelope, chosen when \code{envelope} is an empty polygon
#' @details \code{st_voronoi} creates voronoi tesselation. \code{st_voronoi} requires GEOS version 3.5 or above
#' @examples
#' set.seed(1)
#' x = st_multipoint(matrix(runif(10),,2))
#' box = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
#' if (compareVersion(sf_extSoftVersion()[["GEOS"]], "3.5.0") > -1) {
#'  v = st_sfc(st_voronoi(x, st_sfc(box)))
#'  plot(v, col = 0, border = 1, axes = TRUE)
#'  plot(box, add = TRUE, col = 0, border = 1) # a larger box is returned, as documented
#'  plot(x, add = TRUE, col = 'red', cex=2, pch=16)
#'  plot(st_intersection(st_cast(v), box)) # clip to smaller box
#'  plot(x, add = TRUE, col = 'red', cex=2, pch=16)
#'  # matching Voronoi polygons to data points:
#'  # https://github.com/r-spatial/sf/issues/1030
#'  # generate 50 random unif points:
#'  n = 100
#'  pts = st_as_sf(data.frame(matrix(runif(n), , 2), id = 1:(n/2)), coords = c("X1", "X2"))
#'  # compute Voronoi polygons:
#'  pols = st_collection_extract(st_voronoi(do.call(c, st_geometry(pts))))
#'  # match them to points:
#'  pts$pols = pols[unlist(st_intersects(pts, pols))]
#'  plot(pts["id"], pch = 16) # ID is color
#'  plot(st_set_geometry(pts, "pols")["id"], xlim = c(0,1), ylim = c(0,1), reset = FALSE)
#'  plot(st_geometry(pts), add = TRUE)
#'  layout(matrix(1)) # reset plot layout
#' }
st_voronoi = function(x, envelope, dTolerance = 0.0, bOnlyEdges = FALSE)
	UseMethod("st_voronoi")

#' @export
st_voronoi.sfg = function(x, envelope = st_polygon(), dTolerance = 0.0, bOnlyEdges = FALSE)
	get_first_sfg(st_voronoi(st_sfc(x), st_sfc(envelope), dTolerance, bOnlyEdges = bOnlyEdges))

#' @export
st_voronoi.sfc = function(x, envelope = st_polygon(), dTolerance = 0.0, bOnlyEdges = FALSE) {
	if (compareVersion(CPL_geos_version(), "3.5.0") > -1) {
		if (isTRUE(st_is_longlat(x)))
			warning("st_voronoi does not correctly triangulate longitude/latitude data")
		st_sfc(CPL_geos_voronoi(x, st_sfc(envelope), dTolerance = dTolerance,
			bOnlyEdges = as.integer(bOnlyEdges)))
	} else
		stop("for voronoi, GEOS version 3.5.0 or higher is required")
}

#' @export
st_voronoi.sf = function(x, envelope = st_polygon(), dTolerance = 0.0, bOnlyEdges = FALSE) {
	st_set_geometry(x, st_voronoi(st_geometry(x), st_sfc(envelope), dTolerance, bOnlyEdges))
}

#' @name geos_unary
#' @details \code{st_polygonize} creates polygon from lines that form a closed ring. In case of \code{st_polygonize}, \code{x} must be an object of class \code{LINESTRING} or \code{MULTILINESTRING}, or an \code{sfc} geometry list-column object containing these
#' @export
#' @examples
#' mls = st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0),,2,byrow=TRUE)))
#' st_polygonize(st_sfc(mls))
st_polygonize = function(x)
	UseMethod("st_polygonize")

#' @export
st_polygonize.sfg = function(x)
	get_first_sfg(st_polygonize(st_sfc(x)))

#' @export
st_polygonize.sfc = function(x) {
	stopifnot(inherits(x, "sfc_LINESTRING") || inherits(x, "sfc_MULTILINESTRING"))
	st_sfc(CPL_geos_op("polygonize", x, numeric(0), integer(0), numeric(0), logical(0)))
}

#' @export
st_polygonize.sf = function(x) {
	st_set_geometry(x, st_polygonize(st_geometry(x)))
}

#' @name geos_unary
#' @export
#' @param directed logical; if \code{TRUE}, lines with opposite directions will not be merged
#' @details \code{st_line_merge} merges lines. In case of \code{st_line_merge}, \code{x} must be an object of class \code{MULTILINESTRING}, or an \code{sfc} geometry list-column object containing these
#' @examples
#' mls = st_multilinestring(list(rbind(c(0,0), c(1,1)), rbind(c(2,0), c(1,1))))
#' st_line_merge(st_sfc(mls))
st_line_merge = function(x, ..., directed = FALSE)
	UseMethod("st_line_merge")

#' @export
st_line_merge.sfg = function(x, ..., directed = FALSE)
	get_first_sfg(st_line_merge(st_sfc(x), directed = directed, ...))

#' @export
st_line_merge.sfc = function(x, ..., directed = FALSE) {
	stopifnot(inherits(x, "sfc_MULTILINESTRING"))
	if (directed)
		st_sfc(CPL_geos_op("linemergedirected", x, numeric(0), integer(0), numeric(0), logical(0)))
	else
		st_sfc(CPL_geos_op("linemerge", x, numeric(0), integer(0), numeric(0), logical(0)))
}

#' @export
st_line_merge.sf = function(x, ..., directed = FALSE) {
	st_set_geometry(x, st_line_merge(st_geometry(x), directed = directed, ...))
}

#' @name geos_unary
#' @param of_largest_polygon logical; for \code{st_centroid}: if \code{TRUE}, return centroid of the largest (sub)polygon of a \code{MULTIPOLYGON} rather than of the whole \code{MULTIPOLYGON}
#' @export
#' @details \code{st_centroid} gives the centroid of a geometry
#' @examples
#' plot(nc_g, axes = TRUE)
#' plot(st_centroid(nc_g), add = TRUE, pch = 3, col = 'red')
#' mp = st_combine(st_buffer(st_sfc(lapply(1:3, function(x) st_point(c(x,x)))), 0.2 * 1:3))
#' plot(mp)
#' plot(st_centroid(mp), add = TRUE, col = 'red') # centroid of combined geometry
#' plot(st_centroid(mp, of_largest_polygon = TRUE), add = TRUE, col = 'blue', pch = 3)
st_centroid = function(x, ..., of_largest_polygon = FALSE)
	UseMethod("st_centroid")

#' @export
st_centroid.sfg = function(x, ..., of_largest_polygon = FALSE)
	get_first_sfg(st_centroid(st_sfc(x), of_largest_polygon = of_largest_polygon))

largest_ring = function(x) {
	pols = st_cast(x, "POLYGON", warn = FALSE)
	stopifnot(! is.null(attr(pols, "ids")))
	areas = st_area(pols)
	spl = split(areas, rep(seq_along(x), attr(pols, "ids"))) # group by x
	l = c(0, head(cumsum(lengths(spl)), -1)) # 0-based indexes of first rings of a MULTIPOLYGON
	i = l + sapply(spl, which.max)           # add relative index of largest ring
	st_sfc(pols[i], crs = st_crs(x))
}

#' @export
st_centroid.sfc = function(x, ..., of_largest_polygon = FALSE) {
	if (of_largest_polygon) {
		multi = which(sapply(x, inherits, what = "MULTIPOLYGON") & lengths(x) > 1)
		if (length(multi))
			x[multi] = largest_ring(x[multi])
	}
	longlat = isTRUE(st_is_longlat(x))
	if (longlat && sf_use_s2())
		st_as_sfc(s2::s2_centroid(x), crs = st_crs(x))
	else { 
		if (longlat)
			warning("st_centroid does not give correct centroids for longitude/latitude data")
		st_sfc(CPL_geos_op("centroid", x, numeric(0), integer(0), numeric(0), logical(0)))
	}
}

#' @export
st_centroid.sf = function(x, ..., of_largest_polygon = FALSE) {
	if (any(st_dimension(x) > 0) && !all_constant(x))
		warning("st_centroid assumes attributes are constant over geometries", call. = FALSE)
	ret = st_set_geometry(x,
		st_centroid(st_geometry(x), of_largest_polygon = of_largest_polygon))
	agr = st_agr(ret)
	agr[ agr == "identity" ] = "constant"
	st_set_agr(ret, agr)
}


#' @name geos_unary
#' @export
#' @details \code{st_point_on_surface} returns a point guaranteed to be on the (multi)surface.
#' @examples
#' plot(nc_g, axes = TRUE)
#' plot(st_point_on_surface(nc_g), add = TRUE, pch = 3, col = 'red')
st_point_on_surface = function(x)
	UseMethod("st_point_on_surface")

#' @export
st_point_on_surface.sfg = function(x)
	get_first_sfg(st_point_on_surface(st_sfc(x)))

#' @export
st_point_on_surface.sfc = function(x) {
	if (isTRUE(st_is_longlat(x)))
		warning("st_point_on_surface may not give correct results for longitude/latitude data")
	st_sfc(CPL_geos_op("point_on_surface", x, numeric(0), integer(0), numeric(0), logical(0)))
}

#' @export
st_point_on_surface.sf = function(x) {
	if (any(st_dimension(x) > 0) && !all_constant(x))
		warning("st_point_on_surface assumes attributes are constant over geometries", call. = FALSE)
	st_set_geometry(x, st_point_on_surface(st_geometry(x)))
}

#' @name geos_unary
#' @export
#' @details \code{st_reverse} reverses the nodes in a line
#' @examples
#' if (compareVersion(sf_extSoftVersion()[["GEOS"]], "3.7.0") > -1) {
#'   st_reverse(st_linestring(rbind(c(1,1), c(2,2), c(3,3))))
#' }
#nocov start
st_reverse = function(x)
	UseMethod("st_reverse")

#' @export
st_reverse.sfg = function(x)
	get_first_sfg(st_reverse(st_sfc(x)))

#' @export
st_reverse.sfc = function(x) {
	st_sfc(CPL_geos_op("reverse", x, numeric(0), integer(0), numeric(0), logical(0)))
}

#' @export
st_reverse.sf = function(x) {
	st_set_geometry(x, st_reverse(st_geometry(x)))
}
#nocov end

#' @name geos_unary
#' @export
#' @details \code{st_node} adds nodes to linear geometries at intersections without a node, and only works on individual linear geometries
#' @examples
#' (l = st_linestring(rbind(c(0,0), c(1,1), c(0,1), c(1,0), c(0,0))))
#' st_polygonize(st_node(l))
#' st_node(st_multilinestring(list(rbind(c(0,0), c(1,1), c(0,1), c(1,0), c(0,0)))))
st_node = function(x) UseMethod("st_node")

#' @export
st_node.sfg = function(x)
	get_first_sfg(st_node(st_sfc(x)))

#' @export
st_node.sfc = function(x) {
	dims = st_dimension(x)
	if (!all(is.na(dims) || dims == 1))
		stop("st_node: all geometries should be linear")
	if (isTRUE(st_is_longlat(x)))
		warning("st_node may not give correct results for longitude/latitude data")
	st_sfc(CPL_geos_op("node", x, numeric(0), integer(0), numeric(0), logical(0)))
}

#' @export
st_node.sf = function(x) {
	st_set_geometry(x, st_node(st_geometry(x)))
}

#' @name geos_unary
#' @details \code{st_segmentize} adds points to straight lines
#' @export
#' @param dfMaxLength maximum length of a line segment. If \code{x} has geographical coordinates (long/lat), \code{dfMaxLength} is either a numeric expressed in meter, or an object of class \code{units} with length units \code{rad} or \code{degree}; segmentation in the long/lat case takes place along the great circle, using \link[lwgeom:geod]{st_geod_segmentize}.
#' @param ... ignored
#' @examples
#' sf = st_sf(a=1, geom=st_sfc(st_linestring(rbind(c(0,0),c(1,1)))), crs = 4326)
#' if (require(lwgeom, quietly = TRUE)) {
#'  seg = st_segmentize(sf, units::set_units(100, km))
#'  seg = st_segmentize(sf, units::set_units(0.01, rad))
#'  nrow(seg$geom[[1]])
#' }
st_segmentize	= function(x, dfMaxLength, ...)
	UseMethod("st_segmentize")

#' @export
st_segmentize.sfg = function(x, dfMaxLength, ...)
	get_first_sfg(st_segmentize(st_sfc(x), dfMaxLength, ...))

#' @export
st_segmentize.sfc	= function(x, dfMaxLength, ...) {
	if (isTRUE(st_is_longlat(x))) {
		if (! requireNamespace("lwgeom", quietly = TRUE))
			stop("package lwgeom required, please install it first")
		if (! inherits(dfMaxLength, "units"))
			units(dfMaxLength) = as_units("m")
		lwgeom::st_geod_segmentize(x, dfMaxLength) # takes care of rad or degree units
	} else {
		if (! is.na(st_crs(x)) && inherits(dfMaxLength, "units"))
			units(dfMaxLength) = units(st_crs(x)$SemiMajor) # might convert
		st_sfc(CPL_gdal_segmentize(x, dfMaxLength), crs = st_crs(x))
	}
}

#' @export
st_segmentize.sf = function(x, dfMaxLength, ...) {
	st_set_geometry(x, st_segmentize(st_geometry(x), dfMaxLength, ...))
}

#' Combine or union feature geometries
#'
#' Combine several feature geometries into one, without unioning or resolving internal boundaries
#' @name geos_combine
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @return \code{st_combine} returns a single, combined geometry, with no resolved boundaries; returned geometries may well be invalid.
#' @export
#' @details \code{st_combine} combines geometries without resolving borders, using \link{c.sfg} (analogous to \link[base]{c} for ordinary vectors).
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' st_combine(nc)
st_combine = function(x)
	st_sfc(do.call(c, st_geometry(x)), crs = st_crs(x)) # flatten/merge

# x: object of class sf
# y: object of class sf or sfc
# geoms: result from geos_op2: list of non-empty geometries with the intersection/union/difference/sym_difference
# which has an idx attribute pointing to what is x, what is y
geos_op2_df = function(x, y, geoms) {
	idx = attr(geoms, "idx")
	attr(geoms, "idx") = NULL
	all_constant_x = all_constant_y = TRUE
	all_constant_x = all_constant(x)
	df = x[idx[,1],,drop = FALSE]
	st_geometry(df) = NULL
	if (inherits(y, "sf")) {
		all_constant_y = all_constant(y)
		st_geometry(y) = NULL
		df = data.frame(df, y[idx[,2], , drop = FALSE])
	}
	if (! (all_constant_x && all_constant_y))
		warning("attribute variables are assumed to be spatially constant throughout all geometries",
			call. = FALSE)
	if (inherits(x, "tbl_df")) {
		if (!requireNamespace("tibble", quietly = TRUE))
			stop("package tibble required: install first?")
		df = tibble::new_tibble(df, nrow = nrow(df), class = "sf")
	}
	df[[ attr(x, "sf_column") ]] = geoms
	st_sf(df, sf_column_name = attr(x, "sf_column"))
}

# after checking identical crs,
# call geos_op2 function op on x and y:
# DE-9IM compliant should use model = "closed", more robust seems:
geos_op2_geom = function(op, x, y, model = "semi-open", ...) {
	stopifnot(st_crs(x) == st_crs(y))
	x = st_geometry(x)
	y = st_geometry(y)
	longlat = isTRUE(st_is_longlat(x))
	if (longlat && sf_use_s2()) {
		fn = switch(op, intersection = s2::s2_intersection,
				difference = s2::s2_difference,
				sym_difference = s2::s2_sym_difference,
				union = s2::s2_union, stop("invalid operator"))
		# to be optimized -- this doesn't index on y:
		lst = structure(unlist(lapply(y, function(yy) fn(x, yy, s2::s2_options(model = model, ...))),
			recursive = FALSE), class = "s2_geography")
		e = s2::s2_is_empty(lst)
		idx = cbind(rep(seq_along(x), length(y)), rep(seq_along(y), each = length(x)))
		lst = st_as_sfc(lst, crs = st_crs(x))
		structure(lst[!e], idx = idx[!e,,drop = FALSE])
	} else {
		if (longlat)
			message_longlat(paste0("st_", op))
		st_sfc(CPL_geos_op2(op, x, y), crs = st_crs(x))
	}
}

# return first sfg, or empty geometry in case of zero features
get_first_sfg = function(x) {
	if (length(x) == 0)
		st_geometrycollection()
	else
		x[[1]]
}

#' Geometric operations on pairs of simple feature geometry sets
#'
#' Perform geometric set operations with simple feature geometry collections
#' @name geos_binary_ops
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param ... arguments passed on to \link[s2]{s2_options}
#' @export
#' @return The intersection, difference or symmetric difference between two sets of geometries.
#' The returned object has the same class as that of the first argument (\code{x}) with the non-empty geometries resulting from applying the operation to all geometry pairs in \code{x} and \code{y}. In case \code{x} is of class \code{sf}, the matching attributes of the original object(s) are added. The \code{sfc} geometry list-column returned carries an attribute \code{idx}, which is an \code{n}-by-2 matrix with every row the index of the corresponding entries of \code{x} and \code{y}, respectively.
#' @details When using GEOS and not using s2, a spatial index is built on argument \code{x}; see \url{https://r-spatial.org/r/2017/06/22/spatial-index.html}. The reference for the STR tree algorithm is: Leutenegger, Scott T., Mario A. Lopez, and Jeffrey Edgington. "STR: A simple and efficient algorithm for R-tree packing." Data Engineering, 1997. Proceedings. 13th international conference on. IEEE, 1997. For the pdf, search Google Scholar.
#' @seealso \link{st_union} for the union of simple features collections; \link{intersect} and \link{setdiff} for the base R set operations.
#' @export
#' @note To find whether pairs of simple feature geometries intersect, use
#' the function \code{\link{st_intersects}} instead of \code{st_intersection}.
#'
#' When using GEOS and not using s2 polygons contain their boundary. When using s2 this is determined by the \code{model} defaults of \link[s2]{s2_options}, which can be overriden via the ... argument, e.g. \code{model = "closed"} to force DE-9IM compliant behaviour of polygons (and reproduce GEOS results).
#' @examples
#' set.seed(131)
#' library(sf)
#' m = rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))
#' p = st_polygon(list(m))
#' n = 100
#' l = vector("list", n)
#' for (i in 1:n)
#'   l[[i]] = p + 10 * runif(2)
#' s = st_sfc(l)
#' plot(s, col = sf.colors(categorical = TRUE, alpha = .5))
#' title("overlapping squares")
#' d = st_difference(s) # sequential differences: s1, s2-s1, s3-s2-s1, ...
#' plot(d, col = sf.colors(categorical = TRUE, alpha = .5))
#' title("non-overlapping differences")
#' i = st_intersection(s) # all intersections
#' plot(i, col = sf.colors(categorical = TRUE, alpha = .5))
#' title("non-overlapping intersections")
#' summary(lengths(st_overlaps(s, s))) # includes self-counts!
#' summary(lengths(st_overlaps(d, d)))
#' summary(lengths(st_overlaps(i, i)))
#' sf = st_sf(s)
#' i = st_intersection(sf) # all intersections
#' plot(i["n.overlaps"])
#' summary(i$n.overlaps - lengths(i$origins))
st_intersection = function(x, y, ...) UseMethod("st_intersection")

#' @export
st_intersection.sfg = function(x, y, ...)
	get_first_sfg(geos_op2_geom("intersection", x, y, ...))

#' @name geos_binary_ops
#' @export
#' @details When called with missing \code{y}, the \code{sfc} method for \code{st_intersection} returns all non-empty intersections of the geometries of \code{x}; an attribute \code{idx} contains a list-column with the indexes of contributing geometries.
st_intersection.sfc = function(x, y, ...) {
	if (missing(y)) {
		if (isTRUE(st_is_longlat(x)))
			message_longlat("st_intersection")
		ret = CPL_nary_intersection(x)
		structure(st_sfc(ret), idx = attr(ret, "idx"))
	} else
		geos_op2_geom("intersection", x, y, ...)
}

#' @name geos_binary_ops
#' @export
#' @details when called with a missing \code{y}, the \code{sf} method for \code{st_intersection} returns an \code{sf} object with attributes taken from the contributing feature with lowest index; two fields are added: \code{n.overlaps} with the number of overlapping features in \code{x}, and a list-column \code{origins} with indexes of all overlapping features.
st_intersection.sf = function(x, y, ...) {
	if (missing(y)) {
		geom = st_intersection(st_geometry(x), ...)
		idx = attr(geom, "idx")
		i = sapply(idx, function(i) i[1])
		sf_column = attr(x, "sf_column")
		st_geometry(x) = NULL
		x = x[i, , drop = FALSE]
		x$n.overlaps = lengths(idx)
		x$origins = idx
		x[[ sf_column ]] = structure(geom, idx = NULL)
		st_sf(x)
	} else
		geos_op2_df(x, y, geos_op2_geom("intersection", x, y, ...))
}

#' @name geos_binary_ops
#' @export
#' @examples
#' # A helper function that erases all of y from x:
#' st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
st_difference = function(x, y, ...) UseMethod("st_difference")

#' @export
st_difference.sfg = function(x, y, ...)
	get_first_sfg(geos_op2_geom("difference", x, y, ...))

#' @name geos_binary_ops
#' @export
#' @details When \code{st_difference} is called with a single argument,
#' overlapping areas are erased from geometries that are indexed at greater
#' numbers in the argument to \code{x}; geometries that are empty
#' or contained fully inside geometries with higher priority are removed entirely.
#' The \code{st_difference.sfc} method with a single argument returns an object with
#' an \code{"idx"} attribute with the orginal index for returned geometries.
st_difference.sfc = function(x, y, ...) {
	if (missing(y)) {
		if (isTRUE(st_is_longlat(x)))
			message_longlat("st_difference")
		ret = CPL_nary_difference(x)
		structure(st_sfc(ret), ret = attr(ret, "idx"))
	} else
		geos_op2_geom("difference", x, y, ...)
}

#' @export
st_difference.sf = function(x, y, ...) {
	if (missing(y)) {
		geom = st_difference(st_geometry(x))
		sf_column = attr(x, "sf_column")
		st_geometry(x) = NULL
		x = x[attr(geom, "idx"), , drop=FALSE]
		x[[ sf_column ]] = structure(geom, idx = NULL)
		st_sf(x)
	} else
		geos_op2_df(x, y, geos_op2_geom("difference", x, y, ...))
}

#' @name geos_binary_ops
#' @export
st_sym_difference = function(x, y, ...) UseMethod("st_sym_difference")

#' @export
st_sym_difference.sfg = function(x, y, ...)
	get_first_sfg(geos_op2_geom("sym_difference", x, y, ...))

#' @export
st_sym_difference.sfc = function(x, y, ...)
	geos_op2_geom("sym_difference", x, y, ...)

#' @export
st_sym_difference.sf = function(x, y, ...)
	geos_op2_df(x, y, geos_op2_geom("sym_difference", x, y, ...))

#' @name geos_binary_ops
#' @param tolerance tolerance values used for \code{st_snap}; numeric value or object of class \code{units}; may have tolerance values for each feature in \code{x}
#' @details \code{st_snap} snaps the vertices and segments of a geometry to another geometry's vertices. If \code{y} contains more than one geometry, its geometries are merged into a collection before snapping to that collection.
#'
#' (from the GEOS docs:) "A snap distance tolerance is used to control where snapping is performed. Snapping one geometry to another can improve robustness for overlay operations by eliminating nearly-coincident edges (which cause problems during noding and intersection calculation). Too much snapping can result in invalid topology being created, so the number and location of snapped vertices is decided using heuristics to determine when it is safe to snap. This can result in some potential snaps being omitted, however."
#' @examples
#' poly = st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0))))
#' lines = st_multilinestring(list(
#'  cbind(c(0, 1), c(1, 1.05)),
#'  cbind(c(0, 1), c(0, -.05)),
#'  cbind(c(1, .95, 1), c(1.05, .5, -.05))
#' ))
#' snapped = st_snap(poly, lines, tolerance=.1)
#' plot(snapped, col='red')
#' plot(poly, border='green', add=TRUE)
#' plot(lines, lwd=2, col='blue', add=TRUE)
#' @export
st_snap = function(x, y, tolerance) UseMethod("st_snap")

#' @export
st_snap.sfg = function(x, y, tolerance)
	get_first_sfg(st_snap(st_sfc(x), y, tolerance))

#' @export
st_snap.sfc = function(x, y, tolerance) {
	if (isTRUE(st_is_longlat(x)))
		stop("st_snap for longitude/latitude data not supported; use st_transform first?")
	else if (inherits(tolerance, "units") && !is.null(st_crs(x)$ud_unit))
		units(tolerance) = st_crs(x)$ud_unit # coordinate units
	tolerance = rep(tolerance, length.out = length(x))
	st_sfc(CPL_geos_snap(st_geometry(x), st_geometry(y), as.double(tolerance)))
}

#' @export
st_snap.sf = function(x, y, tolerance)
	st_set_geometry(x, st_snap(st_geometry(x), st_geometry(y), tolerance))

#' @name geos_combine
#' @export
#' @param by_feature logical; if TRUE, union each feature if \code{y} is missing or else each pair of features; if FALSE return a single feature that is the geometric union of the set of features in \code{x} if \code{y} is missing, or else the unions of each of the elements of the Cartesian product of both sets
#' @param is_coverage logical; if TRUE, use an optimized algorithm for features that form a polygonal coverage (have no overlaps)
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg} (optional)
#' @param ... ignored
#' @seealso \link{st_intersection}, \link{st_difference}, \link{st_sym_difference}
#' @return If \code{y} is missing, \code{st_union(x)} returns a single geometry with resolved boundaries, else the geometries for all unioned pairs of `x[i]` and `y[j]`.
#' @details
#' If \code{st_union} is called with a single argument, \code{x}, (with \code{y} missing) and \code{by_feature} is \code{FALSE} all geometries are unioned together and an \code{sfg} or single-geometry \code{sfc} object is returned. If \code{by_feature} is \code{TRUE} each feature geometry is unioned individually. This can for instance be used to resolve internal boundaries after polygons were combined using \code{st_combine}. If \code{y} is provided, all elements of \code{x} and \code{y} are unioned, pairwise if \code{by_feature} is TRUE, or else as the Cartesian product of both sets. 
#'
#' Unioning a set of overlapping polygons has the effect of merging the areas (i.e. the same effect as iteratively unioning all individual polygons together). Unioning a set of LineStrings has the effect of fully noding and dissolving the input linework. In this context "fully noded" means that there will be a node or endpoint in the output for every endpoint or line segment crossing in the input. "Dissolved" means that any duplicate (e.g. coincident) line segments or portions of line segments will be reduced to a single line segment in the output.	Unioning a set of Points has the effect of merging all identical points (producing a set with no duplicates).
#' @examples
#' plot(st_union(nc))
st_union = function(x, y, ..., by_feature = FALSE, is_coverage = FALSE) UseMethod("st_union")

#' @export
st_union.sfg = function(x, y, ..., by_feature = FALSE, is_coverage = FALSE) {
	out = if (missing(y)) # unary union, possibly by_feature:
		st_sfc(CPL_geos_union(st_geometry(x), by_feature, is_coverage))
	else
		st_union(st_geometry(x), st_geometry(y))
	get_first_sfg(out)
}

#' @export
st_union.sfc = function(x, y, ..., by_feature = FALSE, is_coverage = FALSE) {
	ll = isTRUE(st_is_longlat(x))
	if (missing(y)) { # unary union, possibly by_feature:
		if (ll && sf_use_s2()) { 
			if (! by_feature) { # see https://github.com/r-spatial/s2/issues/97 :
				if (is_coverage)
					st_as_sfc(s2::s2_coverage_union_agg(x, ...), crs = st_crs(x))
				else
					st_as_sfc(s2::s2_union_agg(x, ...), crs = st_crs(x)) 
			} else
				st_as_sfc(s2::s2_union(x, ...), crs = st_crs(x)) 
		} else {
			if (ll)
				message_longlat("st_union")
			st_sfc(CPL_geos_union(x, by_feature, is_coverage))
		}
	} else {
		y = st_geometry(y)
		stopifnot(st_crs(x) == st_crs(y))
		if (by_feature) {
			stopifnot(length(x) == length(y))
			if (ll && sf_use_s2())
				st_as_sfc(s2::s2_union(x, y, ...), crs = st_crs(x)) 
			else {
				if (ll)
					message_longlat("st_union")
				st_as_sfc(mapply(st_union, x, y, MoreArgs = list(is_coverage = is_coverage), SIMPLIFY = FALSE),
						  crs = st_crs(x), precision = st_precision(x))
			}
		} else {
			if (ll && sf_use_s2()) {
				i = rep(seq_along(x), each = length(y))
				j = rep(seq_along(y), length(x))
				st_as_sfc(s2::s2_union(x[i], y[j], ...), crs = st_crs(x),
					precision = st_precision(x)) 
			} else {
				if (ll)
					message_longlat("st_union")
				geos_op2_geom("union", x, y, ...)
			}
		}
	}
}

#' @export
st_union.sf = function(x, y, ..., by_feature = FALSE, is_coverage = FALSE) {
	if (missing(y)) { # unary union, possibly by_feature:
		geom = st_union(st_geometry(x), ..., by_feature = by_feature, is_coverage = is_coverage)
		if (by_feature)
			st_set_geometry(x, geom)
		else
			geom
	} else {
		if (by_feature) {
			df = cbind(st_drop_geometry(x), st_drop_geometry(y))
			st_set_geometry(df, st_union(st_geometry(x), st_geometry(y), is_coverage = is_coverage))
		} else
			geos_op2_df(x, y, geos_op2_geom("union", x, y, ...))
	}
}

#' Sample points on a linear geometry
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param n integer; number of points to choose per geometry; if missing, n will be computed as \code{round(density * st_length(geom))}.
#' @param density numeric; density (points per distance unit) of the sampling, possibly a vector of length equal to the number of features (otherwise recycled); \code{density} may be of class \code{units}.
#' @param type character; indicate the sampling type, either "regular" or "random"
#' @param sample numeric; a vector of numbers between 0 and 1 indicating the points to sample - if defined sample overrules n, density and type.
#' @export
#' @examples
#' ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
#' 	st_linestring(rbind(c(0,0),c(10,0))))
#' st_line_sample(ls, density = 1)
#' ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
#'	 st_linestring(rbind(c(0,0),c(.1,0))), crs = 4326)
#' try(st_line_sample(ls, density = 1/1000)) # error
#' st_line_sample(st_transform(ls, 3857), n = 5) # five points for each line
#' st_line_sample(st_transform(ls, 3857), n = c(1, 3)) # one and three points
#' st_line_sample(st_transform(ls, 3857), density = 1/1000) # one per km
#' st_line_sample(st_transform(ls, 3857), density = c(1/1000, 1/10000)) # one per km, one per 10 km
#' st_line_sample(st_transform(ls, 3857), density = units::set_units(1, 1/km)) # one per km
#' # five equidistant points including start and end:
#' st_line_sample(st_transform(ls, 3857), sample = c(0, 0.25, 0.5, 0.75, 1))
st_line_sample = function(x, n, density, type = "regular", sample = NULL) {
	if (isTRUE(st_is_longlat(x)))
		stop("st_line_sample for longitude/latitude not supported; use st_segmentize?")
	l = st_length(x)
	distList = if (is.null(sample)) {
		n = if (missing(n)) {
			if (!is.null(st_crs(x)$ud_unit) && inherits(density, "units"))
				units(density) = 1/st_crs(x)$ud_unit # coordinate units
			round(rep(density, length.out = length(l)) * l)
		} else
			rep(n, length.out = length(l))
		regular = function(n) { (seq_len(n) - 0.5)/n }
		random = function(n) { sort(runif(n)) }
		fn = switch(type,
					regular = regular,
					random = random,
					stop("unknown type"))
		lapply(seq_along(n), function(i) fn(n[i]) * l[i])
	} else
		lapply(seq_along(l), function(i) sample * l[i])

	x = st_geometry(x)
	stopifnot(inherits(x, "sfc_LINESTRING"))
	st_sfc(CPL_gdal_linestring_sample(x, distList), crs = st_crs(x))
}

#' Internal functions
#' @name internal
#' @param msg error message
#' @export
.stop_geos = function(msg) { #nocov start
	on.exit(stop(msg))
	lst = strsplit(msg, " at ")[[1]]
	pts = scan(text = lst[[length(lst)]], quiet = TRUE)
	if (length(pts) == 2 && is.numeric(pts))
  		assign(".geos_error", st_point(pts), envir=.sf_cache)
} #nocov end
