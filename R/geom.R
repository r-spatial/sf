# unary, interfaced through GEOS:

#' Dimension, simplicity or validity queries on simple feature geometries
#' @name geos_query
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param NA_if_empty logical; if TRUE, return NA for empty geometries
#' @return st_dimension returns a numeric vector with 0 for points, 1 for lines, 2 for surfaces, and, if \code{NA_if_empty} is \code{TRUE}, \code{NA} for empty geometries.
#' @export
#' @examples
#' x = st_sfc(
#' 	st_point(0:1),
#' 	st_linestring(rbind(c(0,0),c(1,1))),
#' 	st_polygon(list(rbind(c(0,0),c(1,0),c(0,1),c(0,0)))),
#' 	st_multipoint(),
#' 	st_linestring(),
#' 	st_geometrycollection())
#' st_dimension(x)
#' st_dimension(x, FALSE)
st_dimension = function(x, NA_if_empty = TRUE)
	CPL_gdal_dimension(st_geometry(x), NA_if_empty)

#' @name geos_query
#' @export
#' @return st_is_simple returns a logical vector, indicating for each geometry whether it is simple (e.g., not self-intersecting)
#' @examples
#' ls = st_linestring(rbind(c(0,0), c(1,1), c(1,0), c(0,1)))
#' st_is_simple(st_sfc(ls, st_point(c(0,0))))
st_is_simple = function(x) CPL_geos_is_simple(st_geometry(x))

#' @name geos_measures
#' @export
#' @return If the coordinate reference system of \code{x} was set, these functions return values with unit of measurement; see \link[units]{set_units}.
#'
#' st_area returns the area of a geometry, in the coordinate reference system used; in case \code{x} is in degrees longitude/latitude, \link[geosphere]{areaPolygon} is used for area calculation.
#' @examples
#' b0 = st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
#' b1 = b0 + 2
#' b2 = b0 + c(-0.2, 2)
#' x = st_sfc(b0, b1, b2)
#' st_area(x)
st_area = function(x) {
	if (isTRUE(st_is_longlat(x))) {
		p = crs_parameters(st_crs(x))
		if (!requireNamespace("sp", quietly = TRUE))
			stop("package sp required, please install it first")
		if (!requireNamespace("geosphere", quietly = TRUE))
			stop("package geosphere required, please install it first")
		a = geosphere::areaPolygon(as(st_geometry(x), "Spatial"),
				as.numeric(p$SemiMajor), 1./p$InvFlattening)
		units(a) = units(p$SemiMajor^2)
		a
	} else {
		a = CPL_area(st_geometry(x)) # ignores units: units of coordinates
		if (!is.na(st_crs(x)))
			units(a) = crs_parameters(st_crs(x))$ud_unit^2 # coord units
		a
	}
}

ll_length = function(x, fn, p) {
	if (is.list(x)) # sfc_MULTILINESTRING
		sum(vapply(x, ll_length, 0.0, fn = fn, p = p))
	else {
		pts = unclass(x) # matrix
		sum(fn(head(pts, -1), tail(pts, -1), as.numeric(p$SemiMajor), 1./p$InvFlattening))
	}
}

#' @name geos_measures
#' @export
#' @return st_length returns the length of a LINESTRING or MULTILINESTRING geometry, using the coordinate reference system.  POINT or MULTIPOINT geometries return zero, POLYGON or MULTIPOLYGON are converted into LINESTRING or MULTILINESTRING, respectively.
#' @seealso \link{st_dimension}
#' @examples
#' dist_vincenty = function(p1, p2, a, f) geosphere::distVincentyEllipsoid(p1, p2, a, a * (1-f), f)
#' line = st_sfc(st_linestring(rbind(c(30,30), c(40,40))), crs = 4326)
#' st_length(line)
#' st_length(line, dist_fun = dist_vincenty)
st_length = function(x, dist_fun = geosphere::distGeo) {
	x = st_geometry(x)
	if (inherits(x, "sfc_POINT") || inherits(x, "sfc_MULTIPOINT"))
		return(0.0)

	if (inherits(x, "sfc_POLYGON") || inherits(x, "sfc_MULTIPOLYGON"))
		x = st_cast(x, "MULTILINESTRING")
	else
		stopifnot(inherits(x, "sfc_LINESTRING") || inherits(x, "sfc_MULTILINESTRING"))
	if (isTRUE(st_is_longlat(x))) {
		if (missing(dist_fun) && !requireNamespace("geosphere", quietly = TRUE))
			stop("package geosphere required, please install it first")
		p = crs_parameters(st_crs(x))
		ret = vapply(x, ll_length, 0.0, fn = dist_fun, p = p)
		units(ret) = units(p$SemiMajor)
		ret
	} else {
		ret = CPL_length(x) # units of coordinates
		ret[is.nan(ret)] = NA
		if (!is.na(st_crs(x)))
			units(ret) = crs_parameters(st_crs(x))$ud_unit
		ret
	}
}

message_longlat = function(caller) {
	message(paste("although coordinates are longitude/latitude,",
		caller, "assumes that they are planar"))
}
# binary, interfaced through GEOS:

# returning matrix, distance or relation string -- the work horse is:

st_geos_binop = function(op = "intersects", x, y, par = 0.0, pattern = NA_character_,
		sparse = TRUE, prepared = FALSE) {
	if (missing(y))
		y = x
	else if (!inherits(x, "sfg") && !inherits(y, "sfg"))
		stopifnot(st_crs(x) == st_crs(y))
	if (isTRUE(st_is_longlat(x)) && !(op %in% c("equals", "equals_exact", "polygonize")))
		message_longlat(paste0("st_", op))
	ret = CPL_geos_binop(st_geometry(x), st_geometry(y), op, par, pattern, sparse, prepared)
	if (sparse) {
		if (is.null(id <- row.names(x)))
			id = as.character(1:length(ret))
		structure(ret, predicate = op, region.id = id, ncol = length(st_geometry(y)),
			class = "sgbp")
	} else
		ret[[1]]
}

#' Compute geometric measurements
#'
#' Compute Euclidian or great circle distance between pairs of geometries, the area of a geometry, or the length of a geometry.
#' @name geos_measures
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}, defaults to \code{x}
#' @param dist_fun function to be used for great circle distances of geographical coordinates; for unprojected (long/lat) data, this should be a distance function of package geosphere, or compatible to that; it defaults to \link[geosphere]{distGeo} in that case; for other data metric lengths are computed.
#' @param by_element logical; if \code{TRUE}, return a vector with distance between the first elements of \code{x} and \code{y}, the second, etc. if \code{FALSE}, return the dense matrix with all pairwise distances.
#' @return If \code{by_element} is \code{FALSE} a dense numeric matrix of dimension length(x) by length(y); otherwise a numeric vector of length \code{x} or \code{y}, the shorter one being recycled.
#' @details Function \code{dist_fun} should follow the pattern of the distance function \link[geosphere]{distGeo}: the first two arguments must be 2-column point matrices, the third the semi major axis (radius, in m), the third the ellipsoid flattening.
#' @examples
#' p = st_sfc(st_point(c(0,0)), st_point(c(0,1)), st_point(c(0,2)))
#' st_distance(p, p)
#' st_distance(p, p, by_element = TRUE)
#' @export
st_distance = function(x, y, dist_fun, by_element = FALSE) {
	if (missing(y))
		y = x
	else
		stopifnot(st_crs(x) == st_crs(y))

	if (by_element)
		return(mapply(st_distance, x, y, by_element = FALSE))

	x = st_geometry(x)
	y = st_geometry(y)
	if (!is.na(st_crs(x)))
		p = crs_parameters(st_crs(x))
	if (isTRUE(st_is_longlat(x))) {
		if (!inherits(x, "sfc_POINT") || !inherits(y, "sfc_POINT"))
			stop("st_distance for longitude/latitude data only available for POINT geometries")
		if (!requireNamespace("geosphere", quietly = TRUE))
			stop("package geosphere required, please install it first")
		if (missing(dist_fun))
			dist_fun = geosphere::distGeo
		xp = do.call(rbind, x)[rep(seq_along(x), length(y)),]
		yp = do.call(rbind, y)[rep(seq_along(y), each = length(x)),]
		m = matrix(
			dist_fun(xp, yp, as.numeric(p$SemiMajor), 1./p$InvFlattening),
			length(x), length(y))
		units(m) = units(p$SemiMajor)
		m
	} else {
		d = CPL_geos_dist(x, y)
		if (! is.na(st_crs(x)))
			units(d) = p$ud_unit
		d
	}
}

#' Compute DE9-IM relation between pairs of geometries, or match it to a given pattern
#'
#' Compute DE9-IM relation between pairs of geometries, or match it to a given pattern
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param pattern character; define the pattern to match to, see details.
#' @param sparse logical; should a sparse matrix be returned (TRUE) or a dense matrix?
#' @return In case \code{pattern} is not given, \code{st_relate} returns a dense \code{character} matrix; element [i,j] has nine characters, referring to the DE9-IM relationship between x[i] and y[j], encoded as IxIy,IxBy,IxEy,BxIy,BxBy,BxEy,ExIy,ExBy,ExEy where I refers to interior, B to boundary, and E to exterior, and e.g. BxIy the dimensionality of the intersection of the the boundary of x[i] and the interior of y[j], which is one of {0,1,2,F}, digits denoting dimensionality, F denoting not intersecting. When \code{pattern} is given, a dense logical matrix or sparse index list returned with matches to the given pattern; see \link{st_intersection} for a description of the returned matrix or list. See also \url{https://en.wikipedia.org/wiki/DE-9IM} for further explanation.
#' @export
#' @examples
#' p1 = st_point(c(0,0))
#' p2 = st_point(c(2,2))
#' pol1 = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0)))) - 0.5
#' pol2 = pol1 + 1
#' pol3 = pol1 + 2
#' st_relate(st_sfc(p1, p2), st_sfc(pol1, pol2, pol3))
#' sfc = st_sfc(st_point(c(0,0)), st_point(c(3,3)))
#' grd = st_make_grid(sfc, n = c(3,3))
#' st_intersects(grd)
#' st_relate(grd, pattern = "****1****") # sides, not corners, internals
#' st_relate(grd, pattern = "****0****") # only corners touch
#' st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")
#' st_rook(grd)
#' # queen neighbours, see \url{https://github.com/r-spatial/sf/issues/234#issuecomment-300511129}
#' st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
st_relate	= function(x, y, pattern = NA_character_, sparse = !is.na(pattern)) {
	if (!is.na(pattern)) {
		stopifnot(is.character(pattern) && length(pattern) == 1 && nchar(pattern) == 9)
		st_geos_binop("relate_pattern", x, y, pattern = pattern, sparse = sparse)
	} else
		st_geos_binop("relate", x, y, sparse = FALSE)
}

#' Geometric binary predicates on pairs of simple feature geometry sets
#'
#' Geometric binary predicates on pairs of simple feature geometry sets
#' @name geos_binary_pred
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param sparse logical; should a sparse index list be returned (TRUE) or a dense logical matrix? See below.
#' @return If \code{sparse=FALSE}, \code{st_predicate} (with \code{predicate} e.g. "intersects") returns a dense logical matrix with element \code{i,j} \code{TRUE} when \code{predicate(x[i], y[j])} (e.g., when geometry i and j intersect); if \code{sparse=TRUE}, an object of class \code{sgbp} with a sparse list representation of the same matrix, with list element \code{i} a numeric vector with the indices j for which \code{predicate(x[i],y[j])} is \code{TRUE} (and hence \code{integer(0)} if none of them is \code{TRUE}). From the dense matrix, one can find out if one or more elements intersect by \code{apply(mat, 1, any)}, and from the sparse list by \code{lengths(lst) > 0}, see examples below.
#' @details For most predicates, a spatial index is built on argument \code{x}; see \url{http://r-spatial.org/r/2017/06/22/spatial-index.html}.
#' Specifically, \code{st_intersects}, \code{st_disjoint}, \code{st_touches} \code{st_crosses}, \code{st_within}, \code{st_contains}, \code{st_contains_properly}, \code{st_overlaps}, \code{st_equals}, \code{st_covers} and \code{st_covered_by} all build spatial indexes for more efficient geometry calculations. \code{st_relate}, \code{st_equals_exact}, and \code{st_is_within_distance} do not.
#'
#' Sparse geometry binary predicate (\code{sgbp}) lists have the following attributes: \code{region.id} with the \code{row.names} of \code{x} (if any, else \code{1:n}), and \code{predicate} with the name of the predicate used.
#' @examples
#' pts = st_sfc(st_point(c(.5,.5)), st_point(c(1.5, 1.5)), st_point(c(2.5, 2.5)))
#' pol = st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
#' (lst = st_intersects(pts, pol))
#' (mat = st_intersects(pts, pol, sparse = FALSE))
#' # which points fall inside a polygon?
#' apply(mat, 1, any)
#' lengths(lst) > 0
#' # which points fall inside the first polygon?
#' st_intersects(pol, pts)[[1]]
#' @export
st_intersects	= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared)

#' @name geos_binary_pred
#' @export
st_disjoint		= function(x, y = x, sparse = TRUE, prepared = TRUE) {
	# st_geos_binop("disjoint", x, y, sparse = sparse, prepared = prepared) -> didn't use STRtree
	int = st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared)
	# disjoint = !intersects :
	if (sparse)
		structure(
			lapply(int, function(g) setdiff(1:length(st_geometry(y)), g)),
			predicate = "disjoint", class = "sgbp")
	else
		!int
}

#' @name geos_binary_pred
#' @export
st_touches		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("touches", x, y, sparse = sparse, prepared = prepared)

#' @name geos_binary_pred
#' @export
st_crosses		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("crosses", x, y, sparse = sparse, prepared = prepared)

#' @name geos_binary_pred
#' @export
st_within		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("within", x, y, sparse = sparse, prepared = prepared)

#' @name geos_binary_pred
#' @export
#' @param prepared logical; prepare geometry for x, before looping over y?
st_contains		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("contains", x, y, sparse = sparse, prepared = prepared)

#' @name geos_binary_pred
#' @export
#' @details `st_contains_properly(A,B)` is true if A intersects B's interior, but not its edges or exterior; A contains A, but A does not properly contain A.
#'
#' See also \link{st_relate} and \url{https://en.wikipedia.org/wiki/DE-9IM} for a more detailed description of the underlying algorithms.
st_contains_properly = function(x, y, sparse = TRUE, prepared = TRUE) {
	if (! prepared)
		stop("non-prepared geometries not supported for st_contains_properly")
	st_geos_binop("contains_properly", x, y, sparse = sparse, prepared = TRUE)
}

#' @name geos_binary_pred
#' @export
st_overlaps		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("overlaps", x, y, sparse = sparse, prepared = prepared)

#' @name geos_binary_pred
#' @export
st_equals		= function(x, y, sparse = TRUE, prepared = FALSE) {
	if (prepared)
		stop("prepared geometries not supported for st_equals")
	st_geos_binop("equals", x, y, sparse = sparse)
}

#' @name geos_binary_pred
#' @export
st_covers		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("covers", x, y, sparse = sparse, prepared = prepared)

#' @name geos_binary_pred
#' @export
st_covered_by	= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("covered_by", x, y, sparse = sparse, prepared = prepared)

#' @name geos_binary_pred
#' @export
#' @param par numeric; parameter used for "equals_exact" (margin);
#' @details \code{st_equals_exact} returns true for two geometries of the same type and their vertices corresponding by index are equal up to a specified tolerance.
st_equals_exact = function(x, y, par, sparse = TRUE, prepared = FALSE) {
	if (prepared)
		stop("prepared geometries not supported for st_equals_exact")
	st_geos_binop("equals_exact", x, y, par = par, sparse = sparse)
}

#' @name geos_binary_pred
#' @export
#' @param dist distance threshold; geometry indexes with distances smaller or equal to this value are returned; numeric value or units value having distance units.
#' @details \code{st_is_within_distance} returns a sparse matrix only, and can only be used for non-geographic (Cartesian) coordinates; use \code{st_distance(x,y) <= dist} to obtain the corresponding dense logical matrix.
st_is_within_distance = function(x, y, dist, sparse = TRUE, prepared = FALSE) {
	if (isTRUE(st_is_longlat(x)))
		stop("st_is_within_distance only supported for Cartesian coordinates")
	if (! is.na(st_crs(x))) {
		p = crs_parameters(st_crs(x))
		units(dist) = p$ud_unit
	}
	if (prepared)
		stop("prepared geometries not supported for st_is_within_distance")
	st_geos_binop("is_within_distance", x, y, par = dist, sparse = sparse)
}

# unary, returning geometries

#' Geometric unary operations on simple feature geometry sets
#'
#' Geometric unary operations on simple feature geometry sets. These are all generics, with methods for \code{sfg}, \code{sfc} and \code{sf} objects, returning an object of the same class.
#' @name geos_unary
#' @param x object of class \code{sfg}, \code{sfg} or \code{sf}
#' @param dist numeric; buffer distance for all, or for each of the elements in \code{x}; in case
#' \code{dist} is a \code{units} object, it should be convertible to \code{arc_degree} if
#' \code{x} has geographic coordinates, and to \code{st_crs(x)$units} otherwise
#' @param nQuadSegs integer; number of segments per quadrant (fourth of a circle)
#' @return an object of the same class of \code{x}, with manipulated geometry.
#' @export
st_buffer = function(x, dist, nQuadSegs = 30)
	UseMethod("st_buffer")

#' @export
st_buffer.sfg = function(x, dist, nQuadSegs = 30)
	get_first_sfg(st_buffer(st_sfc(x), dist, nQuadSegs = nQuadSegs))

#' @export
st_buffer.sfc = function(x, dist, nQuadSegs = 30) {
	if (isTRUE(st_is_longlat(x))) {
		warning("st_buffer does not correctly buffer longitude/latitude data")
		if (inherits(dist, "units"))
			dist = units::set_units(dist, "arc_degrees")
		else
			message("dist is assumed to be in decimal degrees (arc_degrees).")
	} else if (inherits(dist, "units")) {
		if (is.na(st_crs(x)))
			stop("x does not have a crs set: can't convert units")
		if (is.null(st_crs(x)$units))
			stop("x has a crs without units: can't convert units")
		units(dist) = units::set_units(dist, udunits_from_proj[st_crs(x)$units])
	}
	dist = rep(dist, length.out = length(x))
	st_sfc(CPL_geos_op("buffer", x, dist, nQuadSegs))
}

#' @export
st_buffer.sf <- function(x, dist, nQuadSegs = 30) {
	st_geometry(x) <- st_buffer(st_geometry(x), dist, nQuadSegs)
	x
}

#' @name geos_unary
#' @export
st_boundary = function(x)
	UseMethod("st_boundary")

#' @export
st_boundary.sfg = function(x)
	get_first_sfg(st_boundary(st_sfc(x)))

#' @export
st_boundary.sfc = function(x)
	st_sfc(CPL_geos_op("boundary", x, numeric(0)))

#' @export
st_boundary.sf = function(x) {
	st_geometry(x) <- st_boundary(st_geometry(x))
	x
}

#' @name geos_unary
#' @export
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' plot(st_convex_hull(nc))
#' plot(nc, border = grey(.5))
st_convex_hull = function(x)
	UseMethod("st_convex_hull")

#' @export
st_convex_hull.sfg = function(x)
	get_first_sfg(st_convex_hull(st_sfc(x)))

#' @export
st_convex_hull.sfc = function(x)
	st_sfc(CPL_geos_op("convex_hull", x, numeric(0)))

#' @export
st_convex_hull.sf = function(x) {
	st_geometry(x) <- st_convex_hull(st_geometry(x))
	x
}

#' @name geos_unary
#' @export
#' @param preserveTopology logical; carry out topology preserving simplification?
#' @param dTolerance numeric; tolerance parameter
st_simplify = function(x, preserveTopology = FALSE, dTolerance = 0.0)
	UseMethod("st_simplify")

#' @export
st_simplify.sfg = function(x, preserveTopology = FALSE, dTolerance = 0.0)
	get_first_sfg(st_simplify(st_sfc(x), preserveTopology, dTolerance = dTolerance))

#' @export
st_simplify.sfc = function(x, preserveTopology = FALSE, dTolerance = 0.0) {
	if (isTRUE(st_is_longlat(x)))
		warning("st_simplify does not correctly simplify longitude/latitude data, dTolerance needs to be in decimal degrees")
	st_sfc(CPL_geos_op("simplify", x, numeric(0), preserveTopology = preserveTopology, dTolerance = dTolerance))
}

#' @export
st_simplify.sf = function(x, preserveTopology = FALSE, dTolerance = 0.0) {
	st_geometry(x) <- st_simplify(st_geometry(x), preserveTopology, dTolerance)
	x
}

#' @name geos_unary
#' @export
#' @param bOnlyEdges logical; if TRUE, return lines, else return polygons
#' @details \code{st_triangulate} requires GEOS version 3.4 or above
st_triangulate = function(x, dTolerance = 0.0, bOnlyEdges = FALSE)
	UseMethod("st_triangulate")

#' @export
st_triangulate.sfg = function(x, dTolerance = 0.0, bOnlyEdges = FALSE)
	get_first_sfg(st_triangulate(st_sfc(x), dTolerance, bOnlyEdges = bOnlyEdges))

#' @export
st_triangulate.sfc = function(x, dTolerance = 0.0, bOnlyEdges = FALSE) {
	if (CPL_geos_version() >= "3.4.0") {
		if (isTRUE(st_is_longlat(x)))
			warning("st_triangulate does not correctly triangulate longitude/latitude data")
		st_sfc(CPL_geos_op("triangulate", x, numeric(0), dTolerance = dTolerance, bOnlyEdges = bOnlyEdges))
	} else
		stop("for triangulate, GEOS version 3.4.0 or higher is required")
}

#' @export
st_triangulate.sf = function(x, dTolerance = 0.0, bOnlyEdges = FALSE) {
	st_geometry(x) <- st_triangulate(st_geometry(x), dTolerance, bOnlyEdges)
	x
}

#' @name geos_unary
#' @export
#' @param envelope object of class \code{sfc} or \code{sfg} containing a \code{POLYGON} with the envelope for a voronoi diagram; this only takes effect when it is larger than the default envelope, chosen when \code{envelope} is an empty polygon
#' @details \code{st_voronoi} requires GEOS version 3.5 or above
#' @examples
#' set.seed(1)
#' x = st_multipoint(matrix(runif(10),,2))
#' box = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
#' if (sf_extSoftVersion()["GEOS"] >= "3.5.0") {
#'  v = st_sfc(st_voronoi(x, st_sfc(box)))
#'  plot(v, col = 0, border = 1, axes = TRUE)
#'  plot(box, add = TRUE, col = 0, border = 1) # a larger box is returned, as documented
#'  plot(x, add = TRUE, col = 'red', cex=2, pch=16)
#'  plot(st_intersection(st_cast(v), box)) # clip to smaller box
#'  plot(x, add = TRUE, col = 'red', cex=2, pch=16)
#' }
st_voronoi = function(x, envelope, dTolerance = 0.0, bOnlyEdges = FALSE)
	UseMethod("st_voronoi")

#' @export
st_voronoi.sfg = function(x, envelope = st_polygon(), dTolerance = 0.0, bOnlyEdges = FALSE)
	get_first_sfg(st_voronoi(st_sfc(x), st_sfc(envelope), dTolerance, bOnlyEdges = bOnlyEdges))

#' @export
st_voronoi.sfc = function(x, envelope = st_polygon(), dTolerance = 0.0, bOnlyEdges = FALSE) {
	if (sf_extSoftVersion()["GEOS"] >= "3.5.0") {
		if (isTRUE(st_is_longlat(x)))
			warning("st_voronoi does not correctly triangulate longitude/latitude data")
		st_sfc(CPL_geos_voronoi(x, st_sfc(envelope), dTolerance = dTolerance, bOnlyEdges = bOnlyEdges))
	} else
		stop("for voronoi, GEOS version 3.5.0 or higher is required")
}

#' @export
st_voronoi.sf = function(x, envelope = st_polygon(), dTolerance = 0.0, bOnlyEdges = FALSE) {
	st_geometry(x) <- st_voronoi(st_geometry(x), st_sfc(envelope), dTolerance, bOnlyEdges)
	x
}

#' @name geos_unary
#' @details in case of \code{st_polygonize}, \code{x} must be an object of class \code{LINESTRING} or \code{MULTILINESTRING}, or an \code{sfc} geometry list-column object containing these
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
	st_sfc(CPL_geos_op("polygonize", x, numeric(0)))
}

#' @export
st_polygonize.sf = function(x) {
	st_geometry(x) <- st_polygonize(st_geometry(x))
	x
}

#' @name geos_unary
#' @export
#' @details in case of \code{st_line_merge}, \code{x} must be an object of class \code{MULTILINESTRING}, or an \code{sfc} geometry list-column object containing these
#' @examples
#' mls = st_multilinestring(list(rbind(c(0,0), c(1,1)), rbind(c(2,0), c(1,1))))
#' st_line_merge(st_sfc(mls))
st_line_merge = function(x)
	UseMethod("st_line_merge")

#' @export
st_line_merge.sfg = function(x)
	get_first_sfg(st_line_merge(st_sfc(x)))

#' @export
st_line_merge.sfc = function(x) {
	stopifnot(inherits(x, "sfc_MULTILINESTRING"))
	st_sfc(CPL_geos_op("linemerge", x, numeric(0)))
}

#' @export
st_line_merge.sf = function(x) {
	st_geometry(x) <- st_line_merge(st_geometry(x))
	x
}

#' @name geos_unary
#' @param of_largest_polygon logical; for \code{st_centroid}: if \code{TRUE}, return centroid of the largest (sub)polygon of a \code{MULTIPOLYGON} rather than of the whole \code{MULTIPOLYGON}
#' @export
#' @examples
#' plot(nc, axes = TRUE)
#' plot(st_centroid(nc), add = TRUE, pch = 3)
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
	spl = split(pols, rep(1:length(x), attr(pols, "ids")))
	st_sfc(lapply(spl, function(y) y[[which.max(st_area(y))]]), crs = st_crs(x))
}

#' @export
st_centroid.sfc = function(x, ..., of_largest_polygon = FALSE) {
	if (of_largest_polygon)
		x = largest_ring(x)
	if (isTRUE(st_is_longlat(x)))
		warning("st_centroid does not give correct centroids for longitude/latitude data")
	st_sfc(CPL_geos_op("centroid", x, numeric(0)))
}

#' @export
st_centroid.sf = function(x, ..., of_largest_polygon = FALSE) {
	st_geometry(x) <- st_centroid(st_geometry(x), of_largest_polygon = of_largest_polygon)
	x
}

#' @name geos_unary
#' @export
#' @details \code{st_point_on_surface} returns a point guaranteed to be on the (multi)surface.
#' @examples
#' plot(nc, axes = TRUE)
#' plot(st_point_on_surface(nc), add = TRUE, pch = 3)
st_point_on_surface = function(x)
	UseMethod("st_point_on_surface")

#' @export
st_point_on_surface.sfg = function(x)
	get_first_sfg(st_point_on_surface(st_sfc(x)))

#' @export
st_point_on_surface.sfc = function(x) {
	if (isTRUE(st_is_longlat(x)))
		warning("st_point_on_surface may not give correct results for longitude/latitude data")
	st_sfc(CPL_geos_op("point_on_surface", x, numeric(0)))
}

#' @export
st_point_on_surface.sf = function(x) {
	st_geometry(x) <- st_point_on_surface(st_geometry(x))
	x
}

#' @name geos_unary
#' @export
#' @details \code{st_node} add nodes to linear geometries at intersections without node
#' @examples
#' l = st_linestring(rbind(c(0,0), c(1,1), c(0,1), c(1,0), c(0,0)))
#' st_polygonize(st_node(l))
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
	st_sfc(CPL_geos_op("node", x, numeric(0)))
}

#' @export
st_node.sf = function(x) {
	st_set_geometry(x, st_node(st_geometry(x)))
}

#' @name geos_unary
#' @export
#' @param dfMaxLength maximum length of a line segment. If \code{x} has geographical coordinates (long/lat), \code{dfMaxLength} is either a numeric expressed in meter, or an object of class \code{units} with length units or unit \code{rad}, and segmentation takes place along the great circle, using \link[geosphere]{gcIntermediate}.
#' @param ... ignored
#' @examples
#' sf = st_sf(a=1, geom=st_sfc(st_linestring(rbind(c(0,0),c(1,1)))), crs = 4326)
#' seg = st_segmentize(sf, units::set_units(100, km))
#' seg = st_segmentize(sf, units::set_units(0.01, rad))
#' nrow(seg$geom[[1]])
st_segmentize	= function(x, dfMaxLength, ...)
	UseMethod("st_segmentize")

#' @export
st_segmentize.sfg = function(x, dfMaxLength, ...)
	get_first_sfg(st_segmentize(st_sfc(x), dfMaxLength, ...))

#' @export
st_segmentize.sfc	= function(x, dfMaxLength, ...) {
	if (isTRUE(st_is_longlat(x)))
		st_sfc(lapply(x, ll_segmentize, dfMaxLength = dfMaxLength, crs = st_crs(x)), crs = st_crs(x))
	else {
		if (!is.na(st_crs(x)) && inherits(dfMaxLength, "units"))
			units(dfMaxLength) = units(crs_parameters(st_crs(x))$SemiMajor) # might convert
		st_sfc(CPL_gdal_segmentize(x, dfMaxLength), crs = st_crs(x))
	}
}

#' @export
st_segmentize.sf = function(x, dfMaxLength, ...) {
	st_geometry(x) <- st_segmentize(st_geometry(x), dfMaxLength, ...)
	x
}

ll_segmentize = function(x, dfMaxLength, crs = st_crs(4326)) {
	# x is a single sfg: LINESTRING or MULTILINESTRING
	if (is.list(x)) # MULTILINESTRING:
		structure(lapply(x, ll_segmentize, dfMaxLength = dfMaxLength, crs = crs),
			class = attr(x, "class"))
	else { # matrix
		if (!requireNamespace("geosphere", quietly = TRUE))
			stop("package geosphere required, please install it first")
		p = crs_parameters(crs)
		pts = unclass(x) # matrix
		p1 = head(pts, -1)
		p2 = tail(pts, -1)
		ll = geosphere::distGeo(p1, p2, as.numeric(p$SemiMajor), 1./p$InvFlattening)
		if (inherits(dfMaxLength, "units")) {
			if (as.character(units(dfMaxLength)) == "rad")
				dfMaxLength = as.numeric(dfMaxLength) * p$SemiMajor
			units(ll) = units(p$SemiMajor)
		}
		n = as.numeric(ceiling(ll / dfMaxLength)) - 1
		ret = geosphere::gcIntermediate(p1, p2, n, addStartEnd = TRUE)
		if (length(n) == 1) # would be a matrix otherwise
			ret = list(ret)
		for (i in seq_along(n)) {
			if (n[i] < 1) # 0 or -1, because of the -1, for 0 distance
				ret[[i]] = ret[[i]][-2,] # take out interpolated middle point
			if (i > 1)
				ret[[i]] = tail(ret[[i]], -1) # take out duplicate starting point
		}
		structure(do.call(rbind, ret), class = attr(x, "class"))
	}
}

#' Combine or union feature geometries
#'
#' Combine several feature geometries into one, with or without resolving internal boundaries
#' @name geos_combine
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @return \code{st_combine} returns a single, combined geometry, with no resolved boundaries.
#' @export
#' @details \code{st_combine} combines geometries without resolving borders, using \link{c.sfg} (analogous to \link[base]{c} for ordinary vectors).
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' st_combine(nc)
st_combine = function(x)
	st_sfc(do.call(c, st_geometry(x)), crs = st_crs(x)) # flatten/merge

# x: object of class sf
# y: object of calss sf or sfc
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
		df = cbind(df, y[idx[,2], , drop = FALSE])
	}
	if (! (all_constant_x && all_constant_y))
		warning("attribute variables are assumed to be spatially constant throughout all geometries",
			call. = FALSE)
	if (inherits(x, "tbl_df")) {
		if (!requireNamespace("dplyr", quietly = TRUE))
			stop("package dplyr required: install first?")
		df = dplyr::tbl_df(df)
	}
	df[[ attr(x, "sf_column") ]] = geoms
	st_sf(df, sf_column_name = attr(x, "sf_column"))
}

# after checking identical crs,
# call geos_op2 function op on x and y:
geos_op2_geom = function(op, x, y) {
	stopifnot(st_crs(x) == st_crs(y))
	if (isTRUE(st_is_longlat(x)))
		message_longlat(paste0("st_", op))
	st_sfc(CPL_geos_op2(op, st_geometry(x), st_geometry(y)), crs = st_crs(x))
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
#' @export
#' @return The intersection, difference or symmetric difference between two sets of geometries.
#' The returned object has the same class as that of the first argument (\code{x}) with the non-empty geometries resulting from applying the operation to all geometry pairs in \code{x} and \code{y}. In case \code{x} is of class \code{sf}, the matching attributes of the original object(s) are added. The \code{sfc} geometry list-column returned carries an attribute \code{idx}, which is an \code{n}-by-2 matrix with every row the index of the corresponding entries of \code{x} and \code{y}, respectively.
#' @details A spatial index is built on argument \code{x}; see \url{http://r-spatial.org/r/2017/06/22/spatial-index.html}. The reference for the STR tree algorithm is: Leutenegger, Scott T., Mario A. Lopez, and Jeffrey Edgington. "STR: A simple and efficient algorithm for R-tree packing." Data Engineering, 1997. Proceedings. 13th international conference on. IEEE, 1997. For the pdf, search Google Scholar.
#' @seealso \link{st_union} for the union of simple features collections; \link{intersect} and \link{setdiff} for the base R set operations.
#' @export
st_intersection = function(x, y) UseMethod("st_intersection")

#' @export
st_intersection.sfg = function(x, y)
	get_first_sfg(geos_op2_geom("intersection", x, y))

#' @export
st_intersection.sfc = function(x, y)
	geos_op2_geom("intersection", x, y)

#' @export
st_intersection.sf = function(x, y)
	geos_op2_df(x, y, geos_op2_geom("intersection", x, y))

#' @name geos_binary_ops
#' @export
#' @examples
#' # A helper function that erases all of y from x:
#' st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
st_difference = function(x, y) UseMethod("st_difference")

#' @export
st_difference.sfg = function(x, y)
	get_first_sfg(geos_op2_geom("difference", x, y))

#' @export
st_difference.sfc = function(x, y)
	geos_op2_geom("difference", x, y)

#' @export
st_difference.sf = function(x, y)
	geos_op2_df(x, y, geos_op2_geom("difference", x, y))

#' @name geos_binary_ops
#' @export
st_sym_difference = function(x, y) UseMethod("st_sym_difference")

#' @export
st_sym_difference.sfg = function(x, y)
	get_first_sfg(geos_op2_geom("sym_difference", x, y))

#' @export
st_sym_difference.sfc = function(x, y)
	geos_op2_geom("sym_difference", x, y)

#' @export
st_sym_difference.sf = function(x, y)
	geos_op2_df(x, y, geos_op2_geom("sym_difference", x, y))

#' @name geos_combine
#' @export
#' @param by_feature logical; if TRUE, union each feature, if FALSE return a single feature that is the geometric union of the set of features
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg} (optional)
#' @param ... ignored
#' @seealso \link{st_intersection}, \link{st_difference}, \link{st_sym_difference}
#' @return If \code{y} is missing, \code{st_union(x)} returns a single geometry with resolved boundaries, else the geometries for all unioned pairs of x[i] and y[j].
#' @details
#' If \code{st_union} is called with a single argument, \code{x}, (with \code{y} missing) and \code{by_feature} is \code{FALSE} all geometries are unioned together and an \code{sfg} or single-geometry \code{sfc} object is returned. If \code{by_feature} is \code{TRUE} each feature geometry is unioned. This can for instance be used to resolve internal boundaries after polygons were combined using \code{st_combine}. If \code{y} is provided, all elements of \code{x} and \code{y} are unioned, pairwise (and \code{by_feature} is ignored). The former corresponds to \link[rgeos]{gUnaryUnion}, the latter to \link[rgeos]{gUnion}.
#'
#' Unioning a set of overlapping polygons has the effect of merging the areas (i.e. the same effect as iteratively unioning all individual polygons together). Unioning a set of LineStrings has the effect of fully noding and dissolving the input linework. In this context "fully noded" means that there will be a node or endpoint in the output for every endpoint or line segment crossing in the input. "Dissolved" means that any duplicate (e.g. coincident) line segments or portions of line segments will be reduced to a single line segment in the output.	Unioning a set of Points has the effect of merging all identical points (producing a set with no duplicates).
#' @examples
#' plot(st_union(nc))
st_union = function(x, y, ..., by_feature = FALSE) UseMethod("st_union")

#' @export
st_union.sfg = function(x, y, ..., by_feature = FALSE) {
	out = if (missing(y)) # unary union, possibly by_feature:
		st_sfc(CPL_geos_union(st_geometry(x), by_feature))
	else
		st_union(st_geometry(x), st_geometry(y))
	get_first_sfg(out)
}

#' @export
st_union.sfc = function(x, y, ..., by_feature = FALSE) {
	if (missing(y)) # unary union, possibly by_feature:
		st_sfc(CPL_geos_union(st_geometry(x), by_feature))
	else
		geos_op2_geom("union", x, y)
}

#' @export
st_union.sf = function(x, y, ..., by_feature = FALSE) {
	if (missing(y)) { # unary union, possibly by_feature:
		geom = st_sfc(CPL_geos_union(st_geometry(x), by_feature))
		if (by_feature)
			st_set_geometry(x, geom)
		else
			geom
	} else
		geos_op2_df(x, y, geos_op2_geom("union", x, y))
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
			if (!is.na(st_crs(x)) && inherits(density, "units"))
				units(density) = 1/crs_parameters(st_crs(x))$ud_unit # coordinate units
			round(rep(density, length.out = length(l)) * l)
		} else
			rep(n, length.out = length(l))
		regular = function(n) { (1:n - 0.5)/n }
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

#' Make a rectangular grid over the bounding box of a sf or sfc object
#'
#' Make a rectangular grid over the bounding box of a sf or sfc object
#' @param x object of class \link{sf} or \link{sfc}
#' @param cellsize target cellsize
#' @param offset numeric of lengt 2; lower left corner coordinates (x, y) of the grid
#' @param n integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @param crs object of class \code{crs}
#' @param what character; one of: \code{"polygons"}, \code{"corners"}, or \code{"centers"}
#' @return Object of class \code{sfc} (simple feature geometry list column) with, depending on \code{what},
#' rectangular polygons, corner points of these polygons, or center points of these polygons.
#' @examples
#' plot(st_make_grid(what = "centers"), axes = TRUE)
#' plot(st_make_grid(what = "corners"), add = TRUE, col = 'green', pch=3)
#' @export
st_make_grid = function(x,
		cellsize = c(diff(st_bbox(x)[c(1,3)]), diff(st_bbox(x)[c(2,4)]))/n,
		offset = st_bbox(x)[1:2], n = c(10, 10),
		crs = if (missing(x)) NA_crs_ else st_crs(x),
		what = "polygons") {

	if (missing(x) && missing(cellsize) && missing(offset)
			&& missing(n) && missing(crs)) # create global 10 x 10 degree grid
		return(st_make_grid(cellsize = c(10,10), offset = c(-180,-90), n = c(36,18),
			crs = st_crs(4326), what = what))

	bb = if (!missing(n) && !missing(offset) && !missing(cellsize)) {
		cellsize = rep(cellsize, length.out = 2)
		n = rep(n, length.out = 2)
		structure(c(offset, offset + n * cellsize),
			names = c("xmin", "ymin", "xmax", "ymax"))
	} else
		st_bbox(x)

	if (! missing(cellsize))
		cellsize = rep(cellsize, length.out = 2)

	if (missing(n)) {
		nx = ceiling((bb[3] - offset[1])/cellsize[1])
		ny = ceiling((bb[4] - offset[2])/cellsize[2])
	} else {
		n = rep(n, length.out = 2)
		nx = n[1]
		ny = n[2]
	}

	# corner points:
	xc = seq(offset[1], bb[3], length.out = nx + 1)
	yc = seq(offset[2], bb[4], length.out = ny + 1)

	if (what == "polygons") {
		ret = vector("list", nx * ny)
		square = function(x1, y1, x2, y2)
			st_polygon(list(matrix(c(x1, x2, x2, x1, x1, y1, y1, y2, y2, y1), 5)))
		for (i in 1:nx)
			for (j in 1:ny)
				ret[[(j - 1) * nx + i]] = square(xc[i], yc[j], xc[i+1], yc[j+1])
	} else if (what == "centers") {
		ret = vector("list", nx * ny)
		cent = function(x1, y1, x2, y2)
			st_point(c( (x1+x2)/2, (y1+y2)/2 ))
		for (i in 1:nx)
			for (j in 1:ny)
				ret[[(j - 1) * nx + i]] = cent(xc[i], yc[j], xc[i+1], yc[j+1])
	} else if (what == "corners") {
		ret = vector("list", (nx + 1) * (ny + 1))
		for (i in 1:(nx + 1))
			for (j in 1:(ny + 1))
				ret[[(j - 1) * (nx + 1) + i]] = st_point(c(xc[i], yc[j]))
	} else
		stop("unknown value of `what'")

	if (missing(x))
		st_sfc(ret, crs = crs)
	else
		st_sfc(ret, crs = st_crs(x))
}
