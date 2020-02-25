# unary, interfaced through GEOS:

#' Dimension, simplicity, validity or is_empty queries on simple feature geometries
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

#' @name geos_query
#' @export
#' @return st_is_empty returns for each geometry whether it is empty
#' @examples
#' ls = st_linestring(rbind(c(0,0), c(1,1), c(1,0), c(0,1)))
#' st_is_empty(st_sfc(ls, st_point(), st_linestring()))
st_is_empty = function(x) CPL_geos_is_empty(st_geometry(x))

#' @name geos_measures
#' @export
#' @return If the coordinate reference system of \code{x} was set, these functions return values with unit of measurement; see \link[units]{set_units}.
#'
#' st_area returns the area of a geometry, in the coordinate reference system used; in case \code{x} is in degrees longitude/latitude, \link[lwgeom]{st_geod_area} is used for area calculation.
#' @examples
#' b0 = st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
#' b1 = b0 + 2
#' b2 = b0 + c(-0.2, 2)
#' x = st_sfc(b0, b1, b2)
#' st_area(x)
st_area = function(x, ...) UseMethod("st_area")

#' @export
st_area.sfc = function(x, ...) {
	if (isTRUE(st_is_longlat(x))) {
		if (! requireNamespace("lwgeom", quietly = TRUE))
			stop("package lwgeom required, please install it first")
		lwgeom::st_geod_area(x)
	} else {
		a = CPL_area(x) # ignores units: units of coordinates
		if (! is.na(st_crs(x))) {
			units(a) = crs_parameters(st_crs(x))$ud_unit^2 # coord units
			if (!is.null(to_m <- st_crs(x)$to_meter))
				a = a * to_m^2
		}
		a
	}
}

#' @export
st_area.sf = function(x, ...) st_area(st_geometry(x, ...))

#' @export
st_area.sfg = function(x, ...) st_area(st_geometry(x, ...))

#' @name geos_measures
#' @export
#' @return st_length returns the length of a \code{LINESTRING} or \code{MULTILINESTRING} geometry, using the coordinate reference system.  \code{POINT}, \code{MULTIPOINT}, \code{POLYGON} or \code{MULTIPOLYGON} geometries return zero.
#' @seealso \link{st_dimension}, \link{st_cast} to convert geometry types
#'
#' @examples
#' line = st_sfc(st_linestring(rbind(c(30,30), c(40,40))), crs = 4326)
#' st_length(line)
#'
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#'
#' poly = st_polygon(list(outer, hole1, hole2))
#' mpoly = st_multipolygon(list(
#' 	list(outer, hole1, hole2),
#' 	list(outer + 12, hole1 + 12)
#' ))
#'
#' st_length(st_sfc(poly, mpoly))
st_length = function(x) {
	x = st_geometry(x)

	if (isTRUE(st_is_longlat(x))) {
		if (! requireNamespace("lwgeom", quietly = TRUE))
			stop("package lwgeom required, please install it first")
		lwgeom::st_geod_length(x)
	} else {
		ret = CPL_length(x)
		ret[is.nan(ret)] = NA
		crs = st_crs(x)
		if (! is.na(crs)) {
			units(ret) = crs_parameters(crs)$ud_unit
			if (!is.null(to_m <- st_crs(x)$to_meter))
				ret = ret * to_m
		}
		ret
	}
}

message_longlat = function(caller) {
	message(paste("although coordinates are longitude/latitude,",
		caller, "assumes that they are planar"))
}

is_symmetric = function(operation, pattern) {
	if (!is.na(pattern)) {
		m = matrix(sapply(1:9, function(i) substr(pattern, i, i)), 3, 3)
		isTRUE(all(m == t(m)))
	} else
		isTRUE(operation %in% c("intersects", "touches", "overlaps", "disjoint", "equals"))
}

# binary, interfaced through GEOS:

# returning matrix, distance or relation string -- the work horse is:

st_geos_binop = function(op, x, y, par = 0.0, pattern = NA_character_,
		sparse = TRUE, prepared = FALSE) {
	if (missing(y))
		y = x
	else if (!inherits(x, "sfg") && !inherits(y, "sfg"))
		stopifnot(st_crs(x) == st_crs(y))
	if (isTRUE(st_is_longlat(x)) && !(op %in% c("equals", "equals_exact", "polygonize")))
		message_longlat(paste0("st_", op))
	if (prepared && is_symmetric(op, pattern) &&
			length(dx <- st_dimension(x)) && length(dy <- st_dimension(y)) &&
			isTRUE(all(dx == 0)) && isTRUE(all(dy == 2))) {
		t(st_geos_binop(op, y, x, par = par, pattern = pattern, sparse = sparse, prepared = prepared))
	} else {
		ret = CPL_geos_binop(st_geometry(x), st_geometry(y), op, par, pattern, prepared)
		if (length(ret) == 0 || is.null(dim(ret[[1]]))) {
			id = if (is.null(row.names(x)))
					as.character(1:length(ret))
				else
					row.names(x)
			sgbp = sgbp(ret, predicate = op, region.id = id, ncol = length(st_geometry(y)))
			if (! sparse)
				as.matrix(sgbp)
			else
				sgbp
		} else # CPL_geos_binop returned a matrix, e.g. from op = "relate"
			ret[[1]]
	}
}

#' Compute geometric measurements
#'
#' Compute Euclidian or great circle distance between pairs of geometries; compute, the area or the length of a set of geometries.
#' @name geos_measures
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}, defaults to \code{x}
#' @param ... ignored
#' @param dist_fun deprecated
#' @param by_element logical; if \code{TRUE}, return a vector with distance between the first elements of \code{x} and \code{y}, the second, etc. if \code{FALSE}, return the dense matrix with all pairwise distances.
#' @param which character; for Cartesian coordinates only: one of \code{Euclidean}, \code{Hausdorff} or \code{Frechet}; for geodetic coordinates, great circle distances are computed; see details
#' @param par for \code{which} equal to \code{Hausdorff} or \code{Frechet}, optionally use a value between 0 and 1 to densify the geometry
#' @param tolerance ignored if \code{st_is_longlat(x)} is \code{FALSE}; otherwise, if set to a positive value, the first distance smaller than \code{tolerance} will be returned, and true distance may be smaller; this may speed up computation. In meters, or a \code{units} object convertible to meters.
#' @return If \code{by_element} is \code{FALSE} \code{st_distance} returns a dense numeric matrix of dimension length(x) by length(y); otherwise it returns a numeric vector of length \code{x} or \code{y}, the shorter one being recycled. Distances involving empty geometries are \code{NA}.
#' @details great circle distance calculations use function \code{geod_inverse} from PROJ; see Karney, Charles FF, 2013, Algorithms for geodesics, Journal of Geodesy 87(1), 43--55
#' @examples
#' p = st_sfc(st_point(c(0,0)), st_point(c(0,1)), st_point(c(0,2)))
#' st_distance(p, p)
#' st_distance(p, p, by_element = TRUE)
#' @export
st_distance = function(x, y, ..., dist_fun, by_element = FALSE, 
		which = ifelse(isTRUE(st_is_longlat(x)), "Great Circle", "Euclidean"), 
		par = 0.0, tolerance = 0.0) {
	if (missing(y))
		y = x
	else
		stopifnot(st_crs(x) == st_crs(y))

	if (! missing(dist_fun))
		stop("dist_fun is deprecated: lwgeom is used for distance calculation")

	x = st_geometry(x)
	y = st_geometry(y)

	if (isTRUE(st_is_longlat(x))) {
		if (! requireNamespace("lwgeom", quietly = TRUE))
			stop("lwgeom required: install first?")
		if (which != "Great Circle")
			stop("for non-great circle distances, data should be projected; see st_transform()")
		units(tolerance) = as_units("m")
		if (by_element) {
			crs = st_crs(x)
			dist_ll = function(x, y, tolerance)
				lwgeom::st_geod_distance(st_sfc(x, crs = crs), st_sfc(y, crs = crs),
					tolerance = tolerance)
			d = mapply(dist_ll, x, y, tolerance = tolerance)
			units(d) = units(crs_parameters(st_crs(x))$SemiMajor)
			d
		} else
			lwgeom::st_geod_distance(x, y, tolerance)
	} else {
		d = if (by_element)
				mapply(st_distance, x, y, by_element = FALSE, which = which, par = par)
			else
				CPL_geos_dist(x, y, which, par)
		if (! is.na(st_crs(x)))
			units(d) = crs_parameters(st_crs(x))$ud_unit
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
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}; if missing, \code{x} is used
#' @param sparse logical; should a sparse index list be returned (TRUE) or a dense logical matrix? See below.
#' @param ... ignored
#' @param prepared logical; prepare geometry for x, before looping over y? See Details.
#' @details If \code{prepared} is \code{TRUE}, and \code{x} contains POINT geometries and \code{y} contains polygons, then the polygon geometries are prepared, rather than the points.
#' @return If \code{sparse=FALSE}, \code{st_predicate} (with \code{predicate} e.g. "intersects") returns a dense logical matrix with element \code{i,j} \code{TRUE} when \code{predicate(x[i], y[j])} (e.g., when geometry of feature i and j intersect); if \code{sparse=TRUE}, an object of class \code{\link{sgbp}} with a sparse list representation of the same matrix, with list element \code{i} an integer vector with all indices j for which \code{predicate(x[i],y[j])} is \code{TRUE} (and hence \code{integer(0)} if none of them is \code{TRUE}). From the dense matrix, one can find out if one or more elements intersect by \code{apply(mat, 1, any)}, and from the sparse list by \code{lengths(lst) > 0}, see examples below.
#' @details For most predicates, a spatial index is built on argument \code{x}; see \url{http://r-spatial.org/r/2017/06/22/spatial-index.html}.
#' Specifically, \code{st_intersects}, \code{st_disjoint}, \code{st_touches} \code{st_crosses}, \code{st_within}, \code{st_contains}, \code{st_contains_properly}, \code{st_overlaps}, \code{st_equals}, \code{st_covers} and \code{st_covered_by} all build spatial indexes for more efficient geometry calculations. \code{st_relate}, \code{st_equals_exact}, and \code{st_is_within_distance} do not.
#'
#' If \code{y} is missing, `st_predicate(x, x)` is effectively called, and a square matrix is returned with diagonal elements `st_predicate(x[i], x[i])`.
#'
#' Sparse geometry binary predicate (\code{\link{sgbp}}) lists have the following attributes: \code{region.id} with the \code{row.names} of \code{x} (if any, else \code{1:n}), \code{ncol} with the number of features in \code{y}, and \code{predicate} with the name of the predicate used.
#'
#' @note For intersection on pairs of simple feature geometries, use
#' the function \code{\link{st_intersection}} instead of \code{st_intersects}.
#'
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
st_intersects	= function(x, y, sparse = TRUE, ...) UseMethod("st_intersects")

#' @export
st_intersects.sfc = function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared)

#' @export
st_intersects.sf = function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared)

#' @export
st_intersects.sfg = function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared)


#' @name geos_binary_pred
#' @export
st_disjoint		= function(x, y = x, sparse = TRUE, prepared = TRUE) {
	# st_geos_binop("disjoint", x, y, sparse = sparse, prepared = prepared) -> didn't use STRtree
	int = st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared)
	# disjoint = !intersects :
	if (sparse)
		sgbp(lapply(int, function(g) setdiff(1:length(st_geometry(y)), g)),
			predicate = "disjoint",
			ncol = attr(int, "ncol"),
			region.id = attr(int, "region.id"))
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
st_is_within_distance = function(x, y, dist, sparse = TRUE) {
	if (isTRUE(st_is_longlat(x))) {
		if (missing(y))
			y = x
		gx = st_geometry(x)
		gy = st_geometry(y)
		units(dist) = as_units("m")
		if (sparse) {
			if (! requireNamespace("lwgeom", quietly = TRUE))
				stop("lwgeom required: install first?")
			ret = if (utils::packageVersion("lwgeom") <= "0.1-2")
					lapply(seq_along(gx), function(i) which(st_distance(gx[i], gy, tolerance = dist) <= dist))
				else
					lwgeom::st_geod_distance(x, y, tolerance = dist, sparse = TRUE)
			sgbp(ret, predicate = "is_within_distance", region.id = 1:length(x), ncol = length(gy))
		} else
			st_distance(x, y, tolerance = dist) <= dist
	} else {
		if (! is.na(st_crs(x)))
			units(dist) = crs_parameters(st_crs(x))$ud_unit # might convert
		if (! sparse)
			st_distance(x, y) <= dist
		else
			st_geos_binop("is_within_distance", x, y, par = dist, sparse = sparse)
	}
}

# unary, returning geometries

#' Geometric unary operations on simple feature geometry sets
#'
#' Geometric unary operations on simple feature geometries. These are all generics, with methods for \code{sfg}, \code{sfc} and \code{sf} objects, returning an object of the same class. All operations work on a per-feature basis, ignoring all other features.
#' @name geos_unary
#' @param x object of class \code{sfg}, \code{sfg} or \code{sf}
#' @param dist numeric; buffer distance for all, or for each of the elements in \code{x}; in case
#' \code{dist} is a \code{units} object, it should be convertible to \code{arc_degree} if
#' \code{x} has geographic coordinates, and to \code{st_crs(x)$units} otherwise
#' @param nQuadSegs integer; number of segments per quadrant (fourth of a circle), for all or per-feature
#' @param endCapStyle character; style of line ends, one of 'ROUND', 'FLAT', 'SQUARE'
#' @param joinStyle character; style of line joins, one of 'ROUND', 'MITRE', 'BEVEL'
#' @param mitreLimit numeric; limit of extension for a join if \code{joinStyle} 'MITRE' is used (default 1.0, minimum 0.0)
#' @param singleSide logical; if \code{TRUE}, single-sided buffers are returned for linear geometries, 
#' in which case negative \code{dist} values give buffers on the right-hand side, positive on the left.
#' @return an object of the same class of \code{x}, with manipulated geometry.
#' @export
#' @details \code{st_buffer} computes a buffer around this geometry/each geometry. If any of \code{endCapStyle},
#' \code{joinStyle}, or \code{mitreLimit} are set to non-default values ('ROUND', 'ROUND', 1.0 respectively) then
#' the underlying 'buffer with style' GEOS function is used.
#' See \href{https://postgis.net/docs/ST_Buffer.html}{postgis.net/docs/ST_Buffer.html} for details.
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
					 endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1.0, singleSide = FALSE)
	UseMethod("st_buffer")

#' @export
st_buffer.sfg = function(x, dist, nQuadSegs = 30,
						 endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1.0, singleSide = FALSE)
	get_first_sfg(st_buffer(st_sfc(x), dist, nQuadSegs = nQuadSegs, endCapStyle = endCapStyle, 
		joinStyle = joinStyle, mitreLimit = mitreLimit, singleSide = singleSide))

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
	if (any(is.na(singleSide))) stop("singleSide should be TRUE or FALSE")
	styls$with_styles = TRUE
	styls$endCapStyle = ecs
	styls$joinStyle = js
	styls$mitreLimit = mitreLimit
	styls
}
#' @export
st_buffer.sfc = function(x, dist, nQuadSegs = 30,
						 endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1.0, 
						 singleSide = FALSE) {
	if (isTRUE(st_is_longlat(x))) {
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
		units(dist) = crs_parameters(st_crs(x))$ud_unit
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

#' @export
st_buffer.sf = function(x, dist, nQuadSegs = 30,
						endCapStyle = "ROUND", joinStyle = "ROUND", mitreLimit = 1.0,
						singleSide = FALSE) {
	st_set_geometry(x, st_buffer(st_geometry(x), dist, nQuadSegs,
							   endCapStyle = endCapStyle, joinStyle = joinStyle, mitreLimit = mitreLimit,
							   singleSide = singleSide))
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
	st_geometry(x) <- st_boundary(st_geometry(x))
	x
}

#' @name geos_unary
#' @export
#' @details \code{st_convex_hull} creates the convex hull of a set of points
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
	st_sfc(CPL_geos_op("convex_hull", x, numeric(0), integer(0), numeric(0), logical(0)))

#' @export
st_convex_hull.sf = function(x) {
	st_geometry(x) <- st_convex_hull(st_geometry(x))
	x
}

#' @name geos_unary
#' @export
#' @details \code{st_simplify} simplifies lines by removing vertices
#' @param preserveTopology logical; carry out topology preserving simplification? May be specified for each, or for all feature geometries. Note that topology is preserved only for single feature geometries, not for sets of them.
#' @param dTolerance numeric; tolerance parameter, specified for all or for each feature geometry.
st_simplify = function(x, preserveTopology = FALSE, dTolerance = 0.0)
	UseMethod("st_simplify")

#' @export
st_simplify.sfg = function(x, preserveTopology = FALSE, dTolerance = 0.0)
	get_first_sfg(st_simplify(st_sfc(x), preserveTopology, dTolerance = dTolerance))

#' @export
st_simplify.sfc = function(x, preserveTopology = FALSE, dTolerance = 0.0) {
	if (isTRUE(st_is_longlat(x)))
		warning("st_simplify does not correctly simplify longitude/latitude data, dTolerance needs to be in decimal degrees")
	stopifnot(mode(preserveTopology) == 'logical')

	st_sfc(CPL_geos_op("simplify", x, numeric(0), integer(0),
		preserveTopology = rep(preserveTopology, length.out = length(x)),
		dTolerance = rep(dTolerance, length.out = length(x))))
}

#' @export
st_simplify.sf = function(x, preserveTopology = FALSE, dTolerance = 0.0) {
	st_geometry(x) <- st_simplify(st_geometry(x), preserveTopology, dTolerance)
	x
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
	if (CPL_geos_version() >= "3.4.0") {
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
	st_geometry(x) <- st_triangulate(st_geometry(x), dTolerance, bOnlyEdges)
	x
}

#' @name geos_unary
#' @export
#' @param envelope object of class \code{sfc} or \code{sfg} containing a \code{POLYGON} with the envelope for a voronoi diagram; this only takes effect when it is larger than the default envelope, chosen when \code{envelope} is an empty polygon
#' @details \code{st_voronoi} creates voronoi tesselation. \code{st_voronoi} requires GEOS version 3.5 or above
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
		st_sfc(CPL_geos_voronoi(x, st_sfc(envelope), dTolerance = dTolerance,
			bOnlyEdges = as.integer(bOnlyEdges)))
	} else
		stop("for voronoi, GEOS version 3.5.0 or higher is required")
}

#' @export
st_voronoi.sf = function(x, envelope = st_polygon(), dTolerance = 0.0, bOnlyEdges = FALSE) {
	st_geometry(x) <- st_voronoi(st_geometry(x), st_sfc(envelope), dTolerance, bOnlyEdges)
	x
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
	st_geometry(x) <- st_polygonize(st_geometry(x))
	x
}

#' @name geos_unary
#' @export
#' @details \code{st_line_merge} merges lines. In case of \code{st_line_merge}, \code{x} must be an object of class \code{MULTILINESTRING}, or an \code{sfc} geometry list-column object containing these
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
	st_sfc(CPL_geos_op("linemerge", x, numeric(0), integer(0), numeric(0), logical(0)))
}

#' @export
st_line_merge.sf = function(x) {
	st_geometry(x) <- st_line_merge(st_geometry(x))
	x
}

#' @name geos_unary
#' @param of_largest_polygon logical; for \code{st_centroid}: if \code{TRUE}, return centroid of the largest (sub)polygon of a \code{MULTIPOLYGON} rather than of the whole \code{MULTIPOLYGON}
#' @export
#' @details \code{st_centroid} gives the centroid of a geometry
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
	stopifnot(! is.null(attr(pols, "ids")))
	areas = st_area(pols)
	spl = split(areas, rep(1:length(x), attr(pols, "ids"))) # group by x
	l = c(0, head(cumsum(lengths(spl)), -1)) # 0-based indexes of first rings of a MULTIPOLYGON
	i = l + sapply(spl, which.max)           # add relative index of largest ring
	st_sfc(pols[i], crs = st_crs(x))
}

#' @export
st_centroid.sfc = function(x, ..., of_largest_polygon = FALSE) {
	if (isTRUE(st_is_longlat(x)))
		warning("st_centroid does not give correct centroids for longitude/latitude data")
	if (of_largest_polygon) {
		multi = which(sapply(x, inherits, what = "MULTIPOLYGON") & lengths(x) > 1)
		if (length(multi))
			x[multi] = largest_ring(x[multi])
	}
	st_sfc(CPL_geos_op("centroid", x, numeric(0), integer(0), numeric(0), logical(0)))
}

#' @export
st_centroid.sf = function(x, ..., of_largest_polygon = FALSE) {
	if (! all_constant(x))
		warning("st_centroid assumes attributes are constant over geometries of x")
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
	st_sfc(CPL_geos_op("point_on_surface", x, numeric(0), integer(0), numeric(0), logical(0)))
}

#' @export
st_point_on_surface.sf = function(x) {
	if (! all_constant(x))
		warning("st_point_on_surface assumes attributes are constant over geometries of x")
	st_set_geometry(x, st_point_on_surface(st_geometry(x)))
}

#' @name geos_unary
#' @export
#' @details \code{st_reverse} reverses the nodes in a line
#' @examples
#' if (sf_extSoftVersion()["GEOS"] >= "3.7.0") {
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
#' @param dfMaxLength maximum length of a line segment. If \code{x} has geographical coordinates (long/lat), \code{dfMaxLength} is either a numeric expressed in meter, or an object of class \code{units} with length units \code{rad} or \code{degree}; segmentation in the long/lat case takes place along the great circle, using \link[lwgeom]{st_geod_segmentize}.
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
	if (isTRUE(st_is_longlat(x))) {
		if (! requireNamespace("lwgeom", quietly = TRUE))
			stop("package lwgeom required, please install it first")
		if (! inherits(dfMaxLength, "units"))
			units(dfMaxLength) = as_units("m")
		lwgeom::st_geod_segmentize(x, dfMaxLength) # takes care of rad or degree units
	} else {
		if (! is.na(st_crs(x)) && inherits(dfMaxLength, "units"))
			units(dfMaxLength) = units(crs_parameters(st_crs(x))$SemiMajor) # might convert
		st_sfc(CPL_gdal_segmentize(x, dfMaxLength), crs = st_crs(x))
	}
}

#' @export
st_segmentize.sf = function(x, dfMaxLength, ...) {
	st_geometry(x) <- st_segmentize(st_geometry(x), dfMaxLength, ...)
	x
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
#' @note To find whether pairs of simple feature geometries intersect, use
#' the function \code{\link{st_intersects}} instead of \code{st_intersection}.
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
st_intersection = function(x, y) UseMethod("st_intersection")

#' @export
st_intersection.sfg = function(x, y)
	get_first_sfg(geos_op2_geom("intersection", x, y))

#' @name geos_binary_ops
#' @export
#' @details When called with missing \code{y}, the \code{sfc} method for \code{st_intersection} returns all non-empty intersections of the geometries of \code{x}; an attribute \code{idx} contains a list-column with the indexes of contributing geometries.
st_intersection.sfc = function(x, y) {
	if (missing(y)) {
		ret = CPL_nary_intersection(x)
		structure(st_sfc(ret), idx = attr(ret, "idx"))
	} else
		geos_op2_geom("intersection", x, y)
}

#' @name geos_binary_ops
#' @export
#' @details when called with a missing \code{y}, the \code{sf} method for \code{st_intersection} returns an \code{sf} object with attributes taken from the contributing feature with lowest index; two fields are added: \code{n.overlaps} with the number of overlapping features in \code{x}, and a list-column \code{origins} with indexes of all overlapping features.
st_intersection.sf = function(x, y) {
	if (missing(y)) {
		geom = st_intersection(st_geometry(x))
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
		geos_op2_df(x, y, geos_op2_geom("intersection", x, y))
}

#' @name geos_binary_ops
#' @export
#' @examples
#' # A helper function that erases all of y from x:
#' st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
st_difference = function(x, y) UseMethod("st_difference")

#' @export
st_difference.sfg = function(x, y)
	get_first_sfg(geos_op2_geom("difference", x, y))

#' @name geos_binary_ops
#' @export
#' @details When \code{st_difference} is called with a single argument,
#' overlapping areas are erased from geometries that are indexed at greater
#' numbers in the argument to \code{x}; geometries that are empty
#' or contained fully inside geometries with higher priority are removed entirely.
#' The \code{st_difference.sfc} method with a single argument returns an object with
#' an \code{"idx"} attribute with the orginal index for returned geometries.
st_difference.sfc = function(x, y) {
	if (missing(y)) {
		ret = CPL_nary_difference(x)
		structure(st_sfc(ret), ret = attr(ret, "idx"))
	} else
		geos_op2_geom("difference", x, y)
}

#' @export
st_difference.sf = function(x, y) {
	if (missing(y)) {
		geom = st_difference(st_geometry(x))
		sf_column = attr(x, "sf_column")
		st_geometry(x) = NULL
		x = x[attr(geom, "idx"), , drop=FALSE]
		x[[ sf_column ]] = structure(geom, idx = NULL)
		st_sf(x)
	} else
		geos_op2_df(x, y, geos_op2_geom("difference", x, y))
}

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

#' @name geos_binary_ops
#' @param tolerance tolerance values used for \code{st_snap}; numeric value or object of class \code{units}; may have tolerance values for each feature in \code{x}
#' @export
st_snap = function(x, y, tolerance) UseMethod("st_snap")

#' @export
st_snap.sfg = function(x, y, tolerance)
	get_first_sfg(st_snap(st_sfc(x), y, tolerance))

#' @export
st_snap.sfc = function(x, y, tolerance) {
	if (isTRUE(st_is_longlat(x)))
		stop("st_snap for longitude/latitude data not supported; use st_transform first?")
	else if (inherits(tolerance, "units") && !is.na(st_crs(x)))
		units(tolerance) = crs_parameters(st_crs(x))$ud_unit # coordinate units
	tolerance = rep(tolerance, length.out = length(x))
	st_sfc(CPL_geos_snap(st_geometry(x), st_geometry(y), as.double(tolerance)))
}

#' @export
st_snap.sf = function(x, y, tolerance)
	st_set_geometry(x, st_snap(st_geometry(x), st_geometry(y), tolerance))

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
