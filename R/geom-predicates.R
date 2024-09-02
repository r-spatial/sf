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

is_symmetric = function(operation, pattern) {
	if (!is.na(pattern)) {
		m = matrix(sapply(1:9, function(i) substr(pattern, i, i)), 3, 3)
		isTRUE(all(m == t(m)))
	} else
		isTRUE(operation %in% c("intersects", "touches", "overlaps", "disjoint", "equals"))
}

# binary, interfaced through GEOS or S2:

# [1] X "s2_contains_matrix"    X "s2_covered_by_matrix"
# [3] X "s2_covers_matrix"      X "s2_disjoint_matrix"
# [5] X "s2_distance_matrix"    X "s2_dwithin_matrix"
# [7] X "s2_equals_matrix"      X "s2_intersects_matrix"
# [9] "s2_max_distance_matrix"    "s2_may_intersect_matrix"
#[11] X "s2_touches_matrix"     X "s2_within_matrix"

# returning matrix, distance or relation string -- the work horse is:
st_geos_binop = function(op, x, y, par = 0.0, pattern = NA_character_,
		sparse = TRUE, prepared = FALSE, model = "closed", ..., 
		remove_self = FALSE, retain_unique = FALSE) {
	if (missing(y))
		y = x
	else if (inherits(x, c("sf", "sfc")) && inherits(y, c("sf", "sfc")))
		stopifnot(st_crs(x) == st_crs(y))
	longlat = inherits(x, "s2geography") || isTRUE(st_is_longlat(x))
	if (longlat && sf_use_s2() && op %in% c("intersects", "contains", "within",
			"covers", "covered_by", "disjoint", "equals", "touches")) {
		fn = get(paste0("s2_", op, "_matrix"), envir = getNamespace("s2")) # get op function
		lst = fn(x, y, s2::s2_options(model = model, ...)) # call function
		id = if (is.null(row.names(x)))
				as.character(seq_along(lst))
			else
				row.names(x)
		sgbp(lst, predicate = op, region.id = id, ncol = length(st_geometry(y)), sparse,
			remove_self = remove_self, retain_unique = retain_unique)
	} else {
		if (longlat && !(op %in% c("equals", "equals_exact")))
			message_longlat(paste0("st_", op))
		if (prepared && is_symmetric(op, pattern) &&
				length(dx <- st_dimension(x)) && length(dy <- st_dimension(y)) &&
				isTRUE(all(dx == 0)) && isTRUE(all(dy == 2))) {
			t(st_geos_binop(op, y, x, par = par, pattern = pattern, sparse = sparse, 
				prepared = prepared, remove_self = remove_self, retain_unique = retain_unique,
				...))
		} else {
			ret = CPL_geos_binop(st_geometry(x), st_geometry(y), op, par, pattern, prepared)
			if (length(ret) == 0 || is.null(dim(ret[[1]]))) {
				id = if (is.null(row.names(x)))
						as.character(seq_along(ret))
					else
						row.names(x)
				sgbp(ret, predicate = op, region.id = id, ncol = length(st_geometry(y)), sparse,
					remove_self = remove_self, retain_unique = retain_unique)
			} else # CPL_geos_binop returned a matrix, e.g. from op = "relate"
				ret[[1]]
		}
	}
}

#' Compute DE9-IM relation between pairs of geometries, or match it to a given pattern
#'
#' Compute DE9-IM relation between pairs of geometries, or match it to a given pattern
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param pattern character; define the pattern to match to, see details.
#' @param sparse logical; should a sparse matrix be returned (`TRUE`) or a dense matrix?
#' @return In case \code{pattern} is not given, \code{st_relate} returns a dense \code{character} matrix; element `[i,j]` has nine characters, referring to the DE9-IM relationship between `x[i]` and `y[j]`, encoded as IxIy,IxBy,IxEy,BxIy,BxBy,BxEy,ExIy,ExBy,ExEy where I refers to interior, B to boundary, and E to exterior, and e.g. BxIy the dimensionality of the intersection of the the boundary of `x[i]` and the interior of `y[j]`, which is one of: 0, 1, 2, or F; digits denoting dimensionality of intersection, F denoting no intersection. When \code{pattern} is given, a dense logical matrix or sparse index list returned with matches to the given pattern; see \link{st_intersects} for a description of the returned matrix or list. See also \url{https://en.wikipedia.org/wiki/DE-9IM} for further explanation.
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
		stopifnot(is.character(pattern), length(pattern) == 1, nchar(pattern) == 9)
		st_geos_binop("relate_pattern", x, y, pattern = pattern, sparse = sparse)
	} else
		st_geos_binop("relate", x, y, sparse = FALSE)
}

#' Identify if `x` and `y` share any space
#'
#' 
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}; if missing, \code{x} is used
#' @param sparse logical; should a sparse index list be returned (`TRUE`) or a dense logical matrix? See below.
#' @inheritDotParams s2::s2_options
#' @return If \code{sparse=FALSE}, \code{st_predicate} (with \code{predicate} e.g. "intersects") returns a dense logical matrix with element \code{i,j} equal to \code{TRUE} when \code{predicate(x[i], y[j])} (e.g., when geometry of feature i and j intersect); if \code{sparse=TRUE}, an object of class \code{\link{sgbp}} is returned, which is a sparse list representation of the same matrix, with list element \code{i} an integer vector with all indices \code{j} for which \code{predicate(x[i],y[j])} is \code{TRUE} (and hence a zero-length integer vector if none of them is \code{TRUE}). From the dense matrix, one can find out if one or more elements intersect by \code{apply(mat, 1, any)}, and from the sparse list by \code{lengths(lst) > 0}, see examples below.
#' @details For most predicates, a spatial index is built on argument \code{x}; see \url{https://r-spatial.org/r/2017/06/22/spatial-index.html}.
#' Specifically, \code{st_intersects}, \code{st_disjoint}, \code{st_touches} \code{st_crosses}, \code{st_within}, \code{st_contains}, \code{st_contains_properly}, \code{st_overlaps}, \code{st_equals}, \code{st_covers} and \code{st_covered_by} all build spatial indexes for more efficient geometry calculations. \code{st_relate}, \code{st_equals_exact}, and do not; \code{st_is_within_distance} uses a spatial index for geographic coordinates when \code{sf_use_s2()} is true.
#'
#' If \code{y} is missing, `st_<predicate>(x, x)` is effectively called, and a square matrix is returned with diagonal elements `st_predicate(x[i], x[i])`.
#'
#' Sparse geometry binary predicate (\code{\link{sgbp}}) lists have the following attributes: \code{region.id} with the \code{row.names} of \code{x} (if any, else \code{1:n}), \code{ncol} with the number of features in \code{y}, and \code{predicate} with the name of the predicate used.
#'
#' @note For intersection on pairs of simple feature geometries, use
#' the function \code{\link{st_intersection}} instead of \code{st_intersects}.
#' @family geometric binary predicates for two spatial objects
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
#' # retain the records with unique geometries:
#' @export
st_intersects	= function(x, y, sparse = TRUE, ...) UseMethod("st_intersects")

#' @export
st_intersects.sfc = function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared, ...)

#' @export
st_intersects.sf = function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared, ...)

#' @export
st_intersects.sfg = function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared, ...)


#' @name geos_binary_pred
#' @export
st_disjoint		= function(x, y = x, sparse = TRUE, prepared = TRUE) {
	# st_geos_binop("disjoint", x, y, sparse = sparse, prepared = prepared) -> didn't use STRtree
	int = st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared)
	# disjoint = !intersects :
	if (sparse)
		sgbp(lapply(int, function(g) setdiff(seq_along(st_geometry(y)), g)),
			predicate = "disjoint",
			ncol = attr(int, "ncol"),
			region.id = attr(int, "region.id"))
	else
		!int
}

#' @name geos_binary_pred
#' @inheritParams st_intersects
#' @inheritDotParams s2::s2_options
#' @details If \code{prepared} is \code{TRUE}, and \code{x} contains POINT geometries and \code{y} contains polygons, then the polygon geometries are prepared, rather than the points.

#' @param prepared logical; prepare geometry for `x`, before looping over `y`? See Details.
#' @export
st_touches		= function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("touches", x, y, sparse = sparse, prepared = prepared, ...)

#' @name geos_binary_pred
#' @export
st_crosses		= function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("crosses", x, y, sparse = sparse, prepared = prepared, ...)

#' @name geos_binary_pred
#' @export
st_within	= function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("within", x, y, sparse = sparse, prepared = prepared, ...)

#' Identify if y is within x
#' 
#' * `st_contains()` is true if 
#' * `st_contains_properly(x, y)` is true if `x` intersects `y`'s interior, but not its edges or exterior; `x` contains `x`, but `x` does not properly contain `x`.
#' @param model character; polygon/polyline model; one of
#' "open", "semi-open" or "closed"; see Details.
#' @details for \code{model}, see https://github.com/r-spatial/s2/issues/32
#' @inheritParams geos_binary_pred
#' @param ... passed on to [s2::s2_contains()]
#' @export
#' @family geometric binary predicates for two spatial objects
st_contains	= function(x, y, sparse = TRUE, prepared = TRUE, ..., model = "open")
	st_geos_binop("contains", x, y, sparse = sparse, prepared = prepared, ..., model = model)

#' @rdname st_contains
#' @export
#' @details 
#' See also \link{st_relate} and \url{https://en.wikipedia.org/wiki/DE-9IM} for a more detailed description of the underlying algorithms.
st_contains_properly = function(x, y, sparse = TRUE, prepared = TRUE, ...) {
	if (! prepared)
		stop("non-prepared geometries not supported for st_contains_properly")
	st_geos_binop("contains_properly", x, y, sparse = sparse, prepared = TRUE, ...)
}

#' @name geos_binary_pred
#' @export
st_overlaps		= function(x, y, sparse = TRUE, prepared = TRUE, ...)
	st_geos_binop("overlaps", x, y, sparse = sparse, prepared = prepared, ...)


#' Verify if geographies are equal
#' 
#' * `st_equals()` validate if x and y are equal.
#' * `st_equals_exact()` returns true for two geometries of the same type and their vertices corresponding by index are equal up to a specified tolerance.
#' 
#' @inheritParams geos_binary_pred
#' @param retain_unique logical; if `TRUE` (and `y` is missing) return only
#'   indexes of points larger than the current index; this can be used to select
#'   unique geometries, see examples. This argument can be used for all geometry predicates;
#'   see also \link{distinct.sf} to find records where geometries AND attributes are distinct.
#' @param remove_self logical; if `TRUE` (and `y` is missing) return only indexes of geometries different from the current index; this can be used to omit self-intersections; see examples.
#' This argument can be used for all geometry predicates
#' @param ... passed on to [s2::s2_options()]
#' @export
#' @family geometric binary predicates for two spatial objects
#' @examples
#' # remove duplicate geometries:
#' p1 = st_point(0:1)
#' p2 = st_point(2:1)
#' p = st_sf(a = letters[1:8], geom = st_sfc(p1, p1, p2, p1, p1, p2, p2, p1))
#' st_equals(p)
#' st_equals(p, remove_self = TRUE)
#' (u = st_equals(p, retain_unique = TRUE))
#' # retain the records with unique geometries:
#' p[-unlist(u),]
st_equals		= function(x, y, sparse = TRUE, prepared = FALSE, ..., 
							retain_unique = FALSE, remove_self = FALSE) {
	if (prepared)
		stop("prepared geometries not supported for st_equals")
	st_geos_binop("equals", x, y, sparse = sparse, ..., 
				  retain_unique = retain_unique, remove_self = remove_self)
}

#' @name geos_binary_pred
#' @param model character; polygon/polyline model; one of
#' `"open"`, `"semi-open"` or `"closed"`; see Details.
#' @details for \code{model}, see https://github.com/r-spatial/s2/issues/32
#' @export
st_covers = function(x, y, sparse = TRUE, prepared = TRUE, ..., model = "closed")
	st_geos_binop("covers", x, y, sparse = sparse, prepared = prepared, ..., model = model)


#' @name geos_binary_pred
#' @export
st_covered_by = function(x, y = x, sparse = TRUE, prepared = TRUE, ..., model = "closed")
	st_geos_binop("covered_by", x, y, sparse = sparse, prepared = prepared, ...)


#' @rdname st_equals
#' @export
#' @param par numeric; parameter used for "equals_exact" (margin);
st_equals_exact = function(x, y, par, sparse = TRUE, prepared = FALSE, ...) {
	if (prepared)
		stop("prepared geometries not supported for st_equals_exact")
	st_geos_binop("equals_exact", x, y, par = par, sparse = sparse, ...)
}

#' @name geos_binary_pred
#' @export
#' @param dist distance threshold; geometry indexes with distances smaller or equal to this value are returned; numeric value or units value having distance units.
st_is_within_distance = function(x, y = x, dist, sparse = TRUE, ...) {

	ret = if (isTRUE(st_is_longlat(x))) {
			units(dist) = as_units("m") # might convert
			r = if (sf_use_s2()) {
					if (inherits(dist, "units"))
						dist = drop_units(dist)
					s2::s2_dwithin_matrix(x, y, dist, ...)
				} else {
					if (!requireNamespace("lwgeom", quietly = TRUE) || 
							utils::packageVersion("lwgeom") <= "0.1-2")
						stop("lwgeom > 0.1-2 required: install first?")
					lwgeom::st_geod_distance(x, y, tolerance = dist, sparse = TRUE)
				}
			sgbp(r, predicate = "is_within_distance", region.id = seq_along(x), 
				ncol = length(st_geometry(y)))
		} else {
			if (!is.null(st_crs(x)$ud_unit))
				units(dist) = st_crs(x)$ud_unit # might convert
			st_geos_binop("is_within_distance", x, y, par = dist, sparse = sparse, ...)
		}
	if (!sparse)
		as.matrix(ret)
	else
		ret
}
