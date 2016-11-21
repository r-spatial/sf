# unary, interfaced through GEOS:

#' Geometric operations on (pairs of) simple feature geometries
#' 
#' Geometric operations on (pairs of) simple feature geometries
#' @name geos
#' @export
#' @return matrix (sparse or dense); if dense: of type \code{character} for \code{relate}, \code{numeric} for \code{distance}, and \code{logical} for all others; matrix has dimension \code{x} by \code{y}; if sparse (only possible for those who return logical in case of dense): return list of length length(x) with indices of the TRUE values for matching \code{y}.
st_is_valid = function(x) CPL_geos_is_valid(st_geometry(x))

#' @name geos
#' @export
#' @return st_area returns the area of a geometry, in the coordinate reference system used
st_area = function(x) { 
	if (isTRUE(st_is_longlat(x)))
		warning("st_area does not give a meaningful area measure for longitude/latitude data.")
	CPL_area(st_geometry(x))
}

#' @name geos
#' @export
#' @return st_length returns the length of a geometry, in the coordinate reference system used
st_length = function(x) { 
	if (isTRUE(st_is_longlat(x)))
		warning("st_length does not give a meaningful length measure for longitude/latitude data.")
	x = CPL_length(st_geometry(x))
	x[is.nan(x)] = NA
	x
}

#' @name geos
#' @export
#' @return st_is_simple and st_is_valid return a logical vector
st_is_simple = function(x) CPL_is_simple(st_geometry(x))

# binary, interfaced through GEOS:

# returning matrix, distance or relation string -- the work horse is:

st_geos_binop = function(op = "intersects", x, y, par = 0.0, sparse = TRUE) {
	if (missing(y))
		y = x
	ret = CPL_geos_binop(st_geometry(x), st_geometry(y), op, par, sparse)
	if (sparse)
		ret
	else
		ret[[1]]
}

#' @param x first simple feature (sf) or simple feature geometry (sfc) collection
#' @param y second simple feature (sf) or simple feature geometry (sfc) collection
#' @name geos
#' @return st_distance returns a dense numeric matrix of dimension length(x) by length(y)
#' @export
st_distance = function(x, y = x) CPL_geos_dist(st_geometry(x), st_geometry(y))

#' @name geos
#' @return st_relate returns a dense \code{character} matrix
#' @export
st_relate           = function(x, y) st_geos_binop("relate", x, y, sparse = FALSE)

#' @name geos
#' @param sparse logical; should a sparse matrix be returned (TRUE) or a dense matrix?
#' @return st_intersects ...  st_equals_exact return a sparse or dense logical matrix with rows and columns corresponding to the number of geometries (or rows) in x and y, respectively
#' @export
st_intersects       = function(x, y, sparse = TRUE) st_geos_binop("intersects", x, y, sparse = sparse)

#' @name geos
#' @export
st_disjoint         = function(x, y, sparse = TRUE) st_geos_binop("disjoint", x, y, sparse = sparse)

#' @name geos
#' @export
st_touches          = function(x, y, sparse = TRUE) st_geos_binop("touches", x, y, sparse = sparse)

#' @name geos
#' @export
st_crosses          = function(x, y, sparse = TRUE) st_geos_binop("crosses", x, y, sparse = sparse)

#' @name geos
#' @export
st_within           = function(x, y, sparse = TRUE) st_geos_binop("within", x, y, sparse = sparse)

#' @name geos
#' @export
st_contains         = function(x, y, sparse = TRUE) st_geos_binop("contains", x, y, sparse = sparse)

#' @name geos
#' @export
st_overlaps         = function(x, y, sparse = TRUE) st_geos_binop("overlaps", x, y, sparse = sparse)

#' @name geos
#' @export
st_equals           = function(x, y, sparse = TRUE) st_geos_binop("equals", x, y, sparse = sparse)

#' @name geos
#' @export
st_covers           = function(x, y, sparse = TRUE) st_geos_binop("covers", x, y, sparse = sparse)

#' @name geos
#' @export
st_covered_by       = function(x, y, sparse = TRUE) st_geos_binop("covered_by", x, y, sparse = sparse)

#' @name geos
#' @export
#' @param par numeric; parameter used for "equals_exact" (margin) and "is_within_distance"
st_equals_exact     = function(x, y, par, sparse = TRUE) 
	st_geos_binop("equals_exact", x, y, par = par, sparse = sparse)

##' @name geos
##' @export
#st_is_within_distance = function(x, y, par, sparse = TRUE) 
#	st_geos_binop("is_within_distance", x, y, par = par, sparse = sparse)

# unary, returning geometries -- GEOS interfaced through GDAL:

#' @name geos
#' @export
#' @param dist buffer distance
#' @param nQuadSegs integer; number of segments per quadrant (fourth of a circle)
#' @return st_buffer ... st_segmentize return an \link{sfc} object with the same number of geometries as in \code{x}
st_buffer = function(x, dist, nQuadSegs = 30)
	st_sfc(CPL_geom_op("buffer", st_geometry(x), dist, nQuadSegs))

#' @name geos
#' @export
st_boundary = function(x) st_sfc(CPL_geom_op("boundary", st_geometry(x)))

#' @name geos
#' @export
#' @examples 
#' nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267)
#' plot(st_union_cascaded(st_sfc(do.call(c, st_geometry(nc)))),col=0)
st_union_cascaded = function(x) st_sfc(CPL_geom_op("union_cascaded", st_geometry(x)))

#' @name geos
#' @export
#' @examples
#' plot(st_convex_hull(nc))
#' plot(nc, border = grey(.5))
st_convex_hull = function(x) st_sfc(CPL_geom_op("convex_hull", st_geometry(x)))

#' @name geos
#' @export
#' @param preserveTopology logical; carry out topology preserving simplification?
#' @param dTolerance numeric; tolerance parameter
st_simplify = function(x, preserveTopology = FALSE, dTolerance = 0.0)
	st_sfc(CPL_geom_op("simplify", st_geometry(x), preserveTopology = preserveTopology, dTolerance = dTolerance))

#' @name geos
#' @export
#' @param bOnlyEdges logical; if TRUE, return lines, else return polygons
#' @details requires GEOS version 3.4 or above
# nocov start
st_triangulate = function(x, dTolerance = 0.0, bOnlyEdges = FALSE) {
	if (CPL_gdal_version() >= "2.1.0")
		st_sfc(CPL_geom_op("triangulate", st_geometry(x), dTolerance = dTolerance, bOnlyEdges = bOnlyEdges))
	else
		stop("for triangulate, GDAL version 2.1.0 is required")
}
# nocov end

#' @name geos
#' @param mlst object of class \code{MULTILINESTRING} or geometry list-column containing these
#' @export
#' @examples 
#' mls = st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0),,2,byrow=TRUE)))
#' x = st_polygonize(mls)
st_polygonize = function(mlst) st_sfc(CPL_geom_op("polygonize", st_geometry(mlst)))

#' @name geos
#' @export
#' @examples
#' plot(nc, axes = TRUE)
#' plot(st_centroid(nc), add = TRUE, pch = 3)
st_centroid = function(x) st_sfc(CPL_geom_op("centroid", st_geometry(x)))

#' @name geos
#' @export
#' @param dfMaxLength numeric; max length of a line segment
st_segmentize  = function(x, dfMaxLength) {
	st_sfc(CPL_geom_op("segmentize", st_geometry(x), dfMaxLength = dfMaxLength))
}

#' @name geos
#' @export
#' @details \code{st_combine} combines geometries without resolving borders. 
#' st_combine(nc)
st_combine = function(x) {
	x = st_geometry(x)
	st_sfc(do.call(c, x), crs = st_crs(x)) # flatten/merge
}

geom_op2 = function(op, x, y) {
	st_sfc(CPL_geom_op2(op, x, y), crs = st_crs(x))
}

#' @name geos
#' @export
#' @param y0 object of class \code{sfc} which is merged, using \code{c.sfg} (\link{st}), before intersection etc. with it is computed 
st_intersection = function(x, y0)   geom_op2("intersection", st_geometry(x), st_combine(y0))

#' @name geos
#' @export
#' @param byid logical; union each geometry (TRUE) or union their combination (FALSE)?
#' @return \code{st_union(x)} unions geometries.  Unioning a set of overlapping polygons has the effect of merging the areas (i.e. the same effect as iteratively unioning all individual polygons together). Unioning a set of LineStrings has the effect of fully noding and dissolving the input linework. In this context "fully noded" means that there will be a node or endpoint in the output for every endpoint or line segment crossing in the input. "Dissolved" means that any duplicate (e.g. coincident) line segments or portions of line segments will be reduced to a single line segment in the output.  Unioning a set of Points has the effect of merging al identical points (producing a set with no duplicates). If \code{y0} in a call to \code{st_union} is not missing, each of the geometries in \code{x} are unioned to the combination of \code{y0}.
#' @examples
#' plot(st_union(nc))
st_union = function(x, y0, byid = FALSE) {
	if (! missing(y0))
		geom_op2("union", st_geometry(x), st_combine(y0))
	else if (byid == FALSE)
		st_sfc(CPL_geos_union(st_combine(x)), crs = st_crs(st_geometry(x)))
	else
		st_sfc(CPL_geos_union(st_geometry(x)), crs = st_crs(st_geometry(x)))
}

#' @name geos
#' @export
st_difference = function(x, y0)     geom_op2("difference", st_geometry(x), st_combine(y0))

#' @name geos
#' @export
st_sym_difference = function(x, y0) geom_op2("sym_difference", st_geometry(x), st_combine(y0))
