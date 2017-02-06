# unary, interfaced through GEOS:

#' Geometric operations on (pairs of) simple feature geometry sets
#' 
#' Geometric operations on (pairs of) simple feature geometry sets
#' @name geos
#' @export
#' @return matrix (sparse or dense); if dense: of type \code{character} for \code{relate}, \code{numeric} for \code{distance}, and \code{logical} for all others; matrix has dimension \code{x} by \code{y}; if sparse (only possible for those who return logical in case of dense): return list of length length(x) with indices of the TRUE values for matching \code{y}.
st_is_valid = function(x) CPL_geos_is_valid(st_geometry(x))

#' @name geos
#' @param NA_if_empty logical; if TRUE, return NA for empty geometries
#' @return st_dimension returns 0 for points, 1 for lines, 2 for surfaces and by default NA for empty geometries.
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
st_dimension = function(x, NA_if_empty = TRUE) CPL_gdal_dimension(st_geometry(x), NA_if_empty)

#' @name geos
#' @export
#' @return st_area returns the area of a geometry, in the coordinate reference system used; in case \code{x} is in degrees longitude/latitude, \link[geosphere]{areaPolygon} is used for area calculation.
st_area = function(x) { 
	if (isTRUE(st_is_longlat(x))) {
		p = crs_parameters(st_crs(x))
		if (!requireNamespace("sp", quietly = TRUE))
			stop("package sp required, please install it first")
		if (!requireNamespace("geosphere", quietly = TRUE))
			stop("package geosphere required, please install it first")
		a = geosphere::areaPolygon(as(st_geometry(x), "Spatial"), 
				as.numeric(p$SemiMajor), 1./p$InvFlattening)
		u = 1
		units(u) = units(p$SemiMajor)
		a * u^2
	} else {
		a = CPL_area(st_geometry(x))
		if (!is.na(st_crs(x)))
			a * crs_parameters(st_crs(x))$ud_unit^2
		else
			a
	}
}

ll_length = function(x, fn, p) {
	if (is.list(x)) # sfc_MULTILINESTRING
		sum(sapply(x, ll_length, fn = fn, p = p))
	else {
		pts = unclass(x) # matrix
		sum(fn(head(pts, -1), tail(pts, -1), as.numeric(p$SemiMajor), 1./p$InvFlattening))
	}
}

#' @name geos
#' @param dist_fun function to be used for great circle distances; for unprojected (long/lat) data, this should be a distance function of package geosphere, or compatible to that; it defaults to \link[geosphere]{distGeo} in that case; for other data metric lengths are computed.
#' @export
#' @return st_length returns the length of a LINESTRING or MULTILINESTRING geometry, using the coordinate reference system used; if the coordinate reference system of \code{x} was set, the returned value has a unit of measurement.
#' @examples
#' dist_vincenty = function(p1, p2, a, f) geosphere::distVincentyEllipsoid(p1, p2, a, a * (1-f), f)
#' line = st_sfc(st_linestring(rbind(c(30,30), c(40,40))), crs = 4326)
#' st_length(line)
#' st_length(line, dist_fun = dist_vincenty)
st_length = function(x, dist_fun = geosphere::distGeo) {
	x = st_geometry(x)
	stopifnot(inherits(x, "sfc_LINESTRING") || inherits(x, "sfc_MULTILINESTRING"))
	if (isTRUE(st_is_longlat(x))) {
		p = crs_parameters(st_crs(x))
		if (missing(dist_fun)) {
			if (!requireNamespace("geosphere", quietly = TRUE))
				stop("package geosphere required, please install it first")
			dist_fun = geosphere::distGeo
		}
		ret = sapply(x, ll_length, fn = dist_fun, p = p)
		units(ret) = units(p$SemiMajor)
		ret
	} else {
		ret = CPL_length(x)
		ret[is.nan(ret)] = NA
		if (!is.na(st_crs(x)))
			ret * crs_parameters(st_crs(x))$ud_unit
		else
			ret
	}
}

#' @name geos
#' @export
#' @return st_is_simple and st_is_valid return a logical vector
st_is_simple = function(x) CPL_geos_is_simple(st_geometry(x))

# binary, interfaced through GEOS:

# returning matrix, distance or relation string -- the work horse is:

st_geos_binop = function(op = "intersects", x, y, par = 0.0, sparse = TRUE, prepared = FALSE) {
	if (missing(y))
		y = x
	else 
		stopifnot(st_crs(x) == st_crs(y))
	if (isTRUE(st_is_longlat(x)) && !(op %in% c("equals", "equals_exact", "polygonize"))) 
		message("although coordinates are longitude/latitude, it is assumed that they are planar")
	ret = CPL_geos_binop(st_geometry(x), st_geometry(y), op, par, sparse, prepared)
	if (sparse)
		ret
	else
		ret[[1]]
}

#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @param y object of class \code{sf}, \code{sfc} or \code{sfg}
#' @name geos
#' @return st_distance returns a dense numeric matrix of dimension length(x) by length(y)
#' @details function \code{dist_fun} should follow the pattern of the distance function \link[geosphere]{distGeo}: the first two arguments must be 2-column point matrices, the third the semi major axis (radius, in m), the third the ellipsoid flattening. 
#' @export
st_distance = function(x, y, dist_fun) {
	if (missing(y))
		y = x
	else
		stopifnot(st_crs(x) == st_crs(y))
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
			d * p$ud_unit
		else
			d
	}
}

#' @name geos
#' @return st_relate returns a dense \code{character} matrix; element [i,j] has nine characters, refering to the DE9-IM relationship between x[i] and y[j], encoded as IxIy,IxBy,IxEy,BxIy,BxBy,BxEy,ExIy,ExBy,ExEy where I refers to interior, B to boundary, and E to exterior, and e.g. BxIy the dimensionality of the intersection of the the boundary of x[i] and the interior of y[j], which is one of {0,1,2,F}, digits denoting dimensionality, F denoting not intersecting.
#' @export
#' @examples
#' p1 = st_point(c(0,0))
#' p2 = st_point(c(2,2))
#' pol1 = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0)))) - 0.5
#' pol2 = pol1 + 1
#' pol3 = pol1 + 2
#' st_relate(st_sfc(p1, p2), st_sfc(pol1, pol2, pol3))
st_relate	= function(x, y) st_geos_binop("relate", x, y, sparse = FALSE)

#' @name geos
#' @param sparse logical; should a sparse matrix be returned (TRUE) or a dense matrix?
#' @return st_intersects ...	st_equals_exact return a sparse or dense logical matrix with rows and columns corresponding to the number of geometries (or rows) in x and y, respectively
#' @export
st_intersects	= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("intersects", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
st_disjoint		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("disjoint", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
st_touches		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("touches", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
st_crosses		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("crosses", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
st_within		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("within", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
#' @param prepared logical; prepare geometry for x, before looping over y?
st_contains		= function(x, y, sparse = TRUE, prepared = TRUE) 
	st_geos_binop("contains", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
#' @details `st_contains_properly(A,B)` is true if A intersects B's interior, but not its edges or exterior; A contains A, but A does not properly contain A. 
st_contains_properly = function(x, y, sparse = TRUE, prepared = TRUE) {
	if (! prepared)
		stop("non-prepared geometries not supported for st_contains_properly")
	st_geos_binop("contains_properly", x, y, sparse = sparse, prepared = TRUE)
}

#' @name geos
#' @export
st_overlaps		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("overlaps", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
st_equals		= function(x, y, sparse = TRUE, prepared = FALSE) {
	if (prepared)
		stop("prepared geometries not supported for st_equals")
	st_geos_binop("equals", x, y, sparse = sparse)
}

#' @name geos
#' @export
st_covers		= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("covers", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
st_covered_by	= function(x, y, sparse = TRUE, prepared = TRUE)
	st_geos_binop("covered_by", x, y, sparse = sparse, prepared = prepared)

#' @name geos
#' @export
#' @param par numeric; parameter used for "equals_exact" (margin) and "is_within_distance"
st_equals_exact = function(x, y, par, sparse = TRUE, prepared = FALSE) {
	if (prepared)
		stop("prepared geometries not supported for st_equals")
	st_geos_binop("equals_exact", x, y, par = par, sparse = sparse)
}

##' @name geos
##' @export
#st_is_within_distance = function(x, y, par, sparse = TRUE) 
#	st_geos_binop("is_within_distance", x, y, par = par, sparse = sparse)

# unary, returning geometries -- GEOS interfaced through GDAL:

#' @name geos
#' @export
#' @param dist numeric; buffer distance for all, or for each of the elements in \code{x}
#' @param nQuadSegs integer; number of segments per quadrant (fourth of a circle)
#' @return st_buffer ... st_segmentize return an \link{sfc} or an \link{sf} object with the same number of geometries as in \code{x}
st_buffer = function(x, dist, nQuadSegs = 30)
	UseMethod("st_buffer")

#' @export
st_buffer.sfg = function(x, dist, nQuadSegs = 30)
	get_first_sfg(st_buffer(st_sfc(x), dist, nQuadSegs = nQuadSegs))

#' @export
st_buffer.sfc = function(x, dist, nQuadSegs = 30) {
	if (isTRUE(st_is_longlat(x)))
		warning("st_buffer does not correctly buffer longitude/latitude data, dist needs to be in decimal degrees.")
	dist = rep(dist, length.out = length(x))
	st_sfc(CPL_geos_op("buffer", x, dist, nQuadSegs))
}

#' @export
st_buffer.sf <- function(x, dist, nQuadSegs = 30) {
	st_geometry(x) <- st_buffer(st_geometry(x), dist, nQuadSegs)
	x
}

#' @name geos
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

#' @name geos
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

#' @name geos
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

#' @name geos
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

#' @name geos
#' @export
#' @param envelope object of class \code{sfc} or \code{sfg} with the envelope for a voronoi diagram
#' @details \code{st_voronoi} requires GEOS version 3.4 or above
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
st_voronoi.sfg = function(x, envelope = list(), dTolerance = 0.0, bOnlyEdges = FALSE)
	get_first_sfg(st_voronoi(st_sfc(x), st_sfc(envelope), dTolerance, bOnlyEdges = bOnlyEdges))

#' @export
st_voronoi.sfc = function(x, envelope = list(), dTolerance = 0.0, bOnlyEdges = FALSE) {
	if (sf_extSoftVersion()["GEOS"] >= "3.5.0") {
		if (isTRUE(st_is_longlat(x)))
			warning("st_voronoi does not correctly triangulate longitude/latitude data")
		st_sfc(CPL_geos_voronoi(x, st_sfc(envelope), dTolerance = dTolerance, bOnlyEdges = bOnlyEdges))
	} else
		stop("for voronoi, GEOS version 3.5.0 or higher is required")
}

#' @export
st_voronoi.sf = function(x, envelope = list(), dTolerance = 0.0, bOnlyEdges = FALSE) {
	st_geometry(x) <- st_voronoi(st_geometry(x), st_sfc(envelope), dTolerance, bOnlyEdges)
	x
}

#' @name geos
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

#' @name geos
#' @export
#' @details in case of \code{st_linemerge}, \code{x} must be an object of class \code{MULTILINESTRING}, or an \code{sfc} geometry list-column object containing these
#' @examples 
#' mls = st_multilinestring(list(rbind(c(0,0), c(1,1)), rbind(c(2,0), c(1,1))))
#' st_linemerge(st_sfc(mls))
st_linemerge = function(x)
	UseMethod("st_linemerge")

#' @export
st_linemerge.sfg = function(x)
	get_first_sfg(st_linemerge(st_sfc(x)))

#' @export
st_linemerge.sfc = function(x) {
	stopifnot(inherits(x, "sfc_MULTILINESTRING"))
	st_sfc(CPL_geos_op("linemerge", x, numeric(0)))
}

#' @export
st_linemerge.sf = function(x) {
	st_geometry(x) <- st_linemerge(st_geometry(x))
	x
}

#' @name geos
#' @export
#' @examples
#' plot(nc, axes = TRUE)
#' plot(st_centroid(nc), add = TRUE, pch = 3)
st_centroid = function(x)
	UseMethod("st_centroid")

#' @export
st_centroid.sfg = function(x)
	get_first_sfg(st_centroid(st_sfc(x)))

#' @export
st_centroid.sfc = function(x) { 
	if (isTRUE(st_is_longlat(x)))
		warning("st_centroid does not give correct centroids for longitude/latitude data")
	st_sfc(CPL_geos_op("centroid", x, numeric(0)))
}

#' @export
st_centroid.sf = function(x) {
	st_geometry(x) <- st_centroid(st_geometry(x))
	x
}

#' @name geos
#' @export
#' @param dfMaxLength numeric; max length of a line segment
#' @param ... ignored
#' @param warn logical; generate a warning in case of long/lat data
st_segmentize	= function(x, dfMaxLength, ..., warn = TRUE)
	UseMethod("st_segmentize")

#' @export 
st_segmentize.sfg = function(x, dfMaxLength, ..., warn = TRUE)
	get_first_sfg(st_segmentize(st_sfc(x), dfMaxLength, ..., warn = warn))

#' @export 
st_segmentize.sfc	= function(x, dfMaxLength, ..., warn = TRUE) {
	if (warn && isTRUE(st_is_longlat(x)))
		warning("st_segmentize does not correctly segmentize longitude/latitude data")
	st_sfc(CPL_gdal_segmentize(x, dfMaxLength), crs = st_crs(x))
}

#' @export
st_segmentize.sf = function(x, dfMaxLength, ..., warn = TRUE) {
	st_geometry(x) <- st_segmentize(st_geometry(x), dfMaxLength, ..., warn = warn)
	x
}

#' @name geos
#' @export
#' @details \code{st_combine} combines geometries without resolving borders. 
#' @examples
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
	st_sf(df, geoms)
}

# after checking identical crs,
# call geos_op2 function op on x and y:
geos_op2_geom = function(op, x, y) {
	stopifnot(st_crs(x) == st_crs(y))
	st_sfc(CPL_geos_op2(op, st_geometry(x), st_geometry(y)), crs = st_crs(x))
}

# return first sfg, or empty geometry in case of zero features
get_first_sfg = function(x) {
	if (length(x) == 0)
		st_geometrycollection()
	else
		x[[1]]
}

#' @name geos
#' @export
#' @return All functions (or methods) returning a geometry return an object of the same class as that of the first argument (\code{x}).  \code{st_intersection}, \code{st_union}, \code{st_difference} and \code{st_sym_difference} return the non-empty geometries resulting from applying the operation to all geometry pairs in \code{x} and \code{y}, and return an object of class \code{sfg}, \code{sfc} or \code{sf}, where in the latter case the matching attributes of the original object(s) are added. The \code{sfc} geometry list-column returned carries an attribute \code{idx}, which is an \code{n x 2} matrix with every row the index of the corresponding entries of \code{x} and \code{y}, respectively. \code{st_union} has in addition the ability to work on a single argument \code{x} (\code{y} missing): in this case, if \code{by_feature} is \code{FALSE} all geometries are unioned together and an \code{sfg} or single-geometry \code{sfc} object is returned, if \code{by_feature} is \code{TRUE} each feature geometry is unioned; this can for instance be used to resolve internal boundaries after polygons were combined using \code{st_combine}.
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

#' @name geos
#' @export
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

#' @name geos
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

#' @name geos
#' @export
#' @param by_feature logical; if TRUE, union each feature, if FALSE return a single feature with the union the set of features
#' @return \code{st_union(x)} unions geometries. Unioning a set of overlapping polygons has the effect of merging the areas (i.e. the same effect as iteratively unioning all individual polygons together). Unioning a set of LineStrings has the effect of fully noding and dissolving the input linework. In this context "fully noded" means that there will be a node or endpoint in the output for every endpoint or line segment crossing in the input. "Dissolved" means that any duplicate (e.g. coincident) line segments or portions of line segments will be reduced to a single line segment in the output.	Unioning a set of Points has the effect of merging al identical points (producing a set with no duplicates).
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
		if (by_feature) {
			st_geometry(x) = geom
			x
		} else
			geom
	} else
		geos_op2_df(x, y, geos_op2_geom("union", x, y))
}

#' @name geos
#' @param n integer; number of points to choose per geometry; if missing, n will be computed as \code{round(density * st_length(geom))}.
#' @param density numeric; density (points per distance unit) of the sampling, possibly a vector of length equal to the number of features (otherwise recycled).
#' @param type character; indicate the sampling type, either "regular" or "random"
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
st_line_sample = function(x, n, density, type = "regular") {
	if (isTRUE(st_is_longlat(x)))
		stop("st_line_sample for longitude/latitude not supported")
	l = st_length(x)
	if (missing(n))
		n = round(rep(density, length.out = length(l)) * l)
	else
		n = rep(n, length.out = length(l))
	regular = function(n) { (1:n - 0.5)/n }
	random = function(n) { sort(runif(n)) }
	fn = switch(type,
		regular = regular,
		random = random,
		stop("unknown type"))
	distList = lapply(seq_along(n), function(i) fn(n[i]) * l[i])
	st_sfc(CPL_gdal_linestring_sample(st_geometry(x), distList))
}

#' Make a rectangular grid of polygons over the bounding box of a sf or sfc object
#' 
#' Make a rectangular grid of polygons over the bounding box of a sf or sfc object
#' @param x object of class \link{sf} or \link{sfc}
#' @param cellsize target cellsize
#' @param offset numeric of lengt 2; lower left corner coordinates (x, y) of the grid
#' @param n integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @export
st_makegrid = function(x, cellsize = c(diff(st_bbox(x)[c(1,3)]), diff(st_bbox(x)[c(2,4)]))/n, 
		offset = st_bbox(x)[1:2], n = c(10, 10)) {

	bb = if (!missing(n) && !missing(offset) && !missing(cellsize)) {
		cellsize = rep(cellsize, length.out = 2)
		n = rep(n, length.out = 2)
		structure(c(offset, offset + n * cellsize), names = c("xmin", "ymin", "xmax", "ymax"))
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
	
	ret = vector("list", nx * ny)
	square = function(x1, y1, x2, y2)	{
		st_polygon(list(rbind(c(x1, y1), c(x2, y1), c(x2, y2), c(x1, y2), c(x1, y1))))
	}
	
	for (i in 1:nx)
		for (j in 1:ny)
			ret[[(j - 1) * nx + i]] = square(xc[i], yc[j], xc[i+1], yc[j+1])
	
	if (missing(x))
		st_sfc(ret)
	else
		st_sfc(ret, crs = st_crs(x))
}
