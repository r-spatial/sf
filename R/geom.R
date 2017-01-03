# unary, interfaced through GEOS:

#' Geometric operations on (pairs of) simple feature geometries
#' 
#' Geometric operations on (pairs of) simple feature geometries
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
		p = crs_pars(st_crs(x))
		if (!requireNamespace("sp", quietly = TRUE))
			stop("package sp required, please install it first")
		if (!requireNamespace("geosphere", quietly = TRUE))
			stop("package sp required, please install it first")
		a = geosphere::areaPolygon(as(st_geometry(x), "Spatial"), 
			as.numeric(p$SemiMajor), 1./p$InvFlattening)
		u = 1
		units(u) = units(p$SemiMajor)
		a * u^2
	} else {
		a = CPL_area(st_geometry(x))
		if (!is.na(st_crs(x)))
			a * crs_pars(st_crs(x))$ud_unit^2
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
st_length = function(x, dist_fun = geosphere::distGeo) {
	x = st_geometry(x)
	stopifnot(inherits(x, "sfc_LINESTRING") || inherits(x, "sfc_MULTILINESTRING"))
	if (isTRUE(st_is_longlat(x))) {
		p = crs_pars(st_crs(x))
		if (missing(dist_fun)) {
			if (!requireNamespace("geosphere", quietly = TRUE))
				stop("package sp required, please install it first")
			dist_fun = geosphere::distGeo
		}
		ret = sapply(x, ll_length, fn = dist_fun, p = p)
		units(ret) = units(p$SemiMajor)
		ret
	} else {
		ret = CPL_length(x)
		ret[is.nan(ret)] = NA
		if (!is.na(st_crs(x)))
			ret * crs_pars(st_crs(x))$ud_unit
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

st_geos_binop = function(op = "intersects", x, y, par = 0.0, sparse = TRUE) {
	if (missing(y))
		y = x
	else 
		stopifnot(st_crs(x) == st_crs(y))
	if (isTRUE(st_is_longlat(x)) && !(op %in% c("equals", "equals_exact", "polygonize"))) 
		message("although coordinates are longitude/latitude, it is assumed that they are planar")
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
#' @details function \code{dist_fun} should follow the pattern of the distance functions in package geosphere: the first two arguments should be 2-column point matrices, the third the semi major axis (radius, in m), the third the ellipsoid flattening. 
#' @export
st_distance = function(x, y, dist_fun) {
	if (missing(y))
		y = x
	else 
		stopifnot(st_crs(x) == st_crs(y))
	x = st_geometry(x)
	y = st_geometry(y)
	if (isTRUE(st_is_longlat(x))) {
		if (!inherits(x, "sfc_POINT") || !inherits(y, "sfc_POINT"))
			stop("st_distance for longitude/latitude data only available for POINT geometries.")
		if (!requireNamespace("geosphere", quietly = TRUE))
			stop("package sp required, please install it first")
		if (missing(dist_fun))
			dist_fun = geosphere::distGeo
		p = crs_pars(st_crs(x))
		xp = do.call(rbind, x)[rep(seq_along(x), length(y)),]
		yp = do.call(rbind, y)[rep(seq_along(y), each = length(x)),]
		m = matrix(
			dist_fun(xp, yp, as.numeric(p$SemiMajor), 1./p$InvFlattening), 
			length(x), length(y))
		units(m) = units(p$SemiMajor)
		m
	} else {
		d = CPL_geos_dist(st_geometry(x), st_geometry(y))
		if (!is.na(st_crs(x)))
			d * crs_pars(st_crs(x))$ud_unit
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
#' st_relate(st_sfc(p1,p2), st_sfc(pol1, pol2, pol3))
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
st_buffer = function(x, dist, nQuadSegs = 30) {
	if (isTRUE(st_is_longlat(x)))
		warning("st_buffer does not correctly buffer longitude/latitude data, dist needs to be in decimal degrees.")
	st_sfc(CPL_geos_op("buffer", st_geometry(x), dist, nQuadSegs))
}

#' @name geos
#' @export
st_boundary = function(x) st_sfc(CPL_geos_op("boundary", st_geometry(x)))

#' @name geos
#' @export
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' plot(st_convex_hull(nc))
#' plot(nc, border = grey(.5))
st_convex_hull = function(x) st_sfc(CPL_geos_op("convex_hull", st_geometry(x)))

#' @name geos
#' @export
#' @param preserveTopology logical; carry out topology preserving simplification?
#' @param dTolerance numeric; tolerance parameter
st_simplify = function(x, preserveTopology = FALSE, dTolerance = 0.0) {
	if (isTRUE(st_is_longlat(x)))
		warning("st_simplify does not correctly simplify longitude/latitude data, dTolerance needs to be in decimal degrees.")
	st_sfc(CPL_geos_op("simplify", st_geometry(x), preserveTopology = preserveTopology, dTolerance = dTolerance))
}

#' @name geos
#' @export
#' @param bOnlyEdges logical; if TRUE, return lines, else return polygons
#' @details requires GEOS version 3.4 or above
# nocov start
st_triangulate = function(x, dTolerance = 0.0, bOnlyEdges = FALSE) {
	if (isTRUE(st_is_longlat(x)))
		stop("st_triangulate does not correctly triangulate longitude/latitude data.")
	if (CPL_geos_version() >= "3.4.0")
		st_sfc(CPL_geos_op("triangulate", st_geometry(x), dTolerance = dTolerance, bOnlyEdges = bOnlyEdges))
	else
		stop("for triangulate, GEOS version 3.4.0 or higher is required")
}
# nocov end

#' @name geos
#' @param mlst object of class \code{MULTILINESTRING} or geometry list-column containing these
#' @export
#' @examples 
#' mls = st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0),,2,byrow=TRUE)))
#' st_polygonize(mls)
st_polygonize = function(mlst) {
	x = st_geometry(mlst)
	stopifnot(inherits(x, "sfc_LINESTRING") || inherits(x, "sfc_MULTILINESTRING"))
	st_sfc(CPL_geos_op("polygonize", x))
}

#' @name geos
#' @export
#' @examples 
#' mls = st_multilinestring(list(rbind(c(0,0), c(1,1)), rbind(c(2,0), c(1,1))))
#' st_linemerge(mls)
st_linemerge = function(mlst) {
	x = st_geometry(mlst)
	stopifnot(inherits(x, "sfc_MULTILINESTRING"))
	st_sfc(CPL_geos_op("linemerge", x))
}

#' @name geos
#' @export
#' @examples
#' plot(nc, axes = TRUE)
#' plot(st_centroid(nc), add = TRUE, pch = 3)
st_centroid = function(x) { 
	if (isTRUE(st_is_longlat(x)))
		warning("st_centroid does not give correct centroids for longitude/latitude data.")
	st_sfc(CPL_geos_op("centroid", st_geometry(x)))
}

#' @name geos
#' @export
#' @param dfMaxLength numeric; max length of a line segment
#' @param ... ignored
#' @param warn logical; generate a warning in case of long/lat data
st_segmentize  = function(x, dfMaxLength, ..., warn = TRUE) {
	if (warn && isTRUE(st_is_longlat(x)))
		warning("st_segmentize does not correctly segmentize longitude/latitude data.")
	st_sfc(CPL_gdal_segmentize(st_geometry(x), dfMaxLength), crs = st_crs(x))
}

#' @name geos
#' @export
#' @details \code{st_combine} combines geometries without resolving borders. 
#' st_combine(nc)
st_combine = function(x) {
	x = st_geometry(x)
	st_sfc(do.call(c, x), crs = st_crs(x)) # flatten/merge
}

geos_op2 = function(op, x, y) {
	stopifnot(st_crs(x) == st_crs(y))
	st_sfc(CPL_geos_op2(op, x, y), crs = st_crs(x))
}

#' @name geos
#' @export
st_intersection = function(x, y) {
	ret = geos_op2("intersection", st_geometry(x), st_geometry(y))
	if (length(ret) == 0 || !(inherits(x, "sf") || inherits(y, "sf"))) # no attributes
		ret
	else { # at least one of them is sf:
		idx = attr(ret, "idx")
		attr(ret, "idx") = NULL
		all_fields_x = all_fields_y = TRUE
		df = NULL
		if (inherits(x, "sf")) {
			all_fields_x = all_fields(x)
			df = x[idx[,1],,drop=FALSE]
			st_geometry(df) = NULL
			all_fields(x)
		}
		if (inherits(y, "sf")) {
			all_fields_y = all_fields(y)
			st_geometry(y) = NULL
			if (is.null(df))
				df = y[idx[,2],,drop=FALSE]
			else
				df = cbind(df, y[idx[,2],,drop=FALSE])
		}
		st_geometry(df) = ret
		if (! (all_fields_x && all_fields_y))
			warning("attribute variables are assumed to be spatially constant throughout all geometries")
		df
	}
}

#' @name geos
#' @export
#' @return \code{st_union(x)} unions geometries.  Unioning a set of overlapping polygons has the effect of merging the areas (i.e. the same effect as iteratively unioning all individual polygons together). Unioning a set of LineStrings has the effect of fully noding and dissolving the input linework. In this context "fully noded" means that there will be a node or endpoint in the output for every endpoint or line segment crossing in the input. "Dissolved" means that any duplicate (e.g. coincident) line segments or portions of line segments will be reduced to a single line segment in the output.  Unioning a set of Points has the effect of merging al identical points (producing a set with no duplicates). If \code{y0} in a call to \code{st_union} is not missing, each of the geometries in \code{x} are unioned to the combination of \code{y0}.
#' @examples
#' plot(st_union(nc))
st_union = function(x, y) {
	if (! missing(y))
		geos_op2("union", st_geometry(x), st_geometry(y))
	else
		st_sfc(CPL_geos_union(st_geometry(x)), crs = st_crs(x))
}

#' @name geos
#' @export
st_difference = function(x, y) {
	geos_op2("difference", st_geometry(x), st_geometry(y))
}

#' @name geos
#' @export
st_sym_difference = function(x, y) {
	geos_op2("sym_difference", st_geometry(x), st_geometry(y))
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
#'   st_linestring(rbind(c(0,0),c(.1,0))), crs = 4326) 
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

#' make a rectangular grid of polygons over the bounding box of a sf or sfc object
#' 
#' make a rectangular grid of polygons over the bounding box of a sf or sfc object
#' @param x object of class \link{sf} or \link{sfc}
#' @param cellsize target cellsize
#' @param offset numeric of lengt 2; lower left corner coordinates (x, y) of the grid
#' @param n integer of length 1 or 2, number of grid cells in x and y direction (columns, rows)
#' @export
st_makegrid = function(x, cellsize = c(diff(st_bbox(x)[c(1,3)]), diff(st_bbox(x)[c(2,4)]))/n, 
		offset = st_bbox(x)[1:2], n = c(10, 10)) {
	if (!missing(n) && !missing(offset) && !missing(cellsize)) {
		cellsize = rep(cellsize, length.out = 2)
		bb = c(xmin = offset[1], ymin = offset[2], 
			xmax = offset[1] + n[1] * cellsize[1], ymax = offset[2] + n[2] * cellsize[2])
	} else
		bb = st_bbox(x)

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
	square = function(x1, y1, x2, y2)  {
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

#' areal-weighted interpolation of polygon data
#' 
#' areal-weighted interpolation of polygon data
#' @param x object of class \code{sf}, for which we want to aggregate attributes
#' @param to object of class \code{sf} or \code{sfc}, with the target geometries
#' @param extensive logical; if TRUE, the attribute variables are assumed to be spatially extensive (like population), otherwise, spatially intensive (like population density).
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' g = sf:::st_makegrid(nc, n = c(20,10))
#' a1 = st_interpolate_aw(nc["BIR74"], g, extensive = FALSE)
#' sum(a1$BIR74) / sum(nc$BIR74) # not close to one: property is assumed spatially intensive
#' a2 = st_interpolate_aw(nc["BIR74"], g, extensive = TRUE)
#' sum(a2$BIR74) / sum(nc$BIR74)
#' a1$intensive = a1$BIR74
#' a1$extensive = a2$BIR74
#' plot(a1[c("intensive", "extensive")])
#' @export
st_interpolate_aw = function(x, to, extensive) {
	if (!inherits(to, "sf") && !inherits(to, "sfc"))
		stop("aggregate.sf requires geometries in argument to")
	i = st_intersection(st_geometry(x), st_geometry(to))
	idx = attr(i, "idx")
	i = st_cast(i, "MULTIPOLYGON")
	x$...area_s = unclass(st_area(x))
	st_geometry(x) = NULL # sets back to data.frame
	x = x[idx[,1], ]      # create st table
	x$...area_st = unclass(st_area(i))
	x$...area_t = unclass(st_area(to)[idx[,2]])
	x = if (extensive)
			lapply(x, function(v) v * x$...area_st / x$...area_s)
		else
			lapply(x, function(v) v * x$...area_st / x$...area_t)
	x = aggregate(x, list(idx[,2]), sum)
	df = st_sf(x, geometry = st_geometry(to)[x$Group.1])
	# FIXME: need to take care of relation_to_geometry here...
	# clean up:
	df$...area_t = df$...area_st = df$...area_s = NULL 
	df
}
