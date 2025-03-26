#' @export
#' @name st_sample
st_sample = function(x, size, ...) UseMethod("st_sample")

#' sample points on or in (sets of) spatial features
#'
#' Sample points on or in (sets of) spatial features.
#' By default, returns a pre-specified number of points that is equal to
#' \code{size} (if \code{type = "random"} and \code{exact = TRUE}) or an approximation of
#' \code{size} otherwise. \code{spatstat} methods are
#' interfaced and do not use the \code{size} argument, see examples.
#'
#' The function is vectorised: it samples \code{size} points across all geometries in
#' the object if \code{size} is a single number, or the specified number of points
#' in each feature if \code{size} is a vector of integers equal in length to the geometry
#' of \code{x}.
#'
#' @param x object of class \code{sf} or \code{sfc}
#' @param size sample size(s) requested; either total size, or a numeric vector with sample sizes for each feature geometry. When sampling polygons, the returned sampling size may differ from the requested size, as the bounding box is sampled, and sampled points intersecting the polygon are returned.
#' @param warn_if_not_integer logical; if \code{FALSE} then no warning is emitted if \code{size} is not an integer
#' @param ... passed on to \link[base]{sample} for \code{multipoint} sampling, or to \code{spatstat} functions for spatstat sampling types (see details)
#' @param type character; indicates the spatial sampling type; one of \code{random}, \code{hexagonal} (triangular really), \code{regular}, \code{Fibonacci},
#' or one of the \code{spatstat} methods such as \code{Thomas} for calling \code{spatstat.random::rThomas} (see Details).
#' @param exact logical; should the length of output be exactly
#' @param by_polygon logical; for \code{MULTIPOLYGON} geometries, should the effort be split by \code{POLYGON}? See https://github.com/r-spatial/sf/issues/1480
#' the same as specified by \code{size}? \code{TRUE} by default. Only applies to polygons, and
#' when \code{type = "random"}.
#' @param progress logical; if \code{TRUE} show progress bar (only if \code{size} is a vector).
#' @param force logical; if `TRUE` continue when the sampled bounding box area is more than 1e4 times the area of interest, else (default) stop with an error. If this error is not justified, try setting `oriented=TRUE`, see details.
#' @return an \code{sfc} object containing the sampled \code{POINT} geometries
#' @details if \code{x} has dimension 2 (polygons) and geographical coordinates (long/lat), uniform random sampling on the sphere is applied, see e.g. \url{https://mathworld.wolfram.com/SpherePointPicking.html}. 
#'
#' For \code{regular} or \code{hexagonal} sampling of polygons, the resulting size is only an approximation.
#'
#' As parameter called \code{offset} can be passed to control ("fix") regular or hexagonal sampling: for polygons a length 2 numeric vector (by default: a random point from \code{st_bbox(x)}); for lines use a number like \code{runif(1)}.
#' 
#' Fibonacci sampling see: Alvaro Gonzalez, 2010. Measurement of Areas on a Sphere Using Fibonacci and Latitude-Longitude Lattices. 
#' Mathematical Geosciences 42(1), p. 49-64
#' 
#' For regular sampling on the sphere, see also \code{geosphere::regularCoordinates}.
#'
#' Sampling methods from package \code{spatstat} are interfaced (see examples), and need their own parameters to be set. 
#' For instance, to use \code{spatstat.random::rThomas()}, set \code{type = "Thomas"}.
#' 
#' For sampling polygons one can specify `oriented=TRUE` to make sure that polygons larger than half the globe are not reverted, e.g. when specifying a polygon from a bounding box of a global dataset. The `st_sample` method for `bbox` does this by default.
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' p1 = st_sample(nc[1:3, ], 6)
#' p2 = st_sample(nc[1:3, ], 1:3)
#' plot(st_geometry(nc)[1:3])
#' plot(p1, add = TRUE)
#' plot(p2, add = TRUE, pch = 2)
#' x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,90),c(0,90),c(0,0)))), crs = st_crs(4326))
#' plot(x, axes = TRUE, graticule = TRUE)
#' if (compareVersion(sf_extSoftVersion()["proj.4"], "4.9.0") >= 0)
#'   plot(p <- st_sample(x, 1000), add = TRUE)
#' if (require(lwgeom, quietly = TRUE)) { # for st_segmentize()
#'   x2 = st_transform(st_segmentize(x, 1e4), st_crs("+proj=ortho +lat_0=30 +lon_0=45"))
#'   g = st_transform(st_graticule(), st_crs("+proj=ortho +lat_0=30 +lon_0=45"))
#'   plot(x2, graticule = g)
#'   if (compareVersion(sf_extSoftVersion()["proj.4"], "4.9.0") >= 0) {
#'     p2 = st_transform(p, st_crs("+proj=ortho +lat_0=30 +lon_0=45"))
#'     plot(p2, add = TRUE)
#'   }
#' }
#' x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,10),c(0,90),c(0,0))))) # NOT long/lat:
#' plot(x)
#' p_exact = st_sample(x, 1000, exact = TRUE)
#' p_not_exact = st_sample(x, 1000, exact = FALSE)
#' length(p_exact); length(p_not_exact)
#' plot(st_sample(x, 1000), add = TRUE)
#' x = st_sfc(st_polygon(list(rbind(c(-180,-90),c(180,-90),c(180,90),c(-180,90),c(-180,-90)))),
#'	 crs=st_crs(4326))
#' # FIXME:
#' #if (compareVersion(sf_extSoftVersion()["proj.4"], "4.9.0") >= 0) {
#' #  p = st_sample(x, 1000)
#' #  st_sample(p, 3)
#' #}
#' # hexagonal:
#' sfc = st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,0)))))
#' plot(sfc)
#' h = st_sample(sfc, 100, type = "hexagonal")
#' h1 = st_sample(sfc, 100, type = "hexagonal")
#' plot(h, add = TRUE)
#' plot(h1, col = 'red', add = TRUE)
#' c(length(h), length(h1)) # approximate!
#' pt = st_multipoint(matrix(1:20,,2))
#' ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
#'  st_linestring(rbind(c(0,0),c(.1,0))),
#'  st_linestring(rbind(c(0,1),c(.1,1))),
#'  st_linestring(rbind(c(2,2),c(2,2.00001))))
#' st_sample(ls, 80)
#' plot(st_sample(ls, 80))
#' # spatstat example:
#' if (require(spatstat.random)) {
#'   x <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 0)))))
#'   # for spatstat.random::rThomas(), set type = "Thomas":
#'   pts <- st_sample(x, kappa = 1, mu = 10, scale = 0.1, type = "Thomas") 
#' }
#' @export
#' @name st_sample
st_sample.sf = function(x, size, ...) st_sample(st_geometry(x), size, ...)

#' @export
#' @name st_sample
st_sample.sfc = function(x, size, ..., type = "random", exact = TRUE, warn_if_not_integer = TRUE,
		by_polygon = FALSE, progress = FALSE, force = FALSE) {

	if (!missing(size) && warn_if_not_integer && any(size %% 1 != 0))
		warning("size is not an integer")
	if (!missing(size) && length(size) > 1) { # recurse:
		size = rep(size, length.out = length(x))
		ret = if (progress) {
				if (!requireNamespace("pbapply", quietly = TRUE))
					stop("package pbapply required, please install it first")
				pbapply::pblapply(seq_along(x), function(i) st_sample(x[i], size[i], type = type, exact = exact, ...))
			} else
				lapply(seq_along(x), function(i) st_sample(x[i], size[i], type = type, exact = exact, ...))
		st_set_crs(do.call(c, ret), st_crs(x))
	} else {
		res = switch(max(st_dimension(x)) + 1,
					 st_multipoints_sample(do.call(c, x), size = size, ..., type = type),
					 st_ll_sample(st_cast(x, "LINESTRING"), size = size, ..., type = type),
					 st_poly_sample(x, size = size, ..., type = type, by_polygon = by_polygon, force = force))
		if (exact && type == "random" && all(st_geometry_type(res) == "POINT")) {
			diff = size - length(res)
			if (diff > 0) { # too few points
				res_additional = st_sample_exact(x = x, size = diff, ..., 
					type = type, by_polygon = by_polygon)
				res = c(res, res_additional)
			} else if (diff < 0) { # too many points
				res = res[1:size]
			}
		}
		res
	}
}

#' @export
#' @name st_sample
st_sample.sfg = function(x, size, ...) {
	st_sample(st_geometry(x), size, ...)
}

#' @export
#' @name st_sample
#' @param great_circles logical; if `TRUE`, great circle arcs are used to connect the bounding box vertices, if `FALSE` parallels (graticules)
#' @param segments units, or numeric (degrees); segment sizes for segmenting a bounding box polygon if `great_circles` is `FALSE`
#' @examples
#' bbox = st_bbox(
#'	c(xmin = 0, xmax = 40, ymax = 70, ymin = 60),
#' 	crs = st_crs('OGC:CRS84')
#' )
#' set.seed(13531)
#' s1 = st_sample(bbox, 400)
#' st_bbox(s1) # within bbox
#' s2 = st_sample(bbox, 400, great_circles = TRUE)
#' st_bbox(s2) # outside bbox
st_sample.bbox = function(x, size, ..., great_circles = FALSE, segments = units::set_units(2, "degree", mode = "standard")) {
	polygon = st_as_sfc(x)
	crs = st_crs(x)
	if (isTRUE(st_is_longlat(x)) && !great_circles) {
		st_crs(polygon) = NA_crs_ # to fool segmentize that we're on R2:
		segments = units::drop_units(units::set_units(segments, "degree", mode = "standard"))
		polygon = st_set_crs(st_segmentize(polygon, segments), crs)
	}
	st_sample(polygon, size, ..., oriented = TRUE)
}

st_poly_sample = function(x, size, ..., type = "random",
						offset = st_sample(st_as_sfc(st_bbox(x)), 1)[[1]],
						by_polygon = FALSE, oriented = FALSE, force = FALSE) {

	if (by_polygon && inherits(x, "sfc_MULTIPOLYGON")) { # recurse into polygons:
		sum_a = units::drop_units(sum(st_area(x)))
		x = lapply(suppressWarnings(st_cast(st_geometry(x), "POLYGON")), st_sfc, crs = st_crs(x))
		a = sapply(x, st_area)
		ret = mapply(st_poly_sample, x, size = size * a / sum_a, type = type, ...)
		do.call(c, ret)
	} else if (type %in% c("hexagonal", "regular", "random", "Fibonacci")) {

		if (isTRUE(st_is_longlat(x))) {
			if (type == "regular") {
				message_longlat("st_sample")
				x = st_set_crs(x, NA)
			}
			if (type == "hexagonal")
				stop("hexagonal sampling on geographic coordinates not supported; consider projecting first")
		} else if (type == "Fibonacci")
			stop("Fibonacci sampling requires geographic (longlat) coordinates")
		
		global = FALSE
		bb = st_bbox(x)
		if (isTRUE(st_is_longlat(x))) {
			if (sf_use_s2()) { # if FALSE, the user wants the coord ranges to be the bbox
				if (!requireNamespace("lwgeom", quietly = TRUE))
					warning("coordinate ranges not computed along great circles; install package lwgeom to get rid of this warning")
				else {
					# see https://github.com/r-spatial/sf/issues/2331
					# bb = st_bbox(st_segmentize(st_as_sfc(bb),
					#		units::set_units(1, "degree", mode = "standard"))) # get coordinate range on S2
					dfMaxLength = units::set_units(100000, "m", mode = "standard")
					if (! is.na(st_crs(x)))
						units(dfMaxLength) = units(st_crs(x)$SemiMajor) # might convert
					seg = st_sfc(CPL_gdal_segmentize(x, dfMaxLength), crs = st_crs(x)) # avoid lwgeom path
					bb = st_bbox(seg)
				}
			}
			R = s2::s2_earth_radius_meters()
			toRad = pi / 180
			h1 = sin(bb["ymax"] * toRad)
			h2 = sin(bb["ymin"] * toRad)
			a0 = 2 * pi * R^2. * (h1 - h2) * (bb["xmax"] - bb["xmin"]) / 360.
			a1 = sum(s2::s2_area(st_as_s2(x, oriented = oriented)))
			if (!is.finite(a1))
				stop("One or more geometries have a non-finite area")
			global = (a1 / a0) > .9999
			if (a0 / a1 > 1e4 && !force)
				stop(paste0("sampling box is ", format(a0/a1), " times larger than sampling region;\nuse force=TRUE if you really want this, or try setting oriented=TRUE\n(after reading the documentation)"), call. = FALSE)
			size = round(size * a0 / a1)
		} else {
			a0 = as.numeric(st_area(st_as_sfc(bb)))
			a1 = as.numeric(sum(st_area(x)))
			# we're sampling from a box, so n should be size_desired * a0 / a1
			if (is.finite(a0) && is.finite(a1) && a0 > a0 * 0.0 && a1 > a1 * 0.0) { # FIXME: reqs can be removed, now we handle long/lat separately?
				r = size * a0 / a1
				size = if (round(r) == 0)
						rbinom(1, 1, r)
					else
						round(r)
			}
		}

		pts = if (type == "hexagonal") {
				dx = sqrt(a0 / size / (sqrt(3)/2))
				hex_grid_points(x, pt = offset, dx = dx)
			} else if (type == "regular") {
				dx = as.numeric(sqrt(a0 / size))
				offset = c((offset[1] - bb["xmin"]) %% dx,
					(offset[2] - bb["ymin"]) %% dx) + bb[c("xmin", "ymin")]
				n = c(round((bb["xmax"] - offset[1])/dx), round((bb["ymax"] - offset[2])/dx))
				st_make_grid(x, cellsize = c(dx, dx), offset = offset, n = n, what = "corners")
			} else { 
				m = if (type == "random") {
						lon = runif(size, bb[1], bb[3])
						lat = if (isTRUE(st_is_longlat(x))) { # sampling on the sphere:
							toRad = pi/180
							lat0 = (sin(bb[2] * toRad) + 1)/2
							lat1 = (sin(bb[4] * toRad) + 1)/2
							y = runif(size, lat0, lat1)
							asin(2 * y - 1) / toRad # http://mathworld.wolfram.com/SpherePointPicking.html
						} else
							runif(size, bb[2], bb[4])
						structure(cbind(lon, lat), dimnames = NULL)
					} else if (type == "Fibonacci")
						fiboGrid(size %/% 2, bb[c("xmin", "xmax")], bb[c("ymin", "ymax")])
					else
						stop("unknown value for type")
				# st_sfc(lapply(seq_len(nrow(m)), function(i) st_point(m[i,])), crs = st_crs(x))
				st_as_sf(as.data.frame(m), coords = 1:2, crs = st_crs(x))[["geometry"]]
			}
		if (global)
			pts
		else
			pts[x] # cut out x from bbox
	} else { # try to go into spatstat
		if (!requireNamespace("spatstat.random", quietly = TRUE))
			stop("package spatstat.random required, please install it (or the full spatstat package) first")
		spatstat_fun = try(get(paste0("r", type), asNamespace("spatstat.random")), silent = TRUE)
		if (inherits(spatstat_fun, "try-error"))
			stop(paste0("r", type), " is not an exported function from spatstat.random.")
		pts = if ("win" %in% names(as.list(args(spatstat_fun))))
				try(spatstat_fun(..., win = spatstat.geom::as.owin(x)), silent = TRUE)
			else
				try(spatstat_fun(..., W = spatstat.geom::as.owin(x)), silent = TRUE)
		if (inherits(pts, "try-error"))
			stop("The spatstat function ", paste0("r", type),
				" did not return a valid result. Consult the help file.\n",
				"Error message from spatstat:\n", pts)
		st_as_sf(pts)[-1,]
	}
}

st_multipoints_sample = function(x, size, ..., type = "random") {
	if (!inherits(x, "MULTIPOINT"))
		stop("points sampling only implemented for MULTIPOINT; use sample to sample individual features", call.=FALSE)
	m = unclass(x)
	st_sfc(st_multipoint(m[sample(nrow(m), size, ...),]), crs = st_crs(x))
}

st_ll_sample = function(x, size, ..., type = "random", offset = runif(1)) {
	crs = st_crs(x)
	if (isTRUE(st_is_longlat(x))) {
		message_longlat("st_sample")
		st_crs(x) = NA_crs_
	}
	l = st_length(x)
	if (inherits(l, "units"))
		l = drop_units(l)
	if (type == "random") {
		d = runif(size, 0, sum(l))
	} else if (type == "regular") {
		d = ((1:size) - (1. - (offset %% 1)))/size * sum(l)
	} else {
		stop(paste("sampling type", type, "not available for LINESTRING")) # nocov
	}
	lcs = c(0, cumsum(l))
	if (sum(l) == 0) {
		grp = list(0) # nocov
		message("line is of length zero, only one point is sampled") # nocov
	} else {
		grp = split(d, cut(d, lcs, include.lowest = TRUE))
		grp = lapply(seq_along(x), function(i) grp[[i]] - lcs[i])
	}
	st_sfc(CPL_gdal_linestring_sample(x, grp), crs = crs)
}

### return points on a triangular grid that
## - covers a bounding box st_bbox(obj)
## - contains pt
## - has x spacing dx: the shortest distance between x coordinates with identical y coordinate
hex_grid_points = function(obj, pt, dx) {

	bb = st_bbox(obj)
	dy = sqrt(3) * dx / 2
	xlim = bb[c("xmin", "xmax")]
	ylim = bb[c("ymin", "ymax")]
	offset = c(x = (pt[1] - xlim[1]) %% dx, y = (pt[2] - ylim[1]) %% (2 * dy))
	x = seq(xlim[1] - dx, xlim[2] + dx, dx) + offset[1]
	y = seq(ylim[1] - 2 * dy, ylim[2] + 2 * dy, dy) + offset[2]

	y = rep(y, each = length(x))
	x = rep(c(x, x + dx / 2), length.out = length(y))
	xy = cbind(x, y)[x >= xlim[1] & x <= xlim[2] & y >= ylim[1] & y <= ylim[2], , drop = FALSE]
	colnames(xy) = NULL
	st_sfc(lapply(seq_len(nrow(xy)), function(i) st_point(xy[i,])), crs = st_crs(bb))
}

fiboGrid <- function(N, xlim = c(-180,180), ylim = c(-90,90)) {
	if (max(xlim) <= 180)
		subtr = 180
	else
		subtr = 0
    phi = (1 + sqrt(5))/2
    i = seq(-N, N)
    P = 2 * N + 1
    lat = asin(2*i / P) * 180 / pi
    lon = ((2 * pi * i / phi) %% pi) * 360 / pi - subtr
    sel = lon <= xlim[2] & lon >= xlim[1] & lat <= ylim[2] & lat >= ylim[1]
    cbind(lon, lat)[sel, ]
}

st_sample_exact = function(x, size, ..., type, by_polygon) {
	random_pt = st_sample(x = x, size = size, ..., type = type, exact = FALSE)
	while (length(random_pt) < size) {
		diff = size - length(random_pt)
		random_pt_new = st_sample(x, size = diff, ..., type, exact = FALSE, by_polygon = by_polygon)
		random_pt = c(random_pt, random_pt_new)
	}
	if (length(random_pt) > size) {
		random_pt = random_pt[1:size]
	}
	random_pt
}
