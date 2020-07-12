#' @export
#' @name st_sample
st_sample = function(x, size, ...) UseMethod("st_sample")

#' sample points on or in (sets of) spatial features
#'
#' Sample points on or in (sets of) spatial features.
#' By default, returns a pre-specified number of points that is equal to
#' \code{size} (if \code{type = "random"}) or an approximation of
#' \code{size} (for other sampling types). \code{spatstat} methods are
#' interfaced and do not use the \code{size} argument.
#'
#' The function is vectorised: it samples \code{size} points across all geometries in
#' the object if \code{size} is a single number, or the specified number of points
#' in each feature if \code{size} is a vector of integers equal in length to the geometry
#' of \code{x}.
#'
#' @param x object of class \code{sf} or \code{sfc}
#' @param size sample size(s) requested; either total size, or a numeric vector with sample sizes for each feature geometry. When sampling polygons, the returned sampling size may differ from the requested size, as the bounding box is sampled, and sampled points intersecting the polygon are returned.
#' @param ... passed on to \link[base]{sample} for \code{multipoint} sampling, or to \code{spatstat} functions for spatstat sampling types (see details)
#' @param type character; indicates the spatial sampling type; one of \code{random}, \code{hexagonal} (triangular really), \code{regular},
#' or one of the \code{spatstat} methods such as \code{Thomas} for calling \code{spatstat::rThomas} (see Details).
#' @param exact logical; should the length of output be exactly
#' the same as specified by \code{size}? \code{TRUE} by default. Only applies to polygons, and
#' when \code{type = "random"}.
#' @return an \code{sfc} object containing the sampled \code{POINT} geometries
#' @details if \code{x} has dimension 2 (polygons) and geographical coordinates (long/lat), uniform random sampling on the sphere is applied, see e.g. \url{http://mathworld.wolfram.com/SpherePointPicking.html}
#'
#' For \code{regular} or \code{hexagonal} sampling of polygons, the resulting size is only an approximation.
#'
#' As parameter called \code{offset} can be passed to control ("fix") regular or hexagonal sampling: for polygons a length 2 numeric vector (by default: a random point from \code{st_bbox(x)}); for lines use a number like \code{runif(1)}.
#'
#' Sampling methods from package \code{spatstat} are interfaced (see examples), and need their own parameters to be set. 
#' For instance, to use \code{spatstat::rThomas()}, set \code{type = "Thomas"}.
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' p1 = st_sample(nc[1:3, ], 6)
#' p2 = st_sample(nc[1:3, ], 1:3)
#' plot(st_geometry(nc)[1:3])
#' plot(p1, add = TRUE)
#' plot(p2, add = TRUE, pch = 2)
#' x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,90),c(0,90),c(0,0)))), crs = st_crs(4326))
#' plot(x, axes = TRUE, graticule = TRUE)
#' if (sf_extSoftVersion()["proj.4"] >= "4.9.0")
#'   plot(p <- st_sample(x, 1000), add = TRUE)
#' x2 = st_transform(st_segmentize(x, 1e4), st_crs("+proj=ortho +lat_0=30 +lon_0=45"))
#' g = st_transform(st_graticule(), st_crs("+proj=ortho +lat_0=30 +lon_0=45"))
#' plot(x2, graticule = g)
#' if (sf_extSoftVersion()["proj.4"] >= "4.9.0") {
#'   p2 = st_transform(p, st_crs("+proj=ortho +lat_0=30 +lon_0=45"))
#'   plot(p2, add = TRUE)
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
#' #if (sf_extSoftVersion()["proj.4"] >= "4.9.0") {
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
#' if (require(spatstat)) {
#'  x <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 0)))))
#'  # for spatstat::rThomas(), set type = "Thomas":
#'  pts <- st_sample(x, kappa = 1, mu = 10, scale = 0.1, type = "Thomas") 
#' }
#' @export
#' @name st_sample
st_sample.sf = function(x, size, ...) st_sample(st_geometry(x), size, ...)

#' @export
#' @name st_sample
st_sample.sfc = function(x, size, ..., type = "random", exact = TRUE) {

	if (!missing(size) && any(size %% 1 != 0))
		stop("size should be an integer")
	if (!missing(size) && length(size) > 1) { # recurse:
		size = rep(size, length.out = length(x))
		ret = lapply(1:length(x), function(i) st_sample(x[i], size[i], type = type, exact = exact, ...))
		res = st_set_crs(do.call(c, ret), st_crs(x))
	} else {
		res = switch(max(st_dimension(x)) + 1,
					 st_multipoints_sample(do.call(c, x), size = size, ..., type = type),
					 st_ll_sample(st_cast(x, "LINESTRING"), size = size, ..., type = type),
					 st_poly_sample(x, size = size, ..., type = type))
		if (exact & type == "random" & all(st_geometry_type(res) == "POINT")) {
			diff = size - length(res)
			if(diff > 0) { # too few points
				res_additional = st_sample_exact(x = x, size = diff, ..., type = type)
				res = c(res, res_additional)
			} else if (diff < 0) { # too many points
				res = res[1:size]
			}
		}
	}
	res
}

#' @export
#' @name st_sample
st_sample.sfg = function(x, size, ...) {
	st_sample(st_geometry(x), size, ...)
}

st_poly_sample = function(x, size, ..., type = "random",
                          offset = st_sample(st_as_sfc(st_bbox(x)), 1)[[1]]) {

	if (type %in% c("hexagonal", "regular", "random")) {

		if (isTRUE(st_is_longlat(x))) {
			if (type == "regular")
				message_longlat("st_sample")
			if (type == "hexagonal")
				stop("hexagonal sampling on geographic coordinates not supported; consider projecting first")
		}

		a0 = as.numeric(st_area(st_make_grid(x, n = c(1,1))))
		a1 = as.numeric(sum(st_area(x)))
		# st_polygon(list(rbind(c(-180,-90),c(180,-90),c(180,90),c(-180,90),c(-180,-90))))
		# for instance has 0 st_area
		if (is.finite(a0) && is.finite(a1) && a0 > a0 * 0.0 && a1 > a1 * 0.0)
			size = round(size * a0 / a1)
		bb = st_bbox(x)

		pts = if (type == "hexagonal") {
			dx = sqrt(a0 / size / (sqrt(3)/2))
			hex_grid_points(x, pt = offset, dx = dx)
		} else if (type == "regular") {
			dx = as.numeric(sqrt(a0 / size))
			offset = c((offset[1] - bb["xmin"]) %% dx,
				(offset[2] - bb["ymin"]) %% dx) + bb[c("xmin", "ymin")]
			n = c(round((bb["xmax"] - offset[1])/dx), round((bb["ymax"] - offset[2])/dx))
			st_make_grid(x, cellsize = c(dx, dx), offset = offset, n = n, what = "corners")
		} else if (type == "random") {
			lon = runif(size, bb[1], bb[3])
			lat = if (isTRUE(st_is_longlat(x))) { # sampling on the sphere:
				toRad = pi/180
				lat0 = (sin(bb[2] * toRad) + 1)/2
				lat1 = (sin(bb[4] * toRad) + 1)/2
				y = runif(size, lat0, lat1)
				asin(2 * y - 1) / toRad # http://mathworld.wolfram.com/SpherePointPicking.html
			} else
				runif(size, bb[2], bb[4])
			m = cbind(lon, lat)
			st_sfc(lapply(seq_len(nrow(m)), function(i) st_point(m[i,])), crs = st_crs(x))
		}
		pts[x]
	} else { # try to go into spatstat
		if (!requireNamespace("spatstat", quietly = TRUE))
			stop("package spatstat required, please install it first")
		spatstat_fun = try(get(paste0("r", type), asNamespace("spatstat")), silent = TRUE)
		if (inherits(spatstat_fun, "try-error"))
			stop(paste0("r", type), " is not an exported function from spatstat.")
		pts = try(spatstat_fun(..., win = spatstat::as.owin(x)), silent = TRUE)
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

st_ll_sample = function (x, size, ..., type = "random", offset = runif(1)) {
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

	y  <- rep(y, each = length(x))
	x  <- rep(c(x, x + dx / 2), length.out = length(y))
	xy = cbind(x, y)[x >= xlim[1] & x <= xlim[2] & y >= ylim[1] & y <= ylim[2], ]
	st_sfc(lapply(seq_len(nrow(xy)), function(i) st_point(xy[i,])), crs = st_crs(bb))
}

st_sample_exact = function(x, size, ..., type) {
	random_pt = st_sample(x = x, size = size, ..., type = type, exact = FALSE)
	while (length(random_pt) < size) {
		diff = size - length(random_pt)
		random_pt_new = st_sample(x, size = diff, ..., type, exact = FALSE)
		random_pt = c(random_pt, random_pt_new)
	}
	if(length(random_pt) > size) {
		random_pt = random_pt[1:size]
	}
	random_pt
}
