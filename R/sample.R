#' sample points on or in (sets of) spatial features
#'
#' sample points on or in (sets of) spatial features
#' @param x object of class \code{sf} or \code{sfc}
#' @param size sample size(s) requested; either total size, or a numeric vector with sample sizes for each feature geometry. When sampling polygons, the returned sampling size may differ from the requested size, as the bounding box is sampled, and sampled points intersecting the polygon are returned.
#' @param ... ignored, or passed on to \link[base]{sample} for \code{multipoint} sampling
#' @param type character; indicates the spatial sampling type; only \code{random} is implemented right now
#' @details if \code{x} has dimension 2 (polygons) and geographical coordinates (long/lat), uniform random sampling on the sphere is applied, see e.g. \url{http://mathworld.wolfram.com/SpherePointPicking.html}
#' @examples
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
#' x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,90),c(0,90),c(0,0))))) # NOT long/lat:
#' plot(x)
#' plot(st_sample(x, 1000), add = TRUE)
#' x = st_sfc(st_polygon(list(rbind(c(-180,-90),c(180,-90),c(180,90),c(-180,90),c(-180,-90)))),
#'	 crs=st_crs(4326))
#' if (sf_extSoftVersion()["proj.4"] >= "4.9.0") {
#'   p = st_sample(x, 1000)
#'   st_sample(p, 3)
#' }
#' pt = st_multipoint(matrix(1:20,,2))
#' ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
#'  st_linestring(rbind(c(0,0),c(.1,0))),
#'  st_linestring(rbind(c(0,1),c(.1,1))),
#'  st_linestring(rbind(c(2,2),c(2,2.00001))))
#' st_sample(ls, 80)
#' @export
st_sample = function(x, size, ..., type = "random") {
	x = st_geometry(x)
	if (length(size) > 1) {
		size = rep(size, length.out = length(x))
		st_set_crs(do.call(c, lapply(1:length(x),
			function(i) st_sample(x[i], size[i], type = type))),
			st_crs(x))
	} else {
		dim = max(st_dimension(x))
		if (dim == 0)
			st_multipoints_sample(do.call(c, x), size, ..., type = type)
		else if (dim == 1)
			st_ll_sample(st_cast(x, "LINESTRING"), size, ..., type = type)
		else { # 2: -- take care of empty geoms?
			stopifnot(dim == 2)
			st_poly_sample(x, size, ..., type = type)
		}
	}
}

st_poly_sample = function(x, size, ..., type = "random") {
	toRad = pi/180
	bb = st_bbox(x)
	a0 = st_area(st_make_grid(x, n = c(1,1)))
	a1 = sum(st_area(x))
	# st_polygon(list(rbind(c(-180,-90),c(180,-90),c(180,90),c(-180,90),c(-180,-90))))
	# for instance has 0 st_area
	if (is.finite(a0) && is.finite(a1) && a0 > a0 * 0.0 && a1 > a1 * 0.0)
		size = round(size * a0 / a1)
	lon = runif(size, bb[1], bb[3])
	lat = if (isTRUE(st_is_longlat(x))) { # sampling on the sphere:
		lat0 = (sin(bb[2] * toRad) + 1)/2
		lat1 = (sin(bb[4] * toRad) + 1)/2
		y = runif(size, lat0, lat1)
		asin(2 * y - 1) / toRad # http://mathworld.wolfram.com/SpherePointPicking.html
	} else
		runif(size, bb[2], bb[4])
	m = cbind(lon, lat)
	pts = st_sfc(lapply(seq_len(nrow(m)), function(i) st_point(m[i,])), crs = st_crs(x))
	i = st_intersects(pts, x)
	pts[lengths(i) > 0]
}

st_multipoints_sample = function(x, size, ..., type = "random") {
	if (!inherits(x, "MULTIPOINT"))
		stop("points sampling only implemented for MULTIPOINT; use sample to sample individual features", call.=FALSE)
	m = unclass(x)
	st_sfc(st_multipoint(m[sample(nrow(m), size, ...),]), crs = st_crs(x))
}

st_ll_sample = function(x, size, ..., type = "random") {
	l = st_length(x)
	d = runif(size, 0, sum(l))
	lcs = c(0, cumsum(l))
	grp = split(d, cut(d, lcs, include.lowest = TRUE))
	grp = lapply(seq_along(x), function(i) grp[[i]] - lcs[i])
	st_sfc(CPL_gdal_linestring_sample(x, grp), crs = st_crs(x))
}
