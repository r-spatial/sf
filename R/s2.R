# see https://docs.google.com/presentation/d/1Hl4KapfAENAOf4gv-pSngKwvS_jwNVHRPZTTDzXXn6Q/view?pli=1#slide=id.i0
# and the S2 package:
# https://cran.r-project.org/package=s2
# https://github.com/spatstat/s2

s2_p4s = "+proj=geocent +a=1 +b=1 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"

st_as_s2 = function(x) {
	# geocentric, spherical:
	geom = st_transform(st_geometry(x), st_crs(s2_p4s))
	if (inherits(geom, "sfc_MULTIPOLYGON")) # unlist: S2 sorts out what are holes
		geom = lapply(geom, unlist, recursive = FALSE)
	else if (!inherits(geom, "sfc_POLYGON"))
		stop("objects of class sfc_MULTIPOLYGON or sfc_POLYGON accepted")
	geom
}

load_s2 = function() {
	if (! requireNamespace("s2", quietly = TRUE))
		stop("package s2 required, please install it first")
}

#' @export
st_as_sfc.S2Polygon = function(x, ..., crs = st_crs(4326)) {
	# close all loops:
	loops = lapply(x$loops, function(L) rbind(L, L[1,]))
	loops[x$holes] = lapply(loops[x$holes], function(L) L[nrow(L):1, ])
	w = which(! x$holes)
	splt = rep(seq_along(w), diff(c(w, length(x$holes) + 1)))
	p = if (length(splt) > 1)
		st_multipolygon(split(loops, splt))
	else
		st_polygon(loops)
	st_zm(st_transform(st_sfc(p, crs = st_crs(s2_p4s)), crs))
}

#' @export
st_as_sfc.S2Polygons = function(x, ..., crs = st_crs(4326)) {
	structure(do.call(c, lapply(x, st_as_sfc, crs = crs)), n_empty = attr(x, "n_empty"))
}

#' @export
st_as_sfc.S2Points = function(x, ..., crs = st_crs(4326)) {
	st_zm(st_transform(
		st_sfc(lapply(1:nrow(x), function(i) st_point(x[i,])), crs = st_crs(s2_p4s)),
		crs))
}

s2_intersection = function(x, y) {
	load_s2()
	stopifnot(st_crs(x) == st_crs(y))
	st_as_sfc(s2::S2Polygons_intersection(st_as_s2(x), st_as_s2(y)), crs = st_crs(x))
}

s2_intersects = function(x, y) {
	load_s2()
	stopifnot(st_crs(x) == st_crs(y))
	s2::S2Polygons_intersect(st_as_s2(x), st_as_s2(y))
}

s2_centroid = function(x) {
	load_s2()
	st_as_sfc(structure(s2::S2Polygons_centroid(st_as_s2(x)), class = "S2Points"),
		crs = st_crs(x))
}

s2_area = function(x) {
	load_s2()
	s2::S2Polygons_area(st_as_s2(x))
}
