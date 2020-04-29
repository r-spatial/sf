# see https://docs.google.com/presentation/d/1Hl4KapfAENAOf4gv-pSngKwvS_jwNVHRPZTTDzXXn6Q/view?pli=1#slide=id.i0
# and the libs2 package:
# https://github.com/r-spatial/libs2


#' functions for spherical geometry, using libs2 package
#' 
#' functions for spherical geometry, using the libs2 package based on the google s2geometry.io library
#' @name s2 
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg}
#' @export
#' @details \code{st_as_s2} converts an \code{sf} POLYGON object into a form readable by \code{s2}.
#' @examples
#' m = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))
#' m1 = rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,0), c(-1,-1))
#' m0 = m[5:1,]
#' mp = st_multipolygon(list(
#'	list(m, 0.8 * m0, 0.01 * m1 + 0.9),
#'	list(0.7* m, 0.6*m0), 
#'	list(0.5 * m0), 
#'	list(m+2),
#'	list(m+4,(.9*m0)+4)
#'	))
#' sf = st_sfc(mp, mp)
#' s2 = st_as_s2(s)
#' s2 = list(s2MakePolygon(p, FALSE))
st_as_s2 = function(x, ...) UseMethod("st_as_s2")

#' @name s2
#' @param oriented logical; are the polygon rings guaranteed to be oriented (CCW outer rings, CW holes)?
#' @export
st_as_s2.sfc = function(x, ..., oriented = FALSE) {
	crs = st_crs(x)
	if (!st_is_longlat(x))
		stop("st_as_s2 needs geographic coordinates; consider using st_transform to EPSG:4326")
	load_libs2()
	stopifnot(all(st_dimension(x) == 2))
	x = st_cast(x, "MULTIPOLYGON") # FIXME: ?
	x = lapply(x, unlist, recursive = FALSE)
	ret = lapply(x, s2MakePolygon, oriented = oriented)
	structure(ret, crs = crs, class = "S2Polygon")
}

#' @export
st_as_s2.sf = function(x, ..., oriented = FALSE) {
	st_as_s2(st_geometry(x), ..., oriented = oriented)
}

#' @export
st_as_s2.sfg = function(x, ..., oriented = FALSE, crs = st_crs('EPSG:4326')) {
	st_as_s2(st_geometry(x, crs = crs), ..., oriented = oriented)
}

load_libs2 = function() {
	if (! requireNamespace("libs2", quietly = TRUE))
		stop('package libs2 required, please install it first with remotes::install_github("r-spatial/libs2")')
}

#' @name s2
#' @export
#' @param s2 object of class \code{S2Polygon}
#' @examples
#' x = st_as_sfc(s2)
#' all.equal(st_area(sf), st_area(x))
st_as_sfc.S2Polygon = function(x, ...) {
	st_sfc(lapply(s2GetPolygon(s2), st_multipolygon), crs = st_crs(x))
}



#' @export
st_as_sfc.S2Points = function(x, ..., crs = st_crs(4326), radius = earth_radius) {
	x = x * radius
	st_zm(st_transform(
		st_sfc(lapply(1:nrow(x), function(i) st_point(x[i,])), crs = st_crs(s2_p4s)),
		crs))
}


#' @name s2
#' @export
st_intersects.S2Polygon = function(x, y = x) {
	load_s2()
	stopifnot(st_crs(x) == st_crs(y), inherits(y, "S2Polygon"))
	libs2::s2Intersect(x, y)
}

#' @name s2
#' @export
#' @details \code{s2_centroid} computes the spherical centroid of a set of polygons
#' @examples
#' demo(nc, echo = FALSE, ask = FALSE)
#' s2_centroid(nc)
#' pts = rbind(c(0,80), c(120,80), c(240,80), c(0,80))
#' pole = st_sfc(st_polygon(list(pts)), crs = 4326)
#' s2_centroid(pole)
#s2_centroid = function(x) {
#	load_s2()
#	st_as_sfc(structure(s2::S2Polygons_centroid(st_as_s2(x)), class = "S2Points"),
#		crs = st_crs(x))
#}

#' @name s2
#' @export
#' @details \code{s2_area} computes the area of a set of polygons, as a fraction of 4 * pi.
#' @examples
#' demo(nc, echo = FALSE, ask = FALSE)
#' s2_area(nc)
#s2_area = function(x) {
#	load_s2()
#	s2::S2Polygons_area(st_as_s2(x)) * earth_radius^2
#}

st_area.S2Polygon = function(x, ...) {
}
