# see https://docs.google.com/presentation/d/1Hl4KapfAENAOf4gv-pSngKwvS_jwNVHRPZTTDzXXn6Q/view?pli=1#slide=id.i0
# and the libs2 package:
# https://github.com/r-spatial/libs2

#' @export
st_crs.S2Polygon = function(x, ...) {
	crs = attr(x, "crs")
	if (is.null(crs))
		NA_crs_
	else
		crs
}

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
#' sf = st_sfc(mp, mp, crs = 'EPSG:4326')
#' s2 = st_as_s2(sf)
st_as_s2 = function(x, ...) UseMethod("st_as_s2")

#' @name s2
#' @param oriented logical: are the polygon rings oriented (CCW outer rings, CW holes)?
#' @export
st_as_s2.sfc_MULTIPOLYGON = function(x, ..., oriented = FALSE) {
	crs = st_crs(x)
	if (!st_is_longlat(x))
		stop("st_as_s2 needs geographic coordinates; consider using st_transform to EPSG:4326")
	load_libs2()
	x = lapply(x, unlist, recursive = FALSE)
	ret = lapply(x, libs2::s2MakePolygon, oriented = oriented)
	structure(ret, crs = crs, class = "S2Polygon")
}

#' @name s2
#' @export
st_as_s2.sfc_POLYGON = function(x, ..., oriented = FALSE) {
	st_as_s2(st_cast(x, "MULTIPOLYGON"), ..., oriented = oriented)
}

#' @name s2
#' @export
#' @examples
#' s2pts = st_as_s2(st_sfc(st_point(c(0,0)), st_point(c(2,2))), crs = 4326)
st_as_s2.sfc_POINT = function(x, ..., oriented = FALSE) {
	load_libs2()
	structure(lapply(x, libs2::s2MakePoint), crs = st_crs(x), class = "S2Point")
}

#' @name s2
#' @export
#' @examples
#' ls = st_sfc(st_linestring(rbind(c(2,1), c(1,2), c(0,0), c(1,1))), crs = 4326)
#' s2ls = st_as_s2(ls)
st_as_s2.sfc_LINESTRING = function(x, ..., oriented = FALSE) {
	load_libs2()
	structure(lapply(x, libs2::s2MakePolyline), crs = st_crs(x), class = "S2Polyline")
}

#' @name s2
#' @export
#' @examples
#' mls = st_sfc(st_multilinestring(list(rbind(c(2,1), c(1,2)), rbind(c(0,0), c(1,1)))), crs = 4326)
#' try(st_as_s2(mls))
st_as_s2.sfc_MULTILINESTRING = function(x, ...) {
	stop("s2 does not have a MULTILINESTRING equivalent; st_cast to LINESTRING first?")
}

#' @name s2
#' @export
#' @examples
#' try(st_as_s2(st_multipoint(rbind(c(0,0), c(1,1)))))
st_as_s2.sfc = function(x, ...) {
	stop(paste("no st_as_s2 method available for objects of class", class(x)[1]))
}

#' @name s2
#' @export
st_as_s2.sf = function(x, ..., oriented = FALSE) {
	st_as_s2(st_geometry(x), ..., oriented = oriented)
}

#' @name s2
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
#' @param ... ignored
#' @examples
#' x = st_as_sfc(s2)
#' all.equal(st_area(sf), st_area(x))
st_as_sfc.S2Polygon = function(x, ...) {
	load_libs2()
	st_sfc(lapply(libs2::s2GetPolygon(x), st_multipolygon), crs = st_crs(x))
}

#' @name s2
#' @export
#' @examples
#' st_as_sfc(s2ls)
st_as_sfc.S2Polyline = function(x, ...) {
	load_libs2()
	st_sfc(lapply(libs2::s2GetPolyline(x), st_linestring), crs = st_crs(x))
}

#' @name s2
#' @export
#' @examples
#' st_as_sfc(s2pts)
st_as_sfc.S2Point = function(x, ...) {
	load_libs2()
	st_sfc(lapply(libs2::s2GetPoint(x), st_point), crs = st_crs(x))
}


#' @name s2
#' @param y object of class \code{S2Polygon}
#' @param sparse logical; see \link{st_intersects}
#' @export
st_intersects.S2Polygon = function(x, y = x, sparse = TRUE, ...) {
	load_libs2()
	stopifnot(st_crs(x) == st_crs(y), inherits(y, "S2Polygon"))
	sgbp(libs2::s2Intersects(x, y, TRUE), "intersects", 
			region.id = as.character(seq_along(x)), ncol = length(y),
			sparse)
}

#' @export
st_intersects.S2Polyline = function(x, y = x, sparse = TRUE, ...) {
	load_libs2()
	stopifnot(st_crs(x) == st_crs(y), inherits(y, "S2Polyline"))
	sgbp(libs2::s2Intersects(x, y, FALSE), "intersects", 
			region.id = as.character(seq_along(x)), ncol = length(y),
			sparse)
}

#' @name s2
#' @export
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' st_is_valid(st_as_s2(nc[1:10,]))
st_is_valid.S2Polygon = function(x, ...) {
	load_libs2()
	libs2::s2IsValid(x, TRUE)
}

#' @name s2
#' @export
#' @examples
#' st_is_valid(s2ls)
st_is_valid.S2Polyline = function(x, ...) {
	load_libs2()
	libs2::s2IsValid(x, FALSE)
}

#' @name s2
#' @param radius numeric or \code{units}; radius of the (spherical) Earth.
#' @export
#' @examples
#' st_area(st_as_s2(nc[1:10,]))
#' st_area(nc[1:10,])/st_area(st_as_s2(nc[1:10,]))
st_area.S2Polygon = function(x, ..., radius = units::set_units(6371008.8, "m", mode = "standard")) {
	load_libs2()
	libs2::s2GetArea(x) * (radius^2)
}
