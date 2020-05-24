# see https://docs.google.com/presentation/d/1Hl4KapfAENAOf4gv-pSngKwvS_jwNVHRPZTTDzXXn6Q/view?pli=1#slide=id.i0
# and the libs2 package:
# https://github.com/r-spatial/libs2

load_libs2 = function() {
	if (! requireNamespace("libs2", quietly = TRUE))
		stop('package libs2 required, please install it first with remotes::install_github("r-spatial/libs2")')
}


# from libs2 to sf:
#' @export
#' @name s2
st_as_sfc.wk_wkb = function(x, ..., crs = NA_crs_) {
	class(x) = "WKB"
	st_set_crs(st_as_sfc(x, ...), crs)
}

st_as_sfc_s2 = function(x, ...) {
	load_libs2()
	st_as_sfc(libs2::as_wkb(x), ...)
}

#' @name s2
#' @param x object of the respective class
#' @param ... passed on; might contain named argument crs
#' @export
st_as_sfc.s2latlng = st_as_sfc_s2

#' @name s2
#' @export
st_as_sfc.s2polyline = st_as_sfc_s2

#' @name s2
#' @export
st_as_sfc.s2polygon = function(x, ...) {
	load_libs2()
	st_cast(st_as_sfc(libs2::as_wkb(x), ...))
}

#' @name s2
#' @export
st_as_sfc.s2geography = function(x, ...) {
	load_libs2()
	st_cast(st_as_sfc(libs2::s2_asbinary(x, ...)))
}

# from sf to libs2:
#' @name s2
s2latlng.sfc = function(x, ...) {
	load_libs2()
	libs2::s2latlng(structure(st_as_binary(x,...), class = "wk_wkb"))
}

#' @name s2
s2polyline.sfc = function(x, ...) {
	load_libs2()
	libs2::s2polyline(structure(st_as_binary(x,...), class = "wk_wkb"))
}

#' @name s2
#' @param omit_poles numeric; see \link[libs2]{s2polygon}
s2polygon.sfc = function(x, ..., oriented = FALSE, check = TRUE, omit_poles = 0.0) {
	load_libs2()
	libs2::s2polygon(structure(st_as_binary(x,...), class = "wk_wkb"), 
		oriented = oriented, check = check, omit_poles = omit_poles)
}

register_libs2_methods = function() {
	register_s3_method("libs2", "s2latlng", "sfc")
	register_s3_method("libs2", "s2polyline", "sfc")
	register_s3_method("libs2", "s2polygon", "sfc")
}

##
## OLD:
##



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
#' @param oriented logical, see \link[libs2]{s2polygon}
#' @param check logical, see \link[libs2]{s2polygon}
#' @export
st_as_s2.sfc_MULTIPOLYGON = function(x, ..., oriented = FALSE, check = TRUE, omit_poles = 0.0) {
	if (!st_is_longlat(x))
		stop("st_as_s2 needs geographic coordinates; consider using st_transform to EPSG:4326")
	ret = libs2::s2polygon(x, oriented = oriented, check = check, omit_poles = omit_poles)
	structure(ret, crs = st_crs(x), class = c("S2Polygon", class(ret)))
}

#' @name s2
#' @export
st_as_s2.sfc_POLYGON = function(x, ..., oriented = FALSE, check = TRUE, omit_poles = 0.0) {
	st_as_s2(st_cast(x, "MULTIPOLYGON"), ..., oriented = oriented, check = check, omit_poles = omit_poles)
}

#' @name s2
#' @export
#' @examples
#' s2pts = st_as_s2(st_sfc(st_point(c(0,0)), st_point(c(2,2))), crs = 4326)
st_as_s2.sfc_POINT = function(x, ..., oriented = FALSE) {
	ret = libs2::s2latlng(x)
	structure(ret, crs = st_crs(x), class = c("S2Point", class(ret)))
}

#' @name s2
#' @export
#' @examples
#' ls = st_sfc(st_linestring(rbind(c(2,1), c(1,2), c(0,0), c(1,1))), crs = 4326)
#' s2ls = st_as_s2(ls)
st_as_s2.sfc_LINESTRING = function(x, ..., oriented = FALSE) {
	ret = libs2::s2polyline(x)
	structure(ret, crs = st_crs(x), class = c("S2Polyline", class(ret)))
}

#' @name s2
#' @export
#' @examples
#' mls = st_sfc(st_multilinestring(list(rbind(c(2,1), c(1,2)), rbind(c(0,0), c(1,1)))), crs = 4326)
#' try(st_as_s2(mls))
st_as_s2.sfc_MULTILINESTRING = function(x, ...) {
	stop("s2 does not have a MULTILINESTRING equivalent; st_cast to LINESTRING first?")
}
st_as_s2.sfc_MULTIPOINT = function(x, ...) {
	stop("s2 does not have a MULTIPOINT equivalent; st_cast to POINT first?")
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
st_as_s2.sf = function(x, ..., oriented = FALSE, check = TRUE) {
	st_as_s2(st_geometry(x), ..., oriented = oriented, check = check)
}

#' @name s2
#' @param crs object of class \code{crs}
#' @export
st_as_s2.sfg = function(x, ..., oriented = FALSE, check = TRUE, crs = st_crs('EPSG:4326')) {
	st_as_s2(st_geometry(x, crs = crs), ..., oriented = oriented, check = check)
}
st_as_sfc.S2Polygon = function(x, ...) {
	st_set_crs(NextMethod(), st_crs(x))
}

#' @name s2
#' @export
#' @examples
#' st_as_sfc(s2ls)
st_as_sfc.S2Polyline = function(x, ...) {
	st_set_crs(NextMethod(), st_crs(x))
}

#' @name s2
#' @export
#' @examples
#' st_as_sfc(s2pts)
st_as_sfc.S2Point = function(x, ...) {
	st_set_crs(NextMethod(), st_crs(x))
}


#' @name s2
#' @param y object of class \code{S2Polygon}
#' @param sparse logical; see \link{st_intersects}
#' @export
st_intersects.S2Polygon = function(x, y = x, sparse = TRUE, ...) {
	load_libs2()
	stopifnot(st_crs(x) == st_crs(y), inherits(y, "S2Polygon"))
	sgbp(libs2::s2polygon_intersects(x, y), "intersects", 
			region.id = as.character(seq_along(x)), ncol = length(y),
			sparse)
}

#' @export
st_intersects.S2Polyline = function(x, y = x, sparse = TRUE, ...) {
	load_libs2()
	stopifnot(st_crs(x) == st_crs(y), inherits(y, "S2Polyline"))
	sgbp(libs2::s2polyline_intersects(x, y), "intersects", 
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
	libs2::s2polygon_is_valid(x)
}

#' @name s2
#' @export
#' @examples
#' st_is_valid(s2ls)
st_is_valid.S2Polyline = function(x, ...) {
	load_libs2()
	libs2::s2polyline_is_valid(x)
}

#' @name s2
#' @param radius numeric or \code{units}; radius of the (spherical) Earth.
#' @export
#' @examples
#' st_area(st_as_s2(nc[1:10,]))
#' st_area(nc[1:10,])/st_area(st_as_s2(nc[1:10,]))
st_area.S2Polygon = function(x, ..., radius = units::set_units(6371008.8, "m", mode = "standard")) {
	load_libs2()
	libs2::s2polygon_areas(x) * (radius^2)
}
