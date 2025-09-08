
#' @name st_z_range
#' @param x object of class \code{z_range}
#' @export
is.na.z_range = function(x) identical(x, NA_z_range_)

zb_wrap = function(zb) {
	stopifnot(is.numeric(zb), length(zb) == 2)
	structure(zb, names = c("zmin", "zmax"), class = "z_range")
}

z_range.pointmatrix = function(obj, ...) {
	zb_wrap(range(obj[,3]))
}

z_range.Set = function(obj, ...) {
	sel = vapply(obj, function(x) { length(x) && !all(is.na(x)) }, TRUE)
	if (! any(sel))
		NA_z_range_
	else
		zb_wrap(CPL_get_z_range(unclass(obj)[sel], 0))
}
z_range.Mtrx = function(obj, ...) {
	if (length(obj) == 0)
		NA_z_range_
	else
		zb_wrap(CPL_get_z_range(list(obj), 1)) # note the list()
}
z_range.MtrxSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_z_range_
	else
		zb_wrap(CPL_get_z_range(obj, 1))
}
z_range.MtrxSetSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_z_range_
	else
		zb_wrap(CPL_get_z_range(obj, 2))
}
z_range.MtrxSetSetSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_z_range_
	else
		zb_wrap(CPL_get_z_range(obj, 3))
}

#' Return 'z' range of a simple feature or simple feature set
#'
#' Return 'z' range of a simple feature or simple feature set
#' @param obj object to compute the z range from
#' @param ... ignored
#' @export
#' @return a numeric vector of length two, with \code{zmin} and \code{zmax} values;
#' if \code{obj} is of class \code{sf} or \code{sfc} the object
#' returned has a class \code{z_range}
#' @name st_z_range
#' @examples
#' a = st_sf(a = 1:2, geom = st_sfc(st_point(0:2), st_point(1:3)), crs = 4326)
#' st_z_range(a)
st_z_range = function(obj, ...) UseMethod("st_z_range")

#' @export
#' @name st_z_range
st_z_range.POINT = function(obj, ...) zb_wrap(c(obj[3L], obj[3L]))
#' @export
#' @name st_z_range
st_z_range.MULTIPOINT = z_range.Mtrx
#' @export
#' @name st_z_range
st_z_range.LINESTRING = z_range.Mtrx
#' @export
#' @name st_z_range
st_z_range.POLYGON = z_range.MtrxSet
#' @export
#' @name st_z_range
st_z_range.MULTILINESTRING = z_range.MtrxSet
#' @export
#' @name st_z_range
st_z_range.MULTIPOLYGON = z_range.MtrxSetSet

z_range_list = function(obj, ...) {
	s = vapply(obj, st_z_range, c(0.,0.)) # dispatch on class
	if (length(s) == 0 || all(is.na(s[1L,])))
		NA_z_range_
	else
		zb_wrap(c(min(s[1L,], na.rm = TRUE), max(s[2L,], na.rm = TRUE)))
}

#' @name st_z_range
#' @export
st_z_range.GEOMETRYCOLLECTION = z_range_list

#' @name st_z_range
#' @export
st_z_range.MULTISURFACE = z_range_list

#' @name st_z_range
#' @export
st_z_range.MULTICURVE = z_range_list

#' @name st_z_range
#' @export
st_z_range.CURVEPOLYGON = z_range_list

#' @name st_z_range
#' @export
st_z_range.COMPOUNDCURVE = z_range_list

#' @name st_z_range
#' @export
st_z_range.POLYHEDRALSURFACE = z_range.MtrxSetSet

#' @name st_z_range
#' @export
st_z_range.TIN = z_range.MtrxSetSet

#' @name st_z_range
#' @export
st_z_range.TRIANGLE = z_range.MtrxSet

#' @name st_z_range
#' @export
st_z_range.CIRCULARSTRING = function(obj, ...) {
	# this is of course wrong:
	st_z_range(st_cast(obj, "LINESTRING")) # nocov
}

#' @export
print.z_range = function(x, ...) {
	x = structure(x, crs = NULL, class = NULL) # nocov
	print(set_units(x, attr(x, "units"), mode = "standard")) # nocov
}

compute_z_range = function(obj) {
	if (!is.null(pts <- attr(obj, "points")))
		z_range.pointmatrix(pts)
	else switch(class(obj)[1],
		   sfc_POINT = zb_wrap(z_range.Set(obj)),
		   sfc_MULTIPOINT = zb_wrap(z_range.MtrxSet(obj)),
		   sfc_LINESTRING = zb_wrap(z_range.MtrxSet(obj)),
		   sfc_POLYGON = zb_wrap(z_range.MtrxSetSet(obj)),
		   sfc_MULTILINESTRING = zb_wrap(z_range.MtrxSetSet(obj)),
		   sfc_MULTIPOLYGON = zb_wrap(z_range.MtrxSetSetSet(obj)),
		   z_range_list(obj)
	)
}

#' @name st_z_range
#' @export
st_z_range.sfc = function(obj, ...) {
	a = attr(obj, "z_range")
	if(is.null(a)) return( NULL ) ## TODO return null?
	structure(a, crs = st_crs(obj))
}
#' @name st_z_range
#' @export
st_z_range.sf = function(obj, ...) st_z_range(st_geometry(obj))

#' @name st_z_range
#' @param crs object of class \code{crs}, or argument to \link{st_crs}, specifying the CRS of this bounding box.
#' @examples
#' st_z_range(c(zmin = 16.1, zmax = 16.6), crs = st_crs(4326))
#' @export
st_z_range.numeric = function(obj, ..., crs = NA_crs_) {
	structure(zb_wrap(obj[c("zmin", "zmax")]), crs = st_crs(crs)) # nocov
}

#' @export
st_z_range.z_range = function(obj, ...) obj # nocov


#' @export
"$.z_range" = function(x, name) { # nocov start
	switch(name,
		   zmin = x["zmin"],
		   zmax = x["zmax"],
		   stop("unsupported name")
	)
} # nocov end

#' @name st_z_range
#' @details \code{NA_z_range_} represents the missing value for a \code{z_range} object
#' @export
NA_z_range_ = structure(rep(NA_real_, 2),
					 names = c("zmin", "zmax"),
					 crs = NA_crs_,
					 class = "z_range")
