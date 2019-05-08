
#' @name st_zbox
#' @param x object of class \code{zbox}
#' @export
is.na.zbox = function(x) identical(x, NA_zbox_)

zb_wrap = function(zb) {
	stopifnot(is.numeric(zb) && length(zb) == 2)
	structure(zb, names = c("zmin", "zmax"), class = "zbox")
}

zbox.Set = function(obj, ...) {
	sel = vapply(obj, function(x) { length(x) && !all(is.na(x)) }, TRUE)
	if (! any(sel))
		NA_zbox_
	else
		zb_wrap(CPL_get_zbox(unclass(obj)[sel], 0))
}
zbox.Mtrx = function(obj, ...) {
	if (length(obj) == 0)
		NA_zbox_
	else
		zb_wrap(CPL_get_zbox(list(obj), 1)) # note the list()
}
zbox.MtrxSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_zbox_
	else
		zb_wrap(CPL_get_zbox(obj, 1))
}
zbox.MtrxSetSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_zbox_
	else
		zb_wrap(CPL_get_zbox(obj, 2))
}
zbox.MtrxSetSetSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_zbox_
	else
		zb_wrap(CPL_get_zbox(obj, 3))
}

#' Return 'z' bounding of a simple feature or simple feature set
#'
#' Return 'z' bounding of a simple feature or simple feature set
#' @param obj object to compute the z-bounding box from
#' @param ... ignored
#' @export
#' @return a numeric vector of length two, with \code{zmin} and \code{zmax} values;
#' if \code{obj} is of class \code{sf} or \code{sfc} the object
#' returned has a class \code{zbox}, an attribute \code{crs} and a method to print the
#' zbox and an \code{st_crs} method to retrieve the coordinate reference system
#' corresponding to \code{obj} (and hence the bounding box). \link{st_as_sfc} has a
#' methods for \code{zbox} objects to generate a polygon around the four bounding box points.
#' @name st_zbox
#' @examples
#' a = st_sf(a = 1:2, geom = st_sfc(st_point(0:2), st_point(1:3)), crs = 4326)
#' st_zbox(a)
#' st_as_sfc(st_zbox(a))
st_zbox = function(obj, ...) UseMethod("st_zbox")

#' @export
#' @name st_zbox
st_zbox.POINT = function(obj, ...) zb_wrap(c(obj[3L], obj[3L]))
#' @export
#' @name st_zbox
st_zbox.MULTIPOINT = zbox.Mtrx
#' @export
#' @name st_zbox
st_zbox.LINESTRING = zbox.Mtrx
#' @export
#' @name st_zbox
st_zbox.POLYGON = zbox.MtrxSet
#' @export
#' @name st_zbox
st_zbox.MULTILINESTRING = zbox.MtrxSet
#' @export
#' @name st_zbox
st_zbox.MULTIPOLYGON = zbox.MtrxSetSet

zbox_list = function(obj, ...) {
	s = vapply(obj, st_zbox, c(0.,0.)) # dispatch on class
	if (length(s) == 0 || all(is.na(s[1L,])))
		NA_zbox_
	else
		zb_wrap(c(min(s[1L,], na.rm = TRUE), max(s[2L,], na.rm = TRUE)))
}

#' @name st_zbox
#' @export
st_zbox.GEOMETRYCOLLECTION = zbox_list

#' @name st_zbox
#' @export
st_zbox.MULTISURFACE = zbox_list

#' @name st_zbox
#' @export
st_zbox.MULTICURVE = zbox_list

#' @name st_zbox
#' @export
st_zbox.CURVEPOLYGON = zbox_list

#' @name st_zbox
#' @export
st_zbox.COMPOUNDCURVE = zbox_list

#' @name st_zbox
#' @export
st_zbox.POLYHEDRALSURFACE = zbox.MtrxSetSet

#' @name st_zbox
#' @export
st_zbox.TIN = zbox.MtrxSetSet

#' @name st_zbox
#' @export
st_zbox.TRIANGLE = zbox.MtrxSet

#' @name st_zbox
#' @export
st_zbox.CIRCULARSTRING = function(obj, ...) {
	# this is of course wrong:
	st_zbox(st_cast(obj, "LINESTRING"))
}

#' @export
print.zbox = function(x, ...) {
	x = structure(x, crs = NULL, class = NULL)
	print(set_units(x, attr(x, "units"), mode = "standard"))
}

compute_zbox = function(obj) {
	switch(class(obj)[1],
		   sfc_POINT = zb_wrap(zbox.Set(obj)),
		   sfc_MULTIPOINT = zb_wrap(zbox.MtrxSet(obj)),
		   sfc_LINESTRING = zb_wrap(zbox.MtrxSet(obj)),
		   sfc_POLYGON = zb_wrap(zbox.MtrxSetSet(obj)),
		   sfc_MULTILINESTRING = zb_wrap(zbox.MtrxSetSet(obj)),
		   sfc_MULTIPOLYGON = zb_wrap(zbox.MtrxSetSetSet(obj)),
		   zbox_list(obj)
	)
}

#' @name st_zbox
#' @export
st_zbox.sfc = function(obj, ...) structure(attr(obj, "zbox"), crs = st_crs(obj))

#' @name st_zbox
#' @export
st_zbox.sf = function(obj, ...) st_zbox(st_geometry(obj))

#' @name st_zbox
#' @param crs object of class \code{crs}, or argument to \link{st_crs}, specifying the CRS of this bounding box.
#' @examples
#' st_zbox(c(zmin = 16.1, zmax = 16.6), crs = st_crs(4326))
#' @export
st_zbox.numeric = function(obj, ..., crs = NA_crs_) {
	structure(zb_wrap(obj[c("zmin", "zmax")]), crs = st_crs(crs))
}

#' @export
st_zbox.zbox = function(obj, ...) obj


# #' @export
# "$.zbox" = function(x, name) {
# 	switch(name,
# 		   xrange =,
# 		   xlim = x[c("zmin", "xmax")],
# 		   yrange =,
# 		   ylim = x[c("zmax", "ymax")],
# 		   zmin = x["zmin"],
# 		   zmax = x["zmax"],
# 		   xmax = x["xmax"],
# 		   ymax = x["ymax"],
# 		   stop("unsupported name")
# 	)
# }

#' @name st_zbox
#' @details \code{NA_zbox_} represents the missing value for a \code{zbox} object
#' @export
NA_zbox_ = structure(rep(NA_real_, 2),
					 names = c("zmin", "zmax"),
					 crs = NA_crs_,
					 class = "zbox")
