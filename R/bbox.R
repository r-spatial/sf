#' @name st_bbox
#' @details
#' \code{NA_bbox_} is the \code{bbox} object with a missing value.
#' @export
NA_bbox_ = structure(rep(NA_real_, 4),
	names = c("xmin", "ymin", "xmax", "ymax"),
	crs = NA_crs_,
	class = "bbox")

#' @export
is.na.bbox = function(x) identical(x, NA_bbox_)

bb_wrap = function(bb) {
	stopifnot(is.numeric(bb) && length(bb) == 4)
	structure(bb, names = c("xmin", "ymin", "xmax", "ymax"), class = "bbox")
}

bbox.Set = function(obj) {
	sel = vapply(obj, function(x) length(x) && !all(is.na(x)), TRUE)
	if (! any(sel))
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(unclass(obj)[sel], 0))
}
bbox.Mtrx = function(obj) {
	if (length(obj) == 0)
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(list(obj), 1)) # note the list()
}
bbox.MtrxSet = function(obj) {
	if (length(obj) == 0)
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(obj, 1))
}
bbox.MtrxSetSet = function(obj) {
	if (length(obj) == 0)
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(obj, 2))
}
bbox.MtrxSetSetSet = function(obj) {
	if (length(obj) == 0)
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(obj, 3))
}

#' Return bounding of a simple feature or simple feature set
#'
#' Return bounding of a simple feature or simple feature set
#' @param obj object to compute the bounding box from
#' @export
#' @return a numeric vector of length four, with \code{xmin}, \code{ymin}, \code{xmax}
#' and \code{ymax} values; if \code{obj} is of class \code{sf}, \code{sfc}, \code{Spatial} or \code{Raster}, the object
#' returned has a class \code{bbox}, an attribute \code{crs} and a method to print the
#' bbox and an \code{st_crs} method to retrieve the coordinate reference system
#' corresponding to \code{obj} (and hence the bounding box). \link{st_as_sfc} has a
#' methods for \code{bbox} objects to generate a polygon around the four bounding box points.
#' @name st_bbox
#' @examples
#' a = st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_point(1:2)), crs = 4326)
#' st_bbox(a)
#' st_as_sfc(st_bbox(a))
st_bbox = function(obj) UseMethod("st_bbox")

#' @export
#' @name st_bbox
st_bbox.POINT = function(obj) bb_wrap(c(obj[1L], obj[2L], obj[1L], obj[2L]))
#' @export
#' @name st_bbox
st_bbox.MULTIPOINT = bbox.Mtrx
#' @export
#' @name st_bbox
st_bbox.LINESTRING = bbox.Mtrx
#' @export
#' @name st_bbox
st_bbox.POLYGON = bbox.MtrxSet
#' @export
#' @name st_bbox
st_bbox.MULTILINESTRING = bbox.MtrxSet
#' @export
#' @name st_bbox
st_bbox.MULTIPOLYGON = bbox.MtrxSetSet

bbox_list = function(obj) {
	s = vapply(obj, st_bbox, c(0.,0.,0.,0.)) # dispatch on class
	if (length(s) == 0 || all(is.na(s[1L,])))
		NA_bbox_
	else
		bb_wrap(c(min(s[1L,], na.rm = TRUE), min(s[2L,], na.rm = TRUE),
		  max(s[3L,], na.rm = TRUE), max(s[4L,], na.rm = TRUE)))
}
#' @export
#' @name st_bbox
st_bbox.GEOMETRYCOLLECTION = bbox_list
#' @export
#' @name st_bbox
st_bbox.MULTISURFACE = bbox_list

#' @export
#' @name st_bbox
st_bbox.MULTICURVE = bbox_list
#' @export
#' @name st_bbox
st_bbox.CURVEPOLYGON = bbox_list
#' @export
#' @name st_bbox
st_bbox.COMPOUNDCURVE = bbox_list
#' @export
#' @name st_bbox
st_bbox.POLYHEDRALSURFACE = bbox.MtrxSetSet
#' @export
#' @name st_bbox
st_bbox.TIN = bbox.MtrxSetSet
#' @export
#' @name st_bbox
st_bbox.TRIANGLE = bbox.MtrxSet

#' @export
#' @name st_bbox
st_bbox.CIRCULARSTRING = function(obj) {
	# this is of course wrong:
	st_bbox(st_cast(obj, "LINESTRING"))
}

#' @export
print.bbox = function(x, ...) {
	attr(x, "crs") = NULL
	print(unclass(x))
}

compute_bbox = function(obj) {
	switch(class(obj)[1],
		sfc_POINT = bb_wrap(bbox.Set(obj)),
		sfc_MULTIPOINT = bb_wrap(bbox.MtrxSet(obj)),
		sfc_LINESTRING = bb_wrap(bbox.MtrxSet(obj)),
		sfc_POLYGON = bb_wrap(bbox.MtrxSetSet(obj)),
		sfc_MULTILINESTRING = bb_wrap(bbox.MtrxSetSet(obj)),
		sfc_MULTIPOLYGON = bb_wrap(bbox.MtrxSetSetSet(obj)),
		bbox_list(obj)
	)
}

#' @export
#' @name st_bbox
st_bbox.sfc = function(obj) structure(attr(obj, "bbox"), crs = st_crs(obj))

#' @export
#' @name st_bbox
st_bbox.sf = function(obj) st_bbox(st_geometry(obj))

#' @export
st_bbox.Spatial = function(obj) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	bb = sp::bbox(obj)
	structure(bb_wrap(c(bb[1,1],bb[2,1],bb[1,2],bb[2,2])),
		crs = st_crs(sp::proj4string(obj)))
}

#' @export
st_bbox.Raster = function(obj) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	if (!requireNamespace("raster", quietly = TRUE))
		stop("package raster required, please install it first")
	bb = sp::bbox(obj)
	structure(bb_wrap(c(bb[1,1],bb[2,1],bb[1,2],bb[2,2])),
		crs = st_crs(sp::proj4string(obj)))
}
