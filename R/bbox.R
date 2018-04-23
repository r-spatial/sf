
#' @name st_bbox
#' @param x object of class \code{bbox}
#' @export
is.na.bbox = function(x) identical(x, NA_bbox_)

bb_wrap = function(bb) {
	stopifnot(is.numeric(bb) && length(bb) == 4)
	structure(bb, names = c("xmin", "ymin", "xmax", "ymax"), class = "bbox")
}

bbox.Set = function(obj, ...) {
	sel = vapply(obj, function(x) length(x) && !all(is.na(x)), TRUE)
	if (! any(sel))
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(unclass(obj)[sel], 0))
}
bbox.Mtrx = function(obj, ...) {
	if (length(obj) == 0)
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(list(obj), 1)) # note the list()
}
bbox.MtrxSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(obj, 1))
}
bbox.MtrxSetSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(obj, 2))
}
bbox.MtrxSetSetSet = function(obj, ...) {
	if (length(obj) == 0)
		NA_bbox_
	else
		bb_wrap(CPL_get_bbox(obj, 3))
}

#' Return bounding of a simple feature or simple feature set
#'
#' Return bounding of a simple feature or simple feature set
#' @param obj object to compute the bounding box from
#' @param ... ignored
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
st_bbox = function(obj, ...) UseMethod("st_bbox")

#' @export
#' @name st_bbox
st_bbox.POINT = function(obj, ...) bb_wrap(c(obj[1L], obj[2L], obj[1L], obj[2L]))
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

bbox_list = function(obj, ...) {
	s = vapply(obj, st_bbox, c(0.,0.,0.,0.)) # dispatch on class
	if (length(s) == 0 || all(is.na(s[1L,])))
		NA_bbox_
	else
		bb_wrap(c(min(s[1L,], na.rm = TRUE), min(s[2L,], na.rm = TRUE),
		  max(s[3L,], na.rm = TRUE), max(s[4L,], na.rm = TRUE)))
}

#' @name st_bbox
#' @export
st_bbox.GEOMETRYCOLLECTION = bbox_list

#' @name st_bbox
#' @export
st_bbox.MULTISURFACE = bbox_list

#' @name st_bbox
#' @export
st_bbox.MULTICURVE = bbox_list

#' @name st_bbox
#' @export
st_bbox.CURVEPOLYGON = bbox_list

#' @name st_bbox
#' @export
st_bbox.COMPOUNDCURVE = bbox_list

#' @name st_bbox
#' @export
st_bbox.POLYHEDRALSURFACE = bbox.MtrxSetSet

#' @name st_bbox
#' @export
st_bbox.TIN = bbox.MtrxSetSet

#' @name st_bbox
#' @export
st_bbox.TRIANGLE = bbox.MtrxSet

#' @name st_bbox
#' @export
st_bbox.CIRCULARSTRING = function(obj, ...) {
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

#' @name st_bbox
#' @export
st_bbox.sfc = function(obj, ...) structure(attr(obj, "bbox"), crs = st_crs(obj))

#' @name st_bbox
#' @export
st_bbox.sf = function(obj, ...) st_bbox(st_geometry(obj))

#' @name st_bbox
#' @export
st_bbox.Spatial = function(obj, ...) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	bb = sp::bbox(obj)
	structure(bb_wrap(c(bb[1,1],bb[2,1],bb[1,2],bb[2,2])),
		crs = st_crs(sp::proj4string(obj)))
}

#' @name st_bbox
#' @export
st_bbox.Raster = function(obj, ...) {
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	if (!requireNamespace("raster", quietly = TRUE))
		stop("package raster required, please install it first")
	bb = sp::bbox(obj)
	structure(bb_wrap(c(bb[1,1],bb[2,1],bb[1,2],bb[2,2])),
		crs = st_crs(sp::proj4string(obj)))
}

#' @name st_bbox
#' @export
st_bbox.Extent = function(obj, ...) {
	if (!requireNamespace("raster", quietly = TRUE))
		stop("package raster required, please install it first")
	structure(bb_wrap(c(obj@xmin, obj@ymin, obj@xmax, obj@ymax)), crs = NA_crs_)
}

#' @name st_bbox
#' @param crs object of class \code{crs}, or argument to \link{st_crs}, specifying the CRS of this bounding box.
#' @examples
#' st_bbox(c(xmin = 16.1, xmax = 16.6, ymax = 48.6, ymin = 47.9), crs = st_crs(4326))
#' @export
st_bbox.numeric = function(obj, ..., crs = NA_crs_) {
	structure(bb_wrap(obj[c("xmin", "ymin", "xmax", "ymax")]), crs = st_crs(crs))
}

#' @export
"$.bbox" = function(x, name) {
	switch(name,
		xrange =,
		xlim = x[c("xmin", "xmax")],
		yrange =,
		ylim = x[c("ymin", "ymax")],
		xmin = x["xmin"],
		ymin = x["ymin"],
		xmax = x["xmax"],
		ymax = x["ymax"],
		stop("unsupported name")
	)
}

#' @name st_bbox
#' @details \code{NA_bbox_} represents the missing value for a \code{bbox} object
#' @export
NA_bbox_ = structure(rep(NA_real_, 4),
	names = c("xmin", "ymin", "xmax", "ymax"),
	crs = NA_crs_,
	class = "bbox")
