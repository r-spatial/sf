bbox.Mtrx = function(obj) {
	# r = apply(obj, 2L, range) # min row 1, max row 2
	# c(xmin = r[1L,1L], ymin = r[1L,2L], xmax = r[2L,1L], ymax = r[2L,2L])
	if (length(obj) == 0) 
		structure(rep(NA_real_, 4), names = c("xmin", "ymin", "xmax", "ymax")) 
	else
		structure(CPL_get_bbox(list(obj), 1), names = c("xmin", "ymin", "xmax", "ymax"))
}
bbox.MtrxSet = function(obj) {
	#s = sapply(obj, bbox.Mtrx)
	#c(xmin = min(s[1L,]), ymin = min(s[2L,]), xmax = max(s[3L,]), ymax = max(s[4L,]))
	if (length(obj) == 0) 
		structure(rep(NA_real_, 4), names = c("xmin", "ymin", "xmax", "ymax")) 
	else
		structure(CPL_get_bbox(obj, 1), names = c("xmin", "ymin", "xmax", "ymax"))
}
bbox.MtrxSetSet = function(obj) {
	#s = sapply(obj, bbox.MtrxSet)
	#c(xmin = min(s[1L,]), ymin = min(s[2L,]), xmax = max(s[3L,]), ymax = max(s[4L,]))
	if (length(obj) == 0) 
		structure(rep(NA_real_, 4), names = c("xmin", "ymin", "xmax", "ymax")) 
	else
		structure(CPL_get_bbox(obj, 2), names = c("xmin", "ymin", "xmax", "ymax"))
}
bbox.MtrxSetSetSet = function(obj) {
	#s = sapply(obj, bbox.MtrxSetSet)
	#c(xmin = min(s[1L,]), ymin = min(s[2L,]), xmax = max(s[3L,]), ymax = max(s[4L,]))
	if (length(obj) == 0) 
		structure(rep(NA_real_, 4), names = c("xmin", "ymin", "xmax", "ymax")) 
	else
		structure(CPL_get_bbox(obj, 3), names = c("xmin", "ymin", "xmax", "ymax"))
}

#' Return bounding of a simple feature or simple feature set
#'
#' Return bounding of a simple feature or simple feature set
#' @param obj object to compute the bounding box from
#' @export
#' @return a numeric vector of length four, with \code{xmin}, \code{ymin}, \code{xmax}
#' and \code{ymax} values; if \code{obj} is of class \code{sf} or \code{sfc}, the object
#' returned has a class \code{bbox}, an attribute \code{crs} and a method to print the
#' bbox and an \code{st_crs} method to retrieve the coordinate reference system 
#' corresponding to \code{obj} (and hence the bounding box).
#' @name st_bbox
st_bbox = function(obj) UseMethod("st_bbox")

#' @export
#' @name st_bbox
st_bbox.POINT = function(obj) c(xmin = obj[1L], ymin = obj[2L], xmax = obj[1L], ymax = obj[2L])
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
#' @export
#' @name st_bbox
st_bbox.GEOMETRYCOLLECTION = function(obj) {
	s = sapply(obj, st_bbox) # dispatch on class
	if (length(s) == 0 || all(is.na(s[1L,])))
		structure(rep(NA_real_, 4), names = c("xmin", "ymin", "xmax", "ymax")) 
	else
		c(xmin = min(s[1L,], na.rm = TRUE), ymin = min(s[2L,], na.rm = TRUE), 
		  xmax = max(s[3L,], na.rm = TRUE), ymax = max(s[4L,], na.rm = TRUE))
}

bb_wrap = function(bb, x) {
	structure(bb, class = "bbox", crs = st_crs(x))
}

#' @export
print.bbox = function(x, ...) {
	attr(x, "crs") = NULL
	x = unclass(x)
	NextMethod()
}

#' @export
#' @name st_bbox
st_bbox.sfc_POINT = function(obj) {
	sel = sapply(obj, function(x) length(x) > 0) 
	ret = if (! any(sel))
		structure(rep(NA_real_, 4), names = c("xmin", "ymin", "xmax", "ymax")) 
	else
		structure(CPL_get_bbox(unclass(obj)[sel], 0), names = c("xmin", "ymin", "xmax", "ymax")) 
	bb_wrap(ret, obj)
}

#' @export
#' @name st_bbox
st_bbox.sfc_MULTIPOINT = function(obj) bb_wrap(bbox.MtrxSet(obj), obj)
#' @export
#' @name st_bbox
st_bbox.sfc_LINESTRING = function(obj) bb_wrap(bbox.MtrxSet(obj), obj)
#' @export
#' @name st_bbox
st_bbox.sfc_POLYGON = function(obj) bb_wrap(bbox.MtrxSetSet(obj), obj)
#' @export
#' @name st_bbox
st_bbox.sfc_MULTILINESTRING = function(obj) bb_wrap(bbox.MtrxSetSet(obj), obj)
#' @export
#' @name st_bbox
st_bbox.sfc_MULTIPOLYGON = function(obj) bb_wrap(bbox.MtrxSetSetSet(obj), obj)
#' @export
#' @name st_bbox
st_bbox.sfc_GEOMETRYCOLLECTION = function(obj) bb_wrap(st_bbox.GEOMETRYCOLLECTION(obj), obj)
#' @export
#' @name st_bbox
st_bbox.sfc_GEOMETRY = function(obj) bb_wrap(st_bbox.GEOMETRYCOLLECTION(obj), obj)
#' @export
#' @name st_bbox
st_bbox.sfc = function(obj) bb_wrap(structure(rep(NA_real_, 4), names = c("xmin", "ymin", "xmax", "ymax")), obj)  # nocov
#' @export
#' @name st_bbox
st_bbox.sf = function(obj) st_bbox(st_geometry(obj))
