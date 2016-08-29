bb = function(xmin, ymin, xmax, ymax) {
	c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
	# or return matrix instead, compatible to sp::bbox?
}

bbox.Mtrx = function(obj) {
	mn = apply(obj, 2, min)
	mx = apply(obj, 2, max)
	bb(xmin = mn[1], ymin = mn[2], xmax = mx[1], ymax = mx[2])
}
bbox.MtrxSet = function(obj) {
	s = sapply(obj, bbox.Mtrx)
	bb(xmin = min(s[1,]), ymin = min(s[2,]), xmax = max(s[3,]), ymax = max(s[4,]))
}
bbox.MtrxSetSet = function(obj) {
	s = sapply(obj, bbox.MtrxSet)
	bb(xmin = min(s[1,]), ymin = min(s[2,]), xmax = max(s[3,]), ymax = max(s[4,]))
}
bbox.MtrxSetSetSet = function(obj) {
	s = sapply(obj, bbox.MtrxSetSet)
	bb(xmin = min(s[1,]), ymin = min(s[2,]), xmax = max(s[3,]), ymax = max(s[4,]))
}

##' Return bounding of a simple feature or simple feature set
##'
##' Return bounding of a simple feature or simple feature set
##' @param obj object to compute the bounding box from
##' @export
st_bbox = function(obj) UseMethod("st_bbox")

#' @export
st_bbox.POINT = function(obj) bb(xmin = obj[1], ymin = obj[2], xmax = obj[1], ymax = obj[2])
#' @export
st_bbox.MULTIPOINT = bbox.Mtrx
#' @export
st_bbox.LINESTRING = bbox.Mtrx
#' @export
st_bbox.POLYGON = bbox.MtrxSet
#' @export
st_bbox.MULTILINESTRING = bbox.MtrxSet
#' @export
st_bbox.MULTIPOLYGON = bbox.MtrxSetSet
#' @export
st_bbox.GEOMETRYCOLLECTION = function(obj) {
	s = sapply(obj, st_bbox) # dispatch on class
	bb(xmin = min(s[1,]), ymin = min(s[2,]), xmax = max(s[3,]), ymax = max(s[4,]))
}

#' @export
st_bbox.sfc_POINT = function(obj) bbox.Mtrx(do.call(rbind, obj))
#' @export
st_bbox.sfc_MULTIPOINT = bbox.MtrxSet
#' @export
st_bbox.sfc_LINESTRING = bbox.MtrxSet
#' @export
st_bbox.sfc_POLYGON = bbox.MtrxSetSet
#' @export
st_bbox.sfc_MULTILINESTRING = bbox.MtrxSetSet
#' @export
st_bbox.sfc_MULTIPOLYGON = bbox.MtrxSetSetSet
#' @export
st_bbox.sfc_GEOMETRYCOLLECTION = st_bbox.GEOMETRYCOLLECTION
#' @export
st_bbox.sf = function(obj) st_bbox(st_geometry(obj))
