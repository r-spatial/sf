# third: what does the third dimension, if present, refer to? (XYZ or XYM)
getClassDim = function(x, d, third = "XYZ", type) {
	stopifnot(d > 1)
	type = toupper(type)
	if (d == 2)
		c("XY", type, "sfi")
	else if (d == 3) {
		stopifnot(third %in% c("XYZ", "XYM"))
		c(third, type, "sfi")
	} else if (d == 4)
		c("XYZM", type, "sfi")
	else stop(paste(d, "is an illegal number of columns for a", type))
}

Mtrx = function(x, third = "XYZ", type) {
	stopifnot(is.matrix(x) && is.numeric(x))
	structure(x, class = getClassDim(x, ncol(x), third, type))
}
MtrxSet = function(x, third = "XYZ", type, needClosed = FALSE) {
	stopifnot(is.list(x))
	nc = unique(sapply(x, ncol))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) any(head(y, 1) != tail(y, 1))
	if (needClosed && any(sapply(x, NotClosed)))
		stop("polygons not (all) closed")
	structure(x, class = getClassDim(x, ncol(x[[1]]), third, type))
}
MtrxSetSet = function(x, third = "XYZ", type, needClosed = FALSE) {
	stopifnot(is.list(x) && all(sapply(x, is.list)))
	nc = unique(unlist(lapply(x, function(y) sapply(y, ncol))))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) any(head(y, 1) != tail(y, 1))
	if (needClosed && any(unlist(sapply(x, function(y) sapply(y, NotClosed)))))
		stop("polygons not (all) closed")
	structure(x, class = getClassDim(x, ncol(x[[1]][[1]]), third, type))
}

#return "XY", "XYZ", "XYM", or "XYZM"
Dimension = function(x) { 
	stopifnot(inherits(x, "sfi"))
	class(x)[1]
}

#' Create a point simple feature from a numeric vector
#' 
#' Create a point simple feature from a numeric vector
#' @param x for \code{st_point}, numeric vector (or one-row-matrix) of length 2, 3 or 4; for \code{st_linestring} and \code{st_multipoint}, numeric matrix with points in rows; for \code{st_polygon} and \code{st_multilinestring}, list with numeric matrices with points in rows; for \code{st_multipolygon}, list of lists with numeric matrices; for \code{st_geometrycollection} list with (non-geometrycollection) simple feature objects
#' @param third character, indicating what a 3-dimensional point is ("XYZ" or "XYM"); see details
#' @name st
#' @details "XYZ" refers to coordinates where the third dimension represents altitude, "XYM" refers to three-dimensional coordinates where the third dimension refers to something else ("M" for measure); checking of the sanity of \code{x} may be only partial.
#' @examples 
#' (p1 = st_point(c(1,2)))
#' class(p1)
#' st_bbox(p1)
#' (p2 = st_point(c(1,2,3)))
#' class(p2)
#' (p3 = st_point(c(1,2,3), "XYM"))
#' pts = matrix(1:10, , 2)
#' (mp1 = st_multipoint(pts))
#' pts = matrix(1:15, , 3)
#' (mp2 = st_multipoint(pts))
#' (mp3 = st_multipoint(pts, "XYM"))
#' pts = matrix(1:20, , 4)
#' (mp4 = st_multipoint(pts))
#' pts = matrix(1:10, , 2)
#' (ls1 = st_linestring(pts))
#' pts = matrix(1:15, , 3)
#' (ls2 = st_linestring(pts))
#' (ls3 = st_linestring(pts, "XYM"))
#' pts = matrix(1:20, , 4)
#' (ls4 = st_linestring(pts))
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (ml1 = st_multilinestring(pts))
#' pts3 = lapply(pts, function(x) cbind(x, 0))
#' (ml2 = st_multilinestring(pts3))
#' (ml3 = st_multilinestring(pts3, "XYM"))
#' pts4 = lapply(pts3, function(x) cbind(x, 0))
#' (ml4 = st_multilinestring(pts4))
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (pl1 = st_polygon(pts))
#' pts3 = lapply(pts, function(x) cbind(x, 0))
#' (pl2 = st_polygon(pts3))
#' (pl3 = st_polygon(pts3, "XYM"))
#' pts4 = lapply(pts3, function(x) cbind(x, 0))
#' (pl4 = st_polygon(pts4))
#' pol1 = list(outer, hole1, hole2)
#' pol2 = list(outer + 12, hole1 + 12)
#' pol3 = list(outer + 24)
#' mp = list(pol1,pol2,pol3)
#' (mp1 = st_multipolygon(mp))
#' pts3 = lapply(mp, function(x) lapply(x, function(y) cbind(y, 0)))
#' (mp2 = st_multipolygon(pts3))
#' (mp3 = st_multipolygon(pts3, "XYM"))
#' pts4 = lapply(mp2, function(x) lapply(x, function(y) cbind(y, 0)))
#' (mp4 = st_multipolygon(pts4))
#' (gc = st_geometrycollection(list(p1, ls1, pl1, mp1)))
#' @export
st_point = function(x, third = "XYZ") {
	stopifnot(is.numeric(x))
	if (is.matrix(x))
		stopifnot(nrow(x) == 1) # because we want to be able to call rbind on points
	structure(x, class = getClassDim(x, length(x), third, "POINT"))
}
#' @name st
#' @export
st_multipoint = function(x, third = "XYZ") Mtrx(x, third, type = "MULTIPOINT")
#' @name st
#' @export
st_linestring = function(x, third = "XYZ") Mtrx(x, third, type = "LINESTRING")
#' @name st
#' @export
st_polygon = function(x, third = "XYZ") MtrxSet(x, third, type = "POLYGON", needClosed = TRUE)
#' @name st
#' @export
st_multilinestring = function(x, third = "XYZ") MtrxSet(x, third, type = "MULTILINESTRING", needClosed = FALSE)
#' @name st
#' @export
st_multipolygon = function(x, third = "XYZ") MtrxSetSet(x, third, type = "MULTIPOLYGON", needClosed = TRUE)
#' @name st
#' @export
st_geometrycollection = function(x, third = "XYZ") {
	cls = sapply(x, class)
	if (!is.matrix(cls) || !is.character(cls) || nrow(cls) != 3)
		stop("st_geometrycollection parameter x error: list elements should be simple features")
	stopifnot(all(cls[3,] == "sfi"))
	# check all dimensions are equal:
	dims = unique(cls[1,])
	if (length(dims) > 1)
		stop(paste("multiple dimensions found:", paste(dims, collapse = ", ")))
	structure(x, class = c(dims, "GEOMETRYCOLLECTION", "sfi")) # TODO: no Z/M/ZM modifier here??
}

POINT2MULTIPOINT = function(x, third = "XYZ") {
	if (length(x) == 3) # disambiguate Z/M:
		third = class(x)[1]
	st_multipoint(matrix(unclass(x), 1), third = third)
}
LINESTRING2MULTILINESTRING = function(x, third = "XYZ") {
	if (ncol(x) == 3) # disambiguate Z/M:
		third = class(x)[1]
	st_multilinestring(list(unclass(x)), third = third)
}
POLYGON2MULTIPOLYGON = function(x, third = "XYZ") {
	if (ncol(x[[1]]) == 3) # disambiguate Z/M:
		third = class(x)[1]
	st_multipolygon(list(unclass(x)), third = third)
}

#' @export
print.sfi = function(x, ..., digits = 0) { # avoids having to write print methods for 68 classes:
	f = format(x, ..., digits = digits)
	cat(f, "\n")
	invisible(f)
}

#' @export
format.sfi = function(x, ..., digits = 30) {
	if (is.null(digits)) 
		digits = 30
	pr = st_as_wkt(x)
	if (digits > 0 && nchar(pr) > digits)
		paste0(substr(pr, 1, digits - 3), "...")
	else
		pr
}
