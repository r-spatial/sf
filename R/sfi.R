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

Pt = function(x, third = "XYZ", type) {
	class(x) = getClassDim(x, length(x), third, type)
	x
}
Mtrx = function(x, third = "XYZ", type) {
	class(x) = getClassDim(x, ncol(x), third, type)
	x
}
MtrxSet = function(x, third = "XYZ", type, needClosed = FALSE) {
	nc = unique(sapply(x, ncol))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) any(head(y, 1) != tail(y, 1))
	if (needClosed && any(sapply(x, NotClosed)))
		stop("polygons not (all) closed")
	class(x) = getClassDim(x, ncol(x[[1]]), third, type)
	x
}
MtrxSetSet = function(x, third = "XYZ", type, needClosed = FALSE) {
	nc = unique(unlist(lapply(x, function(y) sapply(y, ncol))))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) any(head(y, 1) != tail(y, 1))
	if (needClosed && any(unlist(sapply(x, function(y) sapply(y, NotClosed)))))
		stop("polygons not (all) closed")
	class(x) = getClassDim(x, ncol(x[[1]][[1]]), third, type)
	x
}

#return "XY", "XYZ", "XYM", or "XYZM"
Dimension = function(x) { 
	stopifnot(inherits(x, "sfi"))
	class(x)[1]
}

CheckGC = function(x, third = "XYZ", type = "GeometryCollection") {
	# check all dimensions are equal:
	cls = unique(sapply(x, function(el) class(el)[1]))
	if (length(cls) > 1)
		stop(paste("multiple dimensions found:", paste(cls, collapse = ", ")))
	class(x) = c(cls, toupper(type), "sfi") # TODO: no Z/M/ZM modifier here??
	x
}

#' Create a point simple feature from a numeric vector
#' 
#' Create a point simple feature from a numeric vector
#' @param x numeric vector of length 2, 3 or 4
#' @param third character, indicating what a 3-dimensional point refers to ("XYZ" or "XYM")
#' @param ... ignored
#' @name ST
#' @examples 
#' (p1 = ST_Point(c(1,2)))
#' class(p1)
#' bbox(p1)
#' (p2 = ST_Point(c(1,2,3)))
#' class(p2)
#' (p3 = ST_Point(c(1,2,3), "XYM"))
#' pts = matrix(1:10, , 2)
#' (mp1 = ST_MultiPoint(pts))
#' pts = matrix(1:15, , 3)
#' (mp2 = ST_MultiPoint(pts))
#' (mp3 = ST_MultiPoint(pts, "XYM"))
#' pts = matrix(1:20, , 4)
#' (mp4 = ST_MultiPoint(pts))
#' pts = matrix(1:10, , 2)
#' (ls1 = ST_LineString(pts))
#' pts = matrix(1:15, , 3)
#' (ls2 = ST_LineString(pts))
#' (ls3 = ST_LineString(pts, "XYM"))
#' pts = matrix(1:20, , 4)
#' (ls4 = ST_LineString(pts))
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (ml1 = ST_MultiLineString(pts))
#' pts3 = lapply(pts, function(x) cbind(x, 0))
#' (ml2 = ST_MultiLineString(pts3))
#' (ml3 = ST_MultiLineString(pts3, "XYM"))
#' pts4 = lapply(pts3, function(x) cbind(x, 0))
#' (ml4 = ST_MultiLineString(pts4))
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (pl1 = ST_Polygon(pts))
#' pts3 = lapply(pts, function(x) cbind(x, 0))
#' (pl2 = ST_Polygon(pts3))
#' (pl3 = ST_Polygon(pts3, "XYM"))
#' pts4 = lapply(pts3, function(x) cbind(x, 0))
#' (pl4 = ST_Polygon(pts4))
#' pol1 = list(outer, hole1, hole2)
#' pol2 = list(outer + 12, hole1 + 12)
#' pol3 = list(outer + 24)
#' mp = list(pol1,pol2,pol3)
#' (mp1 = ST_MultiPolygon(mp))
#' pts3 = lapply(mp, function(x) lapply(x, function(y) cbind(y, 0)))
#' (mp2 = ST_MultiPolygon(pts3))
#' (mp3 = ST_MultiPolygon(pts3, "XYM"))
#' pts4 = lapply(mp2, function(x) lapply(x, function(y) cbind(y, 0)))
#' (mp4 = ST_MultiPolygon(pts4))
#' (gc = ST_GeometryCollection(list(p1, ls1, pl1, mp1)))
#' @export
ST_Point = function(x, third = "XYZ", ...) Pt(x, third, type = "POINT")
#' @name ST
#' @export
ST_MultiPoint = function(x, third = "XYZ", ...) Mtrx(x, third, type = "MULTIPOINT")
#' @name ST
#' @export
ST_LineString = function(x, third = "XYZ", ...) Mtrx(x, third, type = "LINESTRING")
#' @name ST
#' @export
ST_Polygon = function(x, third = "XYZ", ...) MtrxSet(x, third, type = "POLYGON", needClosed = TRUE)
#' @name ST
#' @export
ST_MultiLineString = function(x, third = "XYZ", ...) MtrxSet(x, third, type = "MULTILINESTRING", needClosed = FALSE)
#' @name ST
#' @export
ST_MultiPolygon = function(x, third = "XYZ", ...) MtrxSetSet(x, third, type = "MULTIPOLYGON", needClosed = TRUE)
#' @name ST
#' @export
ST_GeometryCollection = function(x, third = "XYZ", ...) CheckGC(x, third, type = "GEOMETRYCOLLECTION")

POINT2MULTIPOINT = function(x, third = "XYZ") {
	if (length(x) == 3) # disambiguate Z/M:
		third = class(x)[1]
	ST_MultiPoint(matrix(unclass(x), 1), third = third)
}
LINESTRING2MULTILINESTRING = function(x, third = "XYZ") {
	if (ncol(x) == 3) # disambiguate Z/M:
		third = class(x)[1]
	ST_MultiLineString(list(unclass(x)), third = third)
}
POLYGON2MULTIPOLYGON = function(x, third = "XYZ") {
	if (ncol(x[[1]]) == 3) # disambiguate Z/M:
		third = class(x)[1]
	ST_MultiPolygon(list(unclass(x)), third = third)
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
	pr = ST_as.WKT(x)
	if (digits > 0 && nchar(pr) > digits)
		paste0(substr(pr, 1, digits - 3), "...")
	else
		pr
}
