getClassDim = function(x, d, third = "Z", type) {
	type = toupper(type)
	stopifnot(third %in% c("Z", "M"))
	if (d == 2)
		return(type)
	if (d == 3)
		return(c(paste(type, third, sep = "_"), type))
	if (d == 4)
		return(c(paste0(type, "_ZM"), type))
	stop(paste(d, "is an illegal number of columns for a", type))
}

Pt = function(x, third = "Z", type) {
	class(x) = c(getClassDim(x, length(x), third, type), "sfi")
	x
}
Mtrx = function(x, third = "Z", type) {
	class(x) = c(getClassDim(x, ncol(x), third, type), "sfi")
	x
}
MtrxSet = function(x, third = "Z", type, needClosed = FALSE) {
	nc = unique(sapply(x, ncol))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) any(head(y, 1) != tail(y, 1))
	if (needClosed && any(sapply(x, NotClosed)))
		stop("polygons not (all) closed")
	class(x) = c(getClassDim(x, ncol(x[[1]]), third, type), "sfi")
	x
}
MtrxSetSet = function(x, third = "Z", type, needClosed = FALSE) {
	nc = unique(unlist(lapply(x, function(y) sapply(y, ncol))))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) any(head(y, 1) != tail(y, 1))
	if (needClosed && any(unlist(sapply(x, function(y) sapply(y, NotClosed)))))
		stop("polygons not (all) closed")
	class(x) = c(getClassDim(x, ncol(x[[1]][[1]]), third, type), "sfi")
	x
}

#Dimension = function(x) { # should also return "XY", "XYZ", "XYM", "XYZM"?
#	if (is.matrix(x))
#		return(ncol(x))
#	if (is.numeric(x)) # Point
#		return(length(x))
#	# recurse:
#	return(Dimension(x[[1]]))
#}

CheckGC = function(x, third = "Z", type = "GeometryCollection") {
	# TODO
	# check all dimensions are equal
	#class(x) = c(getClassDim(x, Dimension(x[[1]]), third, type), "sfi") -> I'M NOT SURE!!
	class(x) = c(type, "sfi")
	x
}

#' Create a point simple feature from a numeric vector
#' 
#' Create a point simple feature from a numeric vector
#' @param x numeric vector of length 2, 3 or 4
#' @param third character, indicating what a 3-dimensional point refers to ("Z" or "M")
#' @param ... ignored
#' @name ST
#' @examples 
#' (p1 = ST_Point(c(1,2)))
#' class(p1)
#' bbox(p1)
#' (p2 = ST_Point(c(1,2,3)))
#' class(p2)
#' (p3 = ST_Point(c(1,2,3), "M"))
#' pts = matrix(1:10, , 2)
#' (mp1 = ST_MultiPoint(pts))
#' pts = matrix(1:15, , 3)
#' (mp2 = ST_MultiPoint(pts))
#' (mp3 = ST_MultiPoint(pts, "M"))
#' pts = matrix(1:20, , 4)
#' (mp4 = ST_MultiPoint(pts))
#' pts = matrix(1:10, , 2)
#' (ls1 = ST_LineString(pts))
#' pts = matrix(1:15, , 3)
#' (ls2 = ST_LineString(pts))
#' (ls3 = ST_LineString(pts, "M"))
#' pts = matrix(1:20, , 4)
#' (ls4 = ST_LineString(pts))
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (ml1 = ST_MultiLineString(pts))
#' pts3 = lapply(pts, function(x) cbind(x, 0))
#' (ml2 = ST_MultiLineString(pts3))
#' (ml3 = ST_MultiLineString(pts3, "M"))
#' pts4 = lapply(pts3, function(x) cbind(x, 0))
#' (ml4 = ST_MultiLineString(pts4))
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (pl1 = ST_Polygon(pts))
#' pts3 = lapply(pts, function(x) cbind(x, 0))
#' (pl2 = ST_Polygon(pts3))
#' (pl3 = ST_Polygon(pts3, "M"))
#' pts4 = lapply(pts3, function(x) cbind(x, 0))
#' (pl4 = ST_Polygon(pts4))
#' pol1 = list(outer, hole1, hole2)
#' pol2 = list(outer + 12, hole1 + 12)
#' pol3 = list(outer + 24)
#' mp = list(pol1,pol2,pol3)
#' (mp1 = ST_MultiPolygon(mp))
#' pts3 = lapply(mp, function(x) lapply(x, function(y) cbind(y, 0)))
#' (mp2 = ST_MultiPolygon(pts3))
#' (mp3 = ST_MultiPolygon(pts3, "M"))
#' pts4 = lapply(mp2, function(x) lapply(x, function(y) cbind(y, 0)))
#' (mp4 = ST_MultiPolygon(pts4))
#' (gc = ST_GeometryCollection(list(p1, ls1, pl1, mp1)))
#' (gc3 = ST_GeometryCollection(list(mp3, pl1)))
#' @export
ST_Point = function(x, third = "Z", ...) Pt(x, third, type = "POINT")
#' @name ST
#' @export
ST_MultiPoint = function(x, third = "Z", ...) Mtrx(x, third, type = "MULTIPOINT")
#' @name ST
#' @export
ST_LineString = function(x, third = "Z", ...) Mtrx(x, third, type = "LINESTRING")
#' @name ST
#' @export
ST_Polygon = function(x, third = "Z", ...) MtrxSet(x, third, type = "POLYGON", needClosed = TRUE)
#' @name ST
#' @export
ST_MultiLineString = function(x, third = "Z", ...) MtrxSet(x, third, type = "MULTILINESTRING", needClosed = FALSE)
#' @name ST
#' @export
ST_MultiPolygon = function(x, third = "Z", ...) MtrxSetSet(x, third, type = "MULTIPOLYGON", needClosed = TRUE)
#' @name ST
#' @export
ST_GeometryCollection = function(x, third = "Z", ...) CheckGC(x, third, type = "GEOMETRYCOLLECTION")

POINT2MULTIPOINT = function(x, third = "Z") {
	if (length(x) == 3) # disambiguate Z/M:
		third = if (length(grep("Z", class(x)[1])) > 0) "Z" else "M"
	ST_MultiPoint(matrix(unclass(x), 1), third = third)
}
LINESTRING2MULTILINESTRING = function(x, third = "Z") {
	if (ncol(x) == 3) # disambiguate Z/M:
		third = if (length(grep("Z", class(x)[1])) > 0) "Z" else "M"
	ST_MultiLineString(list(unclass(x)), third = third)
}
POLYGON2MULTIPOLYGON = function(x, third = "Z") {
	if (ncol(x[[1]]) == 3) # disambiguate Z/M:
		third = if (length(grep("Z", class(x)[1])) > 0) "Z" else "M"
	ST_MultiPolygon(list(unclass(x)), third = third)
}

# print helper functions
prnt.POINT = function(x, ...) {
	nr = paste0(x, collapse = " ")
	paste0(class(x)[1], "(", nr, ")")
}
prnt.Matrix = function(x, ...)
	paste0("(", paste0(apply(x, 1, paste0, collapse = " "), collapse = ", "), ")")

prnt.MatrixList = function(x, ...)
	paste0("(", paste0(unlist(lapply(x, prnt.Matrix)), collapse = ", "), ")")

prnt.MatrixListList = function(x, ...)
	paste0("(", paste0(unlist(lapply(x, prnt.MatrixList)), collapse = ", "), ")")

prnt.MULTIPOINT = function(x, ...) paste0(class(x)[1], prnt.Matrix(x, ...))
prnt.LINESTRING = function(x, ...) paste0(class(x)[1], prnt.Matrix(x, ...))
prnt.POLYGON = function(x, ...) paste0(class(x)[1], prnt.MatrixList(x, ...))
prnt.MULTILINESTRING = function(x, ...) paste0(class(x)[1], prnt.MatrixList(x, ...))
prnt.MULTIPOLYGON = function(x, ...) paste0(class(x)[1], prnt.MatrixListList(x, ...))
prnt.GEOMETRYCOLLECTION = function(x,...) 
	paste0(class(x)[1], "(", paste0(sapply(x, ST_as.WKT), collapse=", "), ")")

#' Return Well-known Text representation of simple feature item
#'
#' Return Well-known Text representation of simple feature item
#' @param x object of class sfi
#' @export
ST_as.WKT = function(x) UseMethod("ST_as.WKT") # not needed if sp exports bbox

#' @export
ST_as.WKT.default = function(x) stop(paste("no as.WKT method for object of class", class(x)[1]))

#' @export
ST_as.WKT.sfi = function(x) {
	if (inherits(x, "POINT")) return(prnt.POINT(x))
	if (inherits(x, "MULTIPOINT")) return(prnt.MULTIPOINT(x))
	if (inherits(x, "LINESTRING")) return(prnt.LINESTRING(x))
	if (inherits(x, "POLYGON")) return(prnt.POLYGON(x))
	if (inherits(x, "MULTILINESTRING")) return(prnt.MULTILINESTRING(x))
	if (inherits(x, "MULTIPOLYGON")) return(prnt.MULTIPOLYGON(x))
	if (inherits(x, "GEOMETRYCOLLECTION")) return(prnt.GEOMETRYCOLLECTION(x))
	stop(paste("no print method available for object of class", class(x)[1]))
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
	if (digits > 0 && nchar(pr) > digits - 3)
		paste(substr(pr, 1, digits), "...")
	else
		pr
}
