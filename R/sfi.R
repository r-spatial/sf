getClassDim = function(x, d, third = "Z", type) {
	type = toupper(type)
	stopifnot(third %in% c("Z", "M"))
	if (d == 2)
		return(type)
	if (d == 3)
		return(paste(type, third))
	if (d == 4)
		return(paste(type, "ZM"))
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
MtrxSet = function(x, third = "Z", type) {
	nc = unique(sapply(x, ncol))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) any(head(y, 1) != tail(y, 1))
	if (any(sapply(x, NotClosed)))
		stop("polygons not (all) closed")
	class(x) = c(getClassDim(x, ncol(x[[1]]), third, type), "sfi")
	x
}
MtrxSetSet = function(x, third = "Z", type) {
	nc = unique(as.vector(sapply(x, function(y) sapply(y, ncol))))
	if (length(nc) != 1)
		stop("matrices having unequal number of columns")
	NotClosed = function(y) any(head(y, 1) != tail(y, 1))
	if (any(unlist(sapply(x, function(y) sapply(y, NotClosed)))))
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
#'
#' @export
POINT = function(x, third = "Z", ...) Pt(x, third, type = "POINT")
#' Create a multipoint simple feature from a numeric matrix
#' 
#' Create a multipoint simple feature from a numeric matrix
#' @param x numeric matrix with 2, 3 or 4 columns
#' @param third character, indicating what the third dimension refers to ("Z" or "M")
#' @param ... ignored
#'
#' @export
MULTIPOINT = function(x, third = "Z", ...) Mtrx(x, third, type = "MULTIPOINT")
#' Create a linestring simple feature from a numeric matrix
#' 
#' Create a linestring simple feature from a numeric matrix
#' @param x numeric matrix with 2, 3 or 4 columns
#' @param third character, indicating what the third dimension refers to ("Z" or "M")
#' @param ... ignored
#'
#' @export
LINESTRING = function(x, third = "Z", ...) Mtrx(x, third, type = "LINESTRING")
#' Create a polygon simple feature from a list with coordinate matrices
#' 
#' Create a polygon simple feature from a list with coordinate matrices
#' @param x list with numeric matrices with 2, 3 or 4 columns
#' @param third character, indicating what the third dimension refers to ("Z" or "M")
#' @param ... ignored
#'
#' @export
POLYGON = function(x, third = "Z", ...) MtrxSet(x, third, type = "POLYGON")
#' Create a multilinestring simple feature from a list with coordinate matrices
#' 
#' Create a multilinestring simple feature from a list with coordinate matrices
#' @param x list with numeric matrices with 2, 3 or 4 columns
#' @param third character, indicating what the third dimension refers to ("Z" or "M")
#' @param ... ignored
#'
#' @export
MULTILINESTRING = function(x, third = "Z", ...) MtrxSet(x, third, type = "MULTILINESTRING")
#' Create a multipolygon simple feature from a list of lists with coordinate matrices
#' 
#' Create a multipolygon simple feature from a list of lists with coordinate matrices
#' @param x list with numeric matrices with 2, 3 or 4 columns
#' @param third character, indicating what the third dimension refers to ("Z" or "M")
#' @param ... ignored
#'
#' @export
MULTIPOLYGON = function(x, third = "Z", ...) MtrxSetSet(x, third, type = "MULTIPOLYGON")
#' Create a geometrycollection from a list other simple feature items
#'
#' Create a geometrycollection from a list other simple feature items
#'
#' @param x list with numeric matrices with 2, 3 or 4 columns
#' @param third character, indicating what the third dimension refers to ("Z" or "M")
#' @param ... ignored
#'
#' @export
GEOMETRYCOLLECTION = function(x, third = "Z", ...) CheckGC(x, third, type = "GEOMETRYCOLLECTION")

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
	paste0(class(x)[1], "(", paste0(sapply(x, print), collapse=", "), ")")

#' @export
print.sfi = function(x, ...) { # avoids having to write print methods for 68 classes:
	fn = switch(class(x)[1], 
		"POINT" = , "POINT Z" = , "POINT M" = , "POINT ZM" = prnt.POINT,
		"MULTIPOINT" = , "MULTIPOINT Z" = , "MULTIPOINT M" = , "MULTIPOINT ZM" = prnt.MULTIPOINT,
		"LINESTRING" = , "LINESTRING Z" = , "LINESTRING M" = , "LINESTRING ZM" = prnt.LINESTRING,
		"POLYGON" = , "POLYGON Z" = , "POLYGON M" = , "POLYGON ZM" = prnt.POLYGON,
		"MULTILINESTRING" = , "MULTILINESTRING Z" = , "MULTILINESTRING M" = , 
			"MULTILINESTRING ZM" = prnt.MULTILINESTRING,
		"MULTIPOLYGON" = , "MULTIPOLYGON Z" = , "MULTIPOLYGON M" = , 
			"MULTIPOLYGON ZM" = prnt.MULTIPOLYGON,
		"GEOMETRYCOLLECTION" = , "GEOMETRYCOLLECTION Z" = , "GEOMETRYCOLLECTION M" = , 
			"GEOMETRYCOLLECTION ZM" = prnt.GEOMETRYCOLLECTION,
		stop(paste("no print method available for object of class", class(x)[1]))
	)
	print(fn(x, ...))
}
