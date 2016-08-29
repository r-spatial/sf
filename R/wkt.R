# composed, WKT class name: "Z", "POINT" -> "POINT_Z"
WKT_name = function(x, EWKT = TRUE) {
	cls = class(x)
	retval = paste0(cls[2], substr(cls[1], 3, 4))
	if (EWKT && !is.null(attr(x, "epsg")) && !is.na(attr(x, "epsg")))
		paste0("SRID=", attr(x, "epsg"), ";", retval)
	else
		retval
}

# print helper functions
prnt.POINT = function(x, ...) {
	nr = paste0(x, collapse = " ")
	paste0(WKT_name(x, ...), "(", nr, ")")
}
prnt.Matrix = function(x)
	paste0("(", paste0(apply(x, 1, paste0, collapse = " "), collapse = ", "), ")")
prnt.MatrixList = function(x)
	paste0("(", paste0(unlist(lapply(x, prnt.Matrix)), collapse = ", "), ")")
prnt.MatrixListList = function(x)
	paste0("(", paste0(unlist(lapply(x, prnt.MatrixList)), collapse = ", "), ")")
prnt.MULTIPOINT = function(x, ...) paste0(WKT_name(x, ...), prnt.Matrix(x))
prnt.LINESTRING = function(x, ...) paste0(WKT_name(x, ...), prnt.Matrix(x))
prnt.POLYGON = function(x, ...) paste0(WKT_name(x, ...), prnt.MatrixList(x))
prnt.MULTILINESTRING = function(x, ...) paste0(WKT_name(x, ...), prnt.MatrixList(x))
prnt.MULTIPOLYGON = function(x, ...) paste0(WKT_name(x, ...), prnt.MatrixListList(x))
prnt.GEOMETRYCOLLECTION = function(x, ...) 
	paste0(WKT_name(x, ...), "(", paste0(sapply(x, st_as_wkt), collapse=", "), ")")

#' Return Well-known Text representation of simple feature item
#'
#' Return Well-known Text representation of simple feature item
#' @param x object of class sfi or sfc
#' @param ... passed on to WKT_name
#' @name st_as_wkt
#' @details to suppress printing of SRID, \code{EWKT=FALSE} can be passed as parameter
#' @export
st_as_wkt = function(x, ...) UseMethod("st_as_wkt")

#' @name st_as_wkt
#' @export
#' @examples
#' st_as_wkt(st_point(1:2))
st_as_wkt.sfi = function(x, ...) {
	if (inherits(x, "POINT")) return(prnt.POINT(x, ...))
	if (inherits(x, "MULTIPOINT")) return(prnt.MULTIPOINT(x, ...))
	if (inherits(x, "LINESTRING")) return(prnt.LINESTRING(x, ...))
	if (inherits(x, "POLYGON")) return(prnt.POLYGON(x, ...))
	if (inherits(x, "MULTILINESTRING")) return(prnt.MULTILINESTRING(x, ...))
	if (inherits(x, "MULTIPOLYGON")) return(prnt.MULTIPOLYGON(x, ...))
	if (inherits(x, "GEOMETRYCOLLECTION")) return(prnt.GEOMETRYCOLLECTION(x, ...))
	stop(paste("no print method available for object of class", class(x)[1])) # nocov
}

#' @name st_as_wkt
#' @export
st_as_wkt.sfc = function(x, ...) {
	sapply(x, st_as_wkt, ...)
}
