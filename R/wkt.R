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
	paste0(WKT_name(x, ...), "(", paste0(sapply(x, st_as_text), collapse=", "), ")")

#' Return Well-known Text representation of simple feature geometry
#'
#' Return Well-known Text representation of simple feature geometry
#' @param x object of class sfg or sfc
#' @param ... passed on to WKT_name
#' @name st_as_text
#' @details to suppress printing of SRID, \code{EWKT=FALSE} can be passed as parameter
#' @export
st_as_text = function(x, ...) UseMethod("st_as_text")

#' @name st_as_text
#' @export
#' @examples
#' st_as_text(st_point(1:2))
st_as_text.sfg = function(x, ...) {
	if (inherits(x, "POINT")) return(prnt.POINT(x, ...))
	if (inherits(x, "MULTIPOINT")) return(prnt.MULTIPOINT(x, ...))
	if (inherits(x, "LINESTRING")) return(prnt.LINESTRING(x, ...))
	if (inherits(x, "POLYGON")) return(prnt.POLYGON(x, ...))
	if (inherits(x, "MULTILINESTRING")) return(prnt.MULTILINESTRING(x, ...))
	if (inherits(x, "MULTIPOLYGON")) return(prnt.MULTIPOLYGON(x, ...))
	if (inherits(x, "GEOMETRYCOLLECTION")) return(prnt.GEOMETRYCOLLECTION(x, ...))
	if (inherits(x, "CIRCULARSTRING")) return(prnt.MULTIPOINT(x, ...))
	if (inherits(x, "COMPOUNDCURVE")) return(prnt.GEOMETRYCOLLECTION(x, ...))
	if (inherits(x, "CURVE")) return(prnt.MULTIPOINT(x, ...))
	if (inherits(x, "CURVEPOLYGON")) return(prnt.GEOMETRYCOLLECTION(x, ...))
	if (inherits(x, "MULTICURVE")) return(prnt.GEOMETRYCOLLECTION(x, ...))
	if (inherits(x, "MULTISURFACE")) return(prnt.GEOMETRYCOLLECTION(x, ...))
	if (inherits(x, "POLYHEDRALSURFACE")) return(prnt.MULTIPOLYGON(x, ...))
	if (inherits(x, "TRIANGLE")) return(prnt.POLYGON(x, ...))
	if (inherits(x, "TIN")) return(prnt.MULTIPOLYGON(x, ...))
	stop(paste("no print method available for object of class", class(x)[1])) # nocov
}

#' @name st_as_text
#' @param EWKT logical; if TRUE, print SRID=xxx; before the WKT string if epsg is available
#' @export
st_as_text.sfc = function(x, ..., EWKT = FALSE) {
	if (EWKT) {
		epsg = attr(x, "epsg")
		if (!is.na(epsg) && epsg != 0)
			x = lapply(x, function(sfg) structure(sfg, epsg = epsg))
	}
	sapply(x, st_as_text, ..., EWKT = EWKT)
}

#' @name st_as_sfc
#' @details if \code{x} is a character vector, it should be a vector containing the well-known-text representations of a single geometry for each vector element
#' @param crs integer or character; coordinate reference system for the geometry, see \link{st_crs}
#' @export
st_as_sfc.character = function(x, crs = NA_integer_, ...) {
	if (length(x) == 0)
		return(st_sfc(crs = crs))
	ret = st_sfc(CPL_sfc_from_wkt(x))
	st_crs(ret) = crs
	ret
}
