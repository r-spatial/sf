# composed, WKT class name: "XYZ", "POINT" -> "POINT Z"
WKT_name = function(x, EWKT = TRUE) {
	cls = class(x)
	zm = substr(cls[1], 3, 4)

	retval = if (zm == "")
		cls[2]
	else 
		paste(cls[2], substr(cls[1], 3, 4))

	if (EWKT && !is.null(attr(x, "epsg")) && !is.na(attr(x, "epsg")))
		paste0("SRID=", attr(x, "epsg"), ";", retval)
	else
		retval
}

empty = "EMPTY"

# skip leading white space; ... passes on digits:
fmt = function(x, ...) sub("^[ ]+", "", sapply(unclass(x), format, ...))

# print helper functions
prnt.POINT = function(x, ..., EWKT = TRUE) {
	pt = if (any(!is.finite(x)))
		empty
	else 
		paste0("(", paste0(fmt(x, ...), collapse = " "), ")")
	paste(WKT_name(x, EWKT = EWKT), pt)
}

prnt.Matrix = function(x, ...) {
	pf = function(x, ..., collapse) paste0(fmt(x, ...), collapse = collapse)
	if (nrow(x) == 0)
		empty
	else
		paste0("(", paste0(apply(x, 1, pf, collapse = " ", ...), collapse = ", "), ")")
}

prnt.MatrixList = function(x, ...) {
	if (length(x) == 0)
		empty
	else
		paste0("(", paste0(unlist(lapply(x, prnt.Matrix, ...)), collapse = ", "), ")")
}

prnt.MatrixListList = function(x, ...) {
	if (length(x) == 0)
		empty
	else
		paste0("(", paste0(unlist(lapply(x, prnt.MatrixList, ...)), collapse = ", "), ")")
}

prnt.MULTIPOINT = function(x, ..., EWKT = TRUE) paste(WKT_name(x, EWKT = EWKT), prnt.Matrix(x, ...))
prnt.LINESTRING = function(x, ..., EWKT = TRUE) paste(WKT_name(x, EWKT = EWKT), prnt.Matrix(x, ...))
prnt.POLYGON = function(x, ..., EWKT = TRUE) paste(WKT_name(x, EWKT = EWKT), prnt.MatrixList(x, ...))
prnt.MULTILINESTRING = function(x, ..., EWKT = TRUE) paste(WKT_name(x, EWKT = EWKT), prnt.MatrixList(x, ...))
prnt.MULTIPOLYGON = function(x, ..., EWKT = TRUE) paste(WKT_name(x, EWKT = EWKT), prnt.MatrixListList(x, ...))
prnt.GEOMETRYCOLLECTION = function(x, ..., EWKT = TRUE) {
	body = if (length(x) == 0)
		empty
	else
		paste0("(", paste0(vapply(x, st_as_text, "", ...), collapse=", "), ")")
	paste(WKT_name(x, EWKT = EWKT), body)
}

#' Return Well-known Text representation of simple feature geometry or coordinate reference system
#'
#' Return Well-known Text representation of simple feature geometry or coordinate reference system
#' @param x object of class \code{sfg}, \code{sfc} or \code{crs}
#' @param ... modifiers; in particular \code{digits} can be passed to control the number of digits used
#' @name st_as_text
#' @details The returned WKT representation of simple feature geometry conforms to the
#' \href{http://www.opengeospatial.org/standards/sfa}{simple features access} specification and extensions,
#' \href{http://postgis.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT}{known as EWKT}, supported by
#' PostGIS and other simple features implementations for addition of SRID to a WKT string.
#'
#' @export
st_as_text = function(x, ...) UseMethod("st_as_text")

#' @name st_as_text
#' @export
#' @examples
#' st_as_text(st_point(1:2))
#' st_as_text(st_sfc(st_point(c(-90,40)), crs = 4326), EWKT = TRUE)
st_as_text.sfg = function(x, ...) {
	if (Sys.getenv("LWGEOM_WKT") == "true" && requireNamespace("lwgeom", quietly = TRUE) && utils::packageVersion("lwgeom") >= "0.1-5")
		lwgeom::st_astext(x, ...)
	else 
	  switch(class(x)[2],
		POINT = prnt.POINT(x, ...),
		MULTIPOINT =        prnt.MULTIPOINT(x, ...),
		LINESTRING =        prnt.LINESTRING(x, ...),
		POLYGON =           prnt.POLYGON(x, ...),
		MULTILINESTRING =   prnt.MULTILINESTRING(x, ...),
		MULTIPOLYGON =      prnt.MULTIPOLYGON(x, ...),
		GEOMETRYCOLLECTION =prnt.GEOMETRYCOLLECTION(x, ...),
		CIRCULARSTRING =    prnt.MULTIPOINT(x, ...),
		COMPOUNDCURVE =     prnt.GEOMETRYCOLLECTION(x, ...),
		CURVE =             prnt.MULTIPOINT(x, ...),
		CURVEPOLYGON =      prnt.GEOMETRYCOLLECTION(x, ...),
		MULTICURVE =        prnt.GEOMETRYCOLLECTION(x, ...),
		MULTISURFACE =      prnt.GEOMETRYCOLLECTION(x, ...),
		POLYHEDRALSURFACE = prnt.MULTIPOLYGON(x, ...),
		TRIANGLE =          prnt.POLYGON(x, ...),
		TIN =                prnt.MULTIPOLYGON(x, ...),
		stop(paste("no print method available for object of class", class(x)[2])) # nocov
	)
}

#' @name st_as_text
#' @param EWKT logical; if TRUE, print SRID=xxx; before the WKT string if \code{epsg} is available
#' @export
st_as_text.sfc = function(x, ..., EWKT = FALSE) {
	if (Sys.getenv("LWGEOM_WKT") == "true" && requireNamespace("lwgeom", quietly = TRUE) && utils::packageVersion("lwgeom") >= "0.1-5")
		lwgeom::st_astext(x, ..., EWKT = EWKT)
	else {
		if (EWKT) {
			epsg = attr(x, "crs")$epsg
			if (!is.na(epsg) && epsg != 0)
				x = lapply(x, function(sfg) structure(sfg, epsg = epsg))
		}
		vapply(x, st_as_text, "", ..., EWKT = EWKT)
	}
}

#' @name st_as_sfc
#' @rdname st_as_sfc
#' @md
#' @details If `x` is a character vector, it should be a vector containing
#' [well-known-text](http://www.opengeospatial.org/standards/wkt-crs), or
#' [Postgis EWKT](http://postgis.refractions.net/docs/using_postgis_dbmanagement.html#EWKB_EWKT) or
#' GeoJSON representations of a single geometry for each vector element.
#' @param crs integer or character; coordinate reference system for the
#' @param GeoJSON logical; if \code{TRUE}, try to read geometries from GeoJSON text strings
#' geometry, see [st_crs()]
#' @export
#' @examples
#' st_as_sfc("SRID=3978;LINESTRING(1663106 -105415,1664320 -104617)")
st_as_sfc.character = function(x, crs = NA_integer_, ..., GeoJSON = FALSE) {
	if (length(x) == 0)
		st_sfc(crs = crs)
	else if (GeoJSON) {
		ret = st_geometry(do.call(rbind, lapply(x, st_read, quiet = TRUE)))
		if (is.na(st_crs(ret)))
			st_set_crs(ret, crs)
		else
			ret
	} else {
		if (all(is_ewkt(x)) & is.na(crs)) {
			# EWKT
			crs = get_crs_ewkt(x)
			crs = unique(crs)
			if (length(crs) != 1) {
				stop("sf does not support multiple crs (",
					 paste(crs, collapse = ", "),
					 ") within a single geometry column.",
					 "You can override the crs from the string by using the ",
					 "`crs` option from `st_as_sfc()`.",
					 call. = FALSE)
			}
			x = ewkt_to_wkt(x)
		}
		ret = st_sfc(CPL_sfc_from_wkt(x))
		st_crs(ret) = crs
		ret
	}
}
#' @name st_as_sfc
#' @details If \code{x} is a \code{factor}, it is converted to \code{character}.
#' @export
st_as_sfc.factor = function(x, ...) {
	st_as_sfc(as.character(x), ...)
}

is_ewkt = function(x) {
	grepl("^SRID=(\\d+);", x)
}

get_crs_ewkt = function(x) {
	as.numeric(gsub("^SRID=(\\d+);.+$", "\\1", x))
}

ewkt_to_wkt = function(x) {
	gsub("^SRID=(\\d+);(.+)$", "\\2", x)
}
