#' @export
str.sfc <- function(object, ...) {
	n <- length(object)
	cat(paste0(class(object)[1], " of length ", n))
	if (n > 0) {
		cat("; first list element: ")
		str(object[[1]], ...)
	}
}

#' @export
format.sfc = function(x, ..., digits = 30) {
	vapply(x, format, "", ..., digits = digits)
}

#' Create simple feature collection object of class sfc from list
#' 
#' Create simple feature list column, set class, and add coordinate reference system
#' 
#' @name sfc
#' @param ... one or more simple feature geometries
#' @param crs coordinate reference system: integer with the epsg code, or character with proj4string
#' @param precision numeric; see \link{st_as_binary}
#' 
#' @details a simple feature collection object is a list of class 
#' \code{c("stc_TYPE", "sfc")} which contains objects of identical type. This 
#' function creates such an object from a list of simple feature geometries (of 
#' class \code{sfg}). 
#' @examples
#' pt1 = st_point(c(0,1))
#' pt2 = st_point(c(1,1))
#' (sfc = st_sfc(pt1, pt2))
#' d = data.frame(a = 1:2)
#" d$geom = sfc
#' @export
st_sfc = function(..., crs = NA_crs_, precision = 0.0) {
	lst = list(...)
	# if we have only one arg, which is already a list with sfg's, but NOT a geometrycollection:
	# (this is the old form of calling st_sfc; it is way faster to call st_sfc(lst) if lst
	# already contains a zillion sfg objects, than do.call(st_sfc, lst) ...
	if (length(lst) == 1 && is.list(lst[[1]]) && !inherits(lst[[1]], "sfg") 
			&& (length(lst[[1]]) == 0 || inherits(lst[[1]][[1]], "sfg")))
		lst = lst[[1]]
	stopifnot(is.numeric(crs) || is.character(crs) || inherits(crs, "crs"))
	if (length(lst) == 0) # empty set: no geometries read
		class(lst) = c("sfc_GEOMETRY", "sfc")
	else {
		# n_empty:
		if (is.null(attr(lst, "n_empty"))) { # we're NOT comming from CPL_read_wkb:
			attr(lst, "n_empty") = sum(vapply(lst, function(x) length(x) == 0, TRUE))
			u = unique(vapply(lst, class, rep("", 3))[1,])
			if (length(u) > 1)
				stop(paste("found multiple dimensions:", paste(u, collapse = " ")))
		} 

		# class: do we have a mix of geometry types?
		single = if (!is.null(attr(lst, "single_type"))) # set by CPL_read_wkb:
				attr(lst, "single_type")
			else
				length(unique(vapply(lst, function(y) class(y)[2], ""))) == 1L
		if (single) {
			class(lst) = c(paste0("sfc_", class(lst[[1L]])[2L]), "sfc")
			attr(lst, "classes") = NULL
		} else {
			class(lst) = c("sfc_GEOMETRY", "sfc")         # the mix
			attr(lst, "classes") = vapply(lst, class, rep("", 3))[2L,]
		}
		attr(lst, "single_type") = NULL # clean up
	}
	if (! missing(precision) || is.null(attr(lst, "precision")))
		attr(lst, "precision") = precision

	if (is.na(crs) && !is.null(attr(lst, "crs")))
		crs = attr(lst, "crs")
	st_crs(lst) = crs

	structure(lst, "bbox" = c(st_bbox(lst)))
}

#' @export
"[.sfc" = function(x, i, j, ...) {
	recompute_bb = !missing(i)
    old = x
	if (!missing(i) && (inherits(i, "sf") || inherits(i, "sfc")))
		i = lengths(st_geos_binop("intersects", x, i, ...)) != 0
    x = NextMethod("[")
	a = attributes(old)
	if (!is.null(names(x)))
		a$names = names(x)[i]
	if (!is.null(a$classes))
		a$classes = a$classes[i]
    attributes(x) = a
	if (recompute_bb)
		attr(x, "bbox") = st_bbox(x)
    structure(x, class = class(old))
}

#' @export
c.sfc = function(..., recursive = FALSE) {
#	lst = list(...)
#	ret = do.call(st_sfc, unlist(lapply(lst, unclass), recursive = FALSE))
#	return(st_set_crs(ret, st_crs(lst[[1]])))

	lst = list(...)
	cls = class(lst[[1]])
	eq = if (length(lst) > 1)
			all(vapply(lst[-1], function(x) identical(class(x), cls), TRUE))
		else
			TRUE
	if (! eq)
		cls = c("sfc_GEOMETRY", "sfc")

	ret = unlist(lapply(lst, unclass), recursive = FALSE)
	attributes(ret) = attributes(lst[[1]]) # crs
	class(ret) = cls
	attr(ret, "bbox") = st_bbox(ret) # dispatch on class
	if (! eq)
		attr(ret, "classes") = vapply(ret, class, rep("", 3))[2L,]
	ret
}

#' @export
print.sfc = function(x, ..., n = 5L, what = "Geometry set for", append = "") { 
	sep = if (length(x) != 1) "s" else ""
	cls = substr(class(x)[1], 5, nchar(class(x)[1]))
	cat(paste0(what, " ", length(x), " feature", sep, " ", append))
	if (! is.null(attr(x, "n_empty"))) {
		ne = attr(x, "n_empty")
		if (ne > 0)
			cat(paste0(" (of which ", ne, ifelse(ne > 1, " are ", " is "), "empty)"))
	}
	cat("\n")
	if (length(x)) {
		cat(paste0("geometry type:  ", cls, "\n"))
		cat(paste0("dimension:      ", class(x[[1]])[1], "\n"))
	}
	cat(paste0("bbox:           "))
	bb = signif(attr(x, "bbox"), 7)
	cat(paste(paste(names(bb), bb[], sep = ": "), collapse = " "))
	cat("\n")
	# attributes: epsg, proj4string, precision
	cat(paste0("epsg (SRID):    ", attr(x, "crs")$epsg, "\n"))
	cat(paste0("proj4string:    ", attr(x, "crs")$proj4string, "\n"))
	if (attr(x, "precision") != 0.0) {
		cat(paste0("precision:      "))
		if (attr(x, "precision") < 0.0)
			cat("float (single precision)\n")
		else
			cat(paste(attr(x, "precision"), "\n"))
	} # else cat("double (default; no precision model)\n")
	if (length(x) > n && n > 0)
		cat(paste0("First ", n, " geometries:\n"))
	for (i in seq_len(min(n, length(x))))
		print(x[[i]], digits = 50)
	invisible(x)
}

#' Summarize simple feature column
#'
#' Summarize simple feature column
#' @param object object of class \code{sfc}
#' @param ... ignored
#' @param maxsum maximum number of classes to summarize the simple feature column to
#' @param maxp4s maximum number of characters to print from the PROJ.4 string
#' @method summary sfc
#' @export
summary.sfc = function(object, ..., maxsum = 7L, maxp4s = 10L) {
	u = factor(vapply(object, function(x) WKT_name(x, FALSE), ""))
    epsg = paste0("epsg:", attr(object, "crs")$epsg)
	levels(u) = c(levels(u), epsg)
    p4s = attr(object, "crs")$proj4string
	if (!is.na(p4s)) { 
		if (nchar(p4s) > maxp4s)
			p4s = paste0(substr(p4s, 1L, maxp4s), "...")
		levels(u) = c(levels(u), p4s)	
	}
    summary(u, maxsum = maxsum, ...)
}

#' @export
as.data.frame.sfc = function(x, ...) {
	ret = data.frame(row.names = seq_along(x))
	ret$geometry = x
	ret
}


#' @name st_geometry
#' @export
st_geometry.sfc = function(obj, ...) obj

#' Return geometry type of an object
#' 
#' Return geometry type of an object, as a factor
#' @param x object of class \link{sf} or \link{sfc}
#' @return a factor with the geometry type of each simple feature in x
#' @export
st_geometry_type = function(x) {
	x = st_geometry(x)
	factor(vapply(x, function(y) class(y)[2], ""), levels =
		c("GEOMETRY",
		"POINT",
		"LINESTRING",
		"POLYGON",
		"MULTIPOINT",
		"MULTILINESTRING",
		"MULTIPOLYGON",
		"GEOMETRYCOLLECTION",
		"CIRCULARSTRING",
		"COMPOUNDCURVE",
		"CURVEPOLYGON",
		"MULTICURVE",
		"MULTISURFACE",
		"CURVE",
		"SURFACE",
		"POLYHEDRALSURFACE",
		"TIN",
		"TRIANGLE"))
}

#' Drop or add Z and/or M dimensions from feature geometries
#'
#' Drop Z and/or M dimensions from feature geometries, resetting classes appropriately
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param ... ignored
#' @param drop logical; drop, or (FALSE) add?
#' @param what character which dimensions to drop or add
#' @details only combinations \code{drop=TRUE}, \code{what = "ZM"}, and \code{drop=FALSE}, \code{what="Z"} are supported so far. In case \code{add=TRUE}, \code{x} should have \code{XY} geometry, and zero values are added for \code{Z}.
#' @examples
#' st_zm(st_linestring(matrix(1:32,8)))
#' x = st_sfc(st_linestring(matrix(1:32,8)), st_linestring(matrix(1:8,2)))
#' st_zm(x)
#' a = st_sf(a = 1:2, geom=x)
#' st_zm(a)
#' @export
st_zm <- function(x, ..., drop = TRUE, what = "ZM") UseMethod("st_zm")

#' @export
st_zm.sf <- function(x, ..., drop = TRUE, what = "ZM") {
	st_geometry(x) = st_zm(st_geometry(x), drop = drop, what = what)
	x
}

#' @export
st_zm.sfc <- function(x, ..., drop = TRUE, what = "ZM") {
	st_sfc(lapply(x, st_zm, drop = drop, what = what), crs = st_crs(x))
}

#' @export
st_zm.sfg <- function(x, ..., drop = TRUE, what = "ZM") {
	if (drop && what == "ZM") {
		ret = if (is.list(x))
			lapply(x, st_zm, drop = drop, what = what)
		else if (is.matrix(x))
			x[,1:2]
		else
			x[1:2]
		structure(ret, class = c("XY", class(x)[2:3]))
	} else if (!drop && what == "Z") {
		if (class(x)[1] != "XY")
			stop("adding Z only supported for XY geometries")
		ret = if (is.list(x))
			lapply(x, st_zm, drop = drop, what = what)
		else if (is.matrix(x))
			cbind(unclass(x), 0)
		else
			c(unclass(x), 0)
		structure(ret, class = c("XYZ", class(x)[2:3]))
	} else 
		stop("this combination of `x', `drop' and `what' is not implemented")
}

#' @export
st_zm.list <- function(x, ..., drop = TRUE, what = "ZM") 
	lapply(x, st_zm, drop = drop, what = what)

#' @export
st_zm.matrix <- function(x, ..., drop = TRUE, what = "ZM")  {
	if (drop && what == "ZM") {
		x[,1:2]
	} else if (!drop && what == "Z") {
		cbind(unclass(x), 0)
	} else 
		stop("this combination of drop and what is not implemented")
}

#' Get precision
#' 
#' @param x object of class \code{sfc} or \code{sf}
#' @export
st_precision <- function(x) {
  UseMethod("st_precision")
}

#' @export
st_precision.sf <- function(x) {
  x <- st_geometry(x)
  st_precision(x)
}

#' @export
st_precision.sfc <- function(x) {
  attr(x, "precision")
}

#' Set precision
#' 
#' @name st_precision
#' @param precision numeric; see \link{st_as_binary} for how to do this.
#' @details setting a precision has no direct effect on coordinates of geometries, but merely set an attribute tag to an \code{sfc} object. The effect takes place in \link{st_as_binary} or, more precise, in the C++ function \code{CPL_write_wkb}, where simple feature geometries are being serialized to well-known-binary (WKB). This happens always when routines are called in GEOS library (geometrical operations or predicates), for writing geometries using \link{st_write}, \link{write_sf} or \link{st_write_db}, and (if present) for liblwgeom (\link{st_make_valid}). Routines in these libraries receive rounded coordinates, and possibly return results based on them. \link{st_as_binary} contains an example of a roundtrip of \code{sfc} geometries through WKB, in order to see the rounding happening to R data.
#'
#' The reason to support precision is that geometrical operations in GEOS or liblwgeom may work better at reduced precision. For writing data from R to external resources it is harder to think of a good reason to limiting precision.
#' @examples 
#' x <- st_sfc(st_point(c(pi, pi)))
#' st_precision(x)
#' st_precision(x) <- 0.01
#' st_precision(x)
#' @export
st_set_precision <- function(x, precision) {
    UseMethod("st_set_precision")
}

#' @export
st_set_precision.sfc <- function(x, precision) {
    if (length(precision) != 1) {
        stop("Precision applies to all dimensions and must be of length 1.", call. = FALSE)
    }
    if (is.na(precision) || !is.numeric(precision)) {
        stop("Precision must be numeric", call. = FALSE)
    }
    structure(x, precision = precision)
}

#' @export
st_set_precision.sf <- function(x, precision) {
    st_geometry(x) <- st_set_precision(st_geometry(x), precision)
    return(x)
}

#' @name st_precision
#' @param value precision value
#' @export
"st_precision<-" <- function(x, value) {
    st_set_precision(x, value)
}

# if g may have NULL elements, replace it with (appropriate?) empty geometries
fix_NULL_values = function(g) {
	isNull = which(vapply(g, is.null, TRUE))
	for (i in isNull)
		g[[i]] = st_geometrycollection() # should improve here: try st_linestring() etc
	attr(g, "n_empty") = length(isNull)
	if (length(isNull))
		structure(g, class = c("sfc_GEOMETRY", "sfc"))
	else 
		g
}

#' retrieve coordinates in matrix form
#' 
#' retrieve coordinates in matrix form
#' @param x object of class sf, sfc or sfg
#' @param ... ignored
#' @return matrix with coordinates (X, Y, possibly Z and/or M) in rows, possibly followed by integer indicators \code{L1},...,\code{L3} that point out to which structure the coordinate belongs; for \code{POINT} this is absent (each coordinate is a feature), for \code{LINESTRING} \code{L1} refers to the feature, for \code{MULTIPOLYGON} \code{L1} refers to the main ring or holes, \code{L2} to the ring id in the \code{MULTIPOLYGON}, and \code{L3} to the simple feature. 
#' @export
st_coordinates = function(x, ...) UseMethod("st_coordinates")

#' @export
st_coordinates.sf = function(x, ...) st_coordinates(st_geometry(x))

#' @export
st_coordinates.sfg = function(x, ...) st_coordinates(st_geometry(x))

#' @export
st_coordinates.sfc = function(x, ...) {
	if (length(x) == 0)
		return(matrix(nrow = 0, ncol = 2))

	ret = switch(class(x)[1],
		sfc_POINT = t(simplify2array(x)),
		sfc_MULTIPOINT = ,
		sfc_LINESTRING = coord_2(x),
		sfc_MULTILINESTRING = ,
		sfc_POLYGON = coord_3(x),
		sfc_MULTIPOLYGON = coord_4(x),
		stop("not implemented")
	)
	Dims = class(x[[1]])[1]
	ncd = nchar(Dims)
	colnames(ret)[1:ncd] = vapply(seq_len(ncd), function(i) substr(Dims, i, i), "")
	ret
}

coord_2 = function(x) { # x is a list with matrices
	cbind(do.call(rbind, x), L1 = rep(seq_along(x), times = vapply(x, nrow, 0L)))
}

coord_3 = function(x) { # x is a list of list with matrices
	x = lapply(x, coord_2)
	cbind(do.call(rbind, x), L2 = rep(seq_along(x), times = vapply(x, nrow, 0L)))
}

coord_4 = function(x) { # x is a list of list with matrices
	x = lapply(x, coord_3)
	cbind(do.call(rbind, x), L3 = rep(seq_along(x), times = vapply(x, nrow, 0L)))
}
