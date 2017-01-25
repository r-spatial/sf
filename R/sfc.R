#' @export
str.sfc <- function(object,...) {
	n <- length(object)
	if (n == 0L)
		str(object)
	else {
		cat(" List of ",n,", printing ")
		str(object[[1]],...)
	}
}

#' @export
format.sfc = function(x, ..., digits = 30) {
	sapply(x, format, ..., digits = digits)
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
			attr(lst, "n_empty") = sum(sapply(lst, function(x) length(x) == 0))
			u = unique(sapply(lst, class)[1,])
			if (length(u) > 1)
				stop(paste("found multiple dimensions:", paste(u, collapse = " ")))
		} 

		# class: do we have a mix of geometry types?
		single = if (!is.null(attr(lst, "single_type"))) # set by CPL_read_wkb:
				attr(lst, "single_type")
			else
				length(unique(sapply(lst, function(y) class(y)[2]))) == 1L
		if (single)
			class(lst) = c(paste0("sfc_", class(lst[[1L]])[2L]), "sfc")
		else {
			class(lst) = c("sfc_GEOMETRY", "sfc")         # the mix
			attr(lst, "classes") = sapply(lst, class)[2L,] # Rcpp forces me to do this. Or is it me, allowing a mix?
		}
		attr(lst, "single_type") = NULL # clean up
	}
	if (is.na(crs) && !is.null(attr(lst, "crs")))
		crs = attr(lst, "crs")
	if (! missing(precision) || is.null(attr(lst, "precision")))
		attr(lst, "precision") = precision
	st_crs(lst) = crs
	structure(lst, "bbox" = c(st_bbox(lst)))
}

#' @export
"[.sfc" = function(x, i, j, ...) {
	recompute_bb = !missing(i)
    old = x
	if (!missing(i) && (inherits(i, "sf") || inherits(i, "sfc")))
		i = sapply(st_geos_binop("intersects", x, i, ...), length) != 0
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
print.sfc = function(x, ..., n = 5L, what = "Geometry set for", append = "") { 
	if (length(x) != 1) 
		sep = "s" 
	else
		sep = ""
	cls = substr(class(x)[1], 5, nchar(class(x)[1]))
	cat(paste0(what, " ", length(x), " feature", sep, " ", append))
	if (! is.null(attr(x, "n_empty"))) {
		ne = attr(x, "n_empty")
		if (ne > 0)
			cat(paste0("(of which ", ne, ifelse(ne > 1, " are ", " is "), "empty)"))
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
	u = factor(sapply(object, function(x) WKT_name(x, FALSE)))
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
	factor(sapply(x, function(y) class(y)[2]), levels =
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
#' @details only combinations \code{drop=TRUE}, \code{what = "ZM"}, and \code{drop=FALSE}, \code{what="Z"} are supported so far. In case \code{add=TRUE}, zero values are added.
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
		ret = if (is.list(x))
			lapply(x, st_zm, drop = drop, what = what)
		else if (is.matrix(x))
			cbind(unclass(x), 0)
		else
			c(unclass(x), 0)
		structure(ret, class = c("XYZ", class(x)[2:3]))
	} else 
		stop("this combination of drop and what is not implemented")
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
#' @param precision numeric; see \link{st_as_binary}
#' @examples 
#' x <- st_sfc(st_point(c(pi, pi)))
#' st_precision(x)
#' st_precision(x) <- 0.01
#' st_precision(x)
#' @export
st_set_precision <- function(x, precision) {
    UseMethod("st_set_precision")
}

st_set_precision.sfc <- function(x, precision) {
    if (length(precision) != 1) {
        stop("Precision applies to all dimensions and must be of length 1.", call. = FALSE)
    }
    if (is.na(precision) || !is.numeric(precision)) {
        stop("Precision must be numeric", call. = FALSE)
    }
    structure(x, precision = precision)
}

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
