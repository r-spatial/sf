#' @export
format.sfc = function(x, ..., digits = 30) {
	sapply(x, format, ..., digits = digits)
}

#' create simple feature collection object of class sfc from list
#' 
#' create simple feature list column, set class, and add coordinate reference system
#' 
#' @name sfc
#' @param ... one or more simple feature objects
#' @param crs coordinate reference system: integer with the epsg code, or character with proj4string
#' @param precision numeric; see \link{st_as_binary}
#' 
#' @details a simple feature collection object is a list of class \code{c("stc_TYPE", "sfc")} which contains objects of identical type. This function creates such an object from a list of simple feature objects (of class \code{sfg}), and coerces their type if necessary: collections of XX and MULTIXX are coerced to MULTIXX (with XX: POINT, LINESTRING or POLYGON), other sets are coerced to GEOMETRYCOLLECTION. 
#' @details in case \code{epsg} is given but \code{proj4string} is not and packages \code{sp} and \code{rgdal} can be loaded, the \code{proj4string} is expanded using the PROJ.4 epsg database.
#' @examples
#' pt1 = st_point(c(0,1))
#' pt2 = st_point(c(1,1))
#' (sfc = st_sfc(pt1, pt2))
#' d = data.frame(a = 1:2)
#" d$geom = sfc
#' @export
st_sfc = function(..., crs = NA_integer_, precision = 0.0) {
	lst = list(...)
	# if we have only one arg, which is already a list with sfg's, but NOT a geometrycollection:
	# (this is the old form of calling st_sfc; it is way faster to call st_sfc(lst) if lst
	# already contains a zillion sfg objects, than do.call(st_sfc, lst) ...
	if (length(lst) == 1 && is.list(lst[[1]]) && !inherits(lst[[1]], "sfg") 
			&& inherits(lst[[1]][[1]], "sfg"))
		lst = lst[[1]]
	stopifnot(is.numeric(crs) || is.character(crs) || is.list(crs))
	if (length(lst) == 0) # empty set: no geometries read
		class(lst) = c("sfc_GEOMETRY", "sfc")
	else {
		if (is.null(attr(lst, "non_empty"))) { # we're not comming from CPL_read_gdal
			l = sapply(lst, function(x) length(x) > 0)
			if (any(l))
				non_empty = l[1] # 0-based
			else
				non_empty = 1
			attr(lst, "n_empty") = sum(! l)
			u = unique(sapply(lst, class)[1,])
			if (length(u) > 1)
				stop(paste("found multiple dimensions:", paste(u, collapse = " ")))
		} else {
			non_empty = attr(lst, "non_empty")
			if (non_empty == 0)
				non_empty = 1 # FIXME: rethink
			attr(lst, "non_empty") = NULL # clean up
		}
		# do we have a mix of geometry types?
		is_single = function(x) length(unique(sapply(x, function(y) class(y)[2]))) == 1
		single = if (!is.null(attr(lst, "single_type"))) # we're back from CPL_read_gdal:
				attr(lst, "single_type")
			else
				is_single(lst)
		if (single)
			class(lst) = c(paste0("sfc_", class(lst[[non_empty]])[2L]), "sfc")
		else {
			class(lst) = c("sfc_GEOMETRY", "sfc")         # the mix
			attr(lst, "classes") = sapply(lst, class)[2,] # Rcpp forces me to do this. Or is it me, allowing a mix?
		}
		attr(lst, "single_type") = NULL # clean up
	}
	attr(lst, "precision") = precision
	attr(lst, "bbox") = st_bbox(lst)
	if (is.na(crs))
		st_crs(lst) = attributes(lst) # they might be in there, returned from a CPL_*
	else
		st_crs(lst) = crs
	lst
}

# coerce XX and MULTIXX mixes to MULTIXX, other mixes to GeometryCollection:
#coerce_types = function(lst) {
#	if (!identical(unique(sapply(lst, function(x) class(x)[3L])), "sfg"))
#		stop("list item(s) not of class sfg") # sanity check
#	cls = unique(sapply(lst, function(x) class(x)[2L]))
#	if (length(cls) > 1) {
#		if (all(cls %in% c("POINT", "MULTIPOINT")))
#			lapply(lst, 
#				function(x) if (inherits(x, "POINT")) POINT2MULTIPOINT(x) else x)
#		else if (all(cls %in% c("POLYGON", "MULTIPOLYGON")))
#			lapply(lst, 
#				function(x) if (inherits(x, "POLYGON")) POLYGON2MULTIPOLYGON(x) else x)
#		else if (all(cls %in% c("LINESTRING", "MULTILINESTRING")))
#			lapply(lst, 
#				function(x) if (inherits(x, "LINESTRING")) LINESTRING2MULTILINESTRING(x) else x)
#		else lapply(lst, 
#			function(x) if (inherits(x, "GEOMETRYCOLLECTION")) x 
#				else st_geometrycollection(list(x)))
#	} else
#		lst
#}

#' @export
"[.sfc" = function(x, i, j, ...) {
	recompute_bb = !missing(i)
    old = x
	if (!missing(i) && (inherits(i, "sf") || inherits(i, "sfc")))
		i = sapply(st_geos_binop("intersects", x, i, ...), length) != 0
    x = NextMethod("[")
    attributes(x) = attributes(old)
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
	cat(paste0("epsg (SRID):    ", attr(x, "epsg"), "\n"))
	cat(paste0("proj4string:    ", attr(x, "proj4string"), "\n"))
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

#' summarize simple feature column
#'
#' summarize simple feature column
#' @param object object of class \code{sfc}
#' @param ... ignored
#' @param maxsum maximum number of classes to summarize the simple feature column to
#' @param maxp4s maximum number of characters to print from the PROJ.4 string
#' @method summary sfc
#' @export
summary.sfc = function(object, ..., maxsum = 7L, maxp4s = 10L) {
	u = factor(sapply(object, function(x) WKT_name(x, FALSE)))
    epsg = paste0("epsg:", attr(object, "epsg"))
	levels(u) = c(levels(u), epsg)
    p4s = attr(object, "proj4string")
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

#' return geometry type of an object
#' 
#' return geometry type of an object, as a factor
#' @param x object of class \link{sf} or \link{sfc}
#' @return returns a factor with the geometry type of each simple feature in x
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

#' summarize simple feature type for tibble
#'
#' summarize simple feature type for tibble
#' @param x object of class sfc
#' @param ... ignored
#' @name tibble
#' @export
type_sum.sfc <- function(x, ...) {
   "simple_feature"
}

#' summarize simple feature item for tibble
#'
#' summarize simple feature item for tibble
#' @name tibble
#' @export
obj_sum.sfc <- function(x) {
	sapply(x, function(sfg) format(sfg, digits = 15L))
}

#' drop Z and/or M dimensions from feature geometries
#'
#' drop Z and/or M dimensions from feature geometries, resetting classes appropriately
#' @param x object of class \code{sfg}, \code{sfc} or \code{sf}
#' @param ... ignored
#' @examples
#' st_drop_zm(st_linestring(matrix(1:32,8)))
#' x = st_sfc(st_linestring(matrix(1:32,8)), st_linestring(matrix(1:8,2)))
#' st_drop_zm(x)
#' a = st_sf(a = 1:2, geom=x)
#' st_drop_zm(a)
#' @export
st_drop_zm <- function(x, ...) UseMethod("st_drop_zm")

#' @export
st_drop_zm.sf <- function(x, ...) {
	st_geometry(x) = st_drop_zm(st_geometry(x))
	return(x)
}

#' @export
st_drop_zm.sfc <- function(x, ...) {
	st_sfc(lapply(x, st_drop_zm))
}

#' @export
st_drop_zm.sfg <- function(x, ...) {
	ret = if (is.list(x))
		lapply(x, st_drop_zm)
	else if (is.matrix(x))
		x[,1:2]
	else
		x[1:2]
	structure(ret, class = c("XY", class(x)[2:3]))
}

#' @export
st_drop_zm.list <- function(x, ...) lapply(x, st_drop_zm)

#' @export
st_drop_zm.matrix <- function(x, ...) x[, 1:2, drop = FALSE]
