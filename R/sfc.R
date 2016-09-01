#' @export
format.sfc = function(x, ..., digits = 30) {
	sapply(x, format, ..., digits = digits)
}

#' create simple feature collection object of class sfc from list
#' 
#' create simple feature list column, set class, and add coordinate reference system
#' 
#' @param lst list with simple feature objects, or single simple feature
#' @param epsg integer; epsg code
#' @param proj4string character; describing the coordinate reference systems in PROJ.4 syntax
#' 
#' @details a simple feature collection object is a list of class \code{c("stc_TYPE", "sfc")} which contains objects of identical type. This function creates such an object from a list of simple feature objects (of class \code{sfi}), and coerces their type if necessary: collections of XX and MULTIXX are coerced to MULTIXX (with XX: POINT, LINESTRING or POLYGON), other sets are coerced to GEOMETRYCOLLECTION. 
#' @details in case \code{epsg} is given but \code{proj4string} is not and packages \code{sp} and \code{rgdal} can be loaded, the \code{proj4string} is expanded using the PROJ.4 epsg database.
#' @examples
#' pt1 = st_point(c(0,1))
#' pt2 = st_point(c(1,1))
#' (sfc = st_sfc(list(pt1, pt2)))
#' d = data.frame(a = 1:2)
#" d$geom = sfc
#' @export
st_sfc = function(lst, epsg = NA_integer_, proj4string = NA_character_) {
	if (!is.list(lst))
		lst = list(lst)
	if (is.null(attr(lst, "n_types")) || attr(lst, "n_types") != 1)
		lst = coerceTypes(lst)
	class(lst) = c(paste0("sfc_", class(lst[[1L]])[2L]), "sfc")
	attr(lst, "epsg") = epsg
	attr(lst, "bbox") = st_bbox(lst)
	attr(lst, "bbox") = rep(0,4)
	if (missing(proj4string) && !is.na(epsg) && epsg > 0L) {
		proj4string = if (requireNamespace("sp", quietly = TRUE))
				sp::CRS(paste0("+init=epsg:", epsg))@projargs # resolve from proj lib, uses rgdal
			else
				paste0("+init=epsg:", epsg)
	}
	attr(lst, "proj4string") = proj4string
	lst
}

# coerce XX and MULTIXX mixes to MULTIXX, other mixes to GeometryCollection:
coerceTypes = function(lst) {
	if (!identical(unique(sapply(lst, function(x) class(x)[3L])), "sfi"))
		stop("list item(s) not of class sfi") # sanity check
	cls = unique(sapply(lst, function(x) class(x)[2L]))
	if (length(cls) > 1) {
		if (all(cls %in% c("POINT", "MULTIPOINT")))
			lapply(lst, 
				function(x) if (inherits(x, "POINT")) POINT2MULTIPOINT(x) else x)
		else if (all(cls %in% c("POLYGON", "MULTIPOLYGON")))
			lapply(lst, 
				function(x) if (inherits(x, "POLYGON")) POLYGON2MULTIPOLYGON(x) else x)
		else if (all(cls %in% c("LINESTRING", "MULTILINESTRING")))
			lapply(lst, 
				function(x) if (inherits(x, "LINESTRING")) LINESTRING2MULTILINESTRING(x) else x)
		else lapply(lst, 
			function(x) if (inherits(x, "GEOMETRYCOLLECTION")) x 
				else st_geometrycollection(list(x)))
	} else
		lst
}

#' @export
"[.sfc" = function(x, i, j, ...) {
	recompute_bb = !missing(i)
    old = x
    x = NextMethod("[")
    attributes(x) = attributes(old)
	if (recompute_bb)
		attr(x, "bbox") = st_bbox(x)
    structure(x, class = class(old))
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
	sapply(x, function(sfi) format(sfi, digits = 15L))
}
