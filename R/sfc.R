#' @export
format.sfc = function(x, ..., digits = 30) {
	sapply(x, format, ..., digits = digits)
}

#' verify simple feature 
#' 
#' verifies simple feature list column's contents, and sets class
#' 
#' @param lst list with simple feature objects
#' @param epsg integer; epsg code
#' @param proj4string character; describing the coordinate reference systems in PROJ.4 syntax
#' 
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
	lst = coerceTypes(lst) # may coerce X to MULTIX, or a mix to a GeometryCollection
	class(lst) = c(paste0("sfc_", class(lst[[1]])[2]), "sfc")
	attr(lst, "epsg") = epsg
	attr(lst, "bbox") = bbox(lst)
	if (missing(proj4string) && !is.na(epsg) && epsg > 0) {
		proj4string = if (requireNamespace("sp", quietly = TRUE))
				sp::CRS(paste0("+init=epsg:", epsg))@projargs # resolve from proj lib, if rgdal
			else
				paste0("+init=epsg:", epsg)
	}
	attr(lst, "proj4string") = proj4string
	lst
}

# sync XX and_MULTIXX to uniform_MULTIXX set, just like PostGIS does;
# any other mix is merged into GEOMETRYCOLLECTION
coerceTypes = function(lst) { # breaks on errors, or returns the list
	sfi = sapply(lst, function(x) inherits(x, "sfi"))
	if (any(!sfi))
		stop(paste("list item", which(sfi)[1], "is not of class sfi"))
	#cls = unique(sapply(lst, function(x) class(x)[1]))
	cls = unique(sapply(lst, function(x) tail(class(x), 2L)[1]))
	if (length(cls) > 1) {
		if (all(cls %in% c("POINT", "MULTIPOINT")))
			return(lapply(lst, 
				function(x) if (inherits(x, "POINT")) POINT2MULTIPOINT(x) else x))
		if (all(cls %in% c("POLYGON", "MULTIPOLYGON")))
			return(lapply(lst, 
				function(x) if (inherits(x, "POLYGON")) POLYGON2MULTIPOLYGON(x) else x))
		if (all(cls %in% c("LINESTRING", "MULTILINESTRING")))
			return(lapply(lst, 
				function(x) if (inherits(x, "LINESTRING")) LINESTRING2MULTILINESTRING(x) else x))
		# need to coerce to GEOMETRYCOLLECTION here?
		stop(paste("multiple simple feature types [", paste(cls, collapse = ","),
			"] not allowed in a simple feature list column"))
	}
	lst
}

#' @export
"[.sfc" = function(x, i, j, ...) {
	recompute_bb = !missing(i)
    old = x
    x = NextMethod("[")
    attributes(x) = attributes(old)
	if (recompute_bb)
		attr(x, "bbox") = bbox(x)
    class(x) = class(old)
    x
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
summary.sfc = function(object, ..., maxsum = 7, maxp4s = 10) {
	u = factor(sapply(object, function(x) WKT_name(x, FALSE)))
    epsg = paste0("epsg:", attr(object, "epsg"))
	levels(u) = c(levels(u), epsg)
    p4s = attr(object, "proj4string")
	if (!is.na(p4s)) { 
		if (nchar(p4s) > maxp4s)
			p4s = paste0(substr(p4s, 1, maxp4s), "...")
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
	sapply(x, function(sfi) format(sfi, digits = 15))
}
