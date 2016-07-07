#' verify simple feature 
#' 
#' verifies simple feature list column's contents, and sets class
#' 
#' @param lst list with simple feature objects
#' @param epsg integer; epsg code
#' @param proj4string character; describing the coordinate reference systems in PROJ.4 syntax
#' 
#' @examples
#' pt1 = POINT(c(0,1))
#' pt2 = POINT(c(1,1))
#' (sfc = sfc(list(pt1, pt2)))
#' d = data.frame(a = 1:2)
#" d$geom = sfc
#' @export
sfc = function(lst, epsg = -1, proj4string = as.character(NA)) {
	stopifnot(is.list(lst))
	type = checkTypes(lst)
	class(lst) = "sfc"
	attr(lst, "type") = type
	attr(lst, "epsg") = epsg
	attr(lst, "bbox") = bbox(lst)
	if (missing(proj4string) && epsg > 0)
		proj4string = CRS(paste0("+init=epsg:", epsg))@projargs
	attr(lst, "proj4string") = proj4string
	lst
}

checkTypes = function(lst) { # breaks on errors, or returns the unique class
	sfi = sapply(lst, function(x) inherits(x, "sfi"))
	if (any(!sfi))
		stop(paste("list item", which(sfi)[1], "is not of class sfi"))
	cls = unique(sapply(lst, function(x) class(x)[1]))
	if (length(cls) > 1)
		stop("multiple simple feature types not allowed in a simple feature list column")
	cls
}

#' @export
"[.sfc" = function(x, i, j, ...) {
    old = x
    x = NextMethod("[")
    attributes(x) = attributes(old)
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
	u = factor(sapply(object, function(x) class(x)[1]))
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

#' create sf object
#' 
#' create sf, which extends data.frame-like objects with a simple feature list column
#'
#' @param df object of class \code{data.frame}
#'
#' @examples
#' pt1 = POINT(c(0,1))
#' pt2 = POINT(c(1,1))
#' sfc(list(pt1, pt2))
#' d = data.frame(a = 1:2)
#' d$geom = sfc(list(pt1, pt2))
#' df = sf(d)
#' d$geom2 = sfc(list(pt1, pt2))
#' sf(df) # warns
#' @export
sf = function(df) {
	sf = sapply(df, function(x) inherits(x, "sfc"))
	if (!any(sf))
		stop("no simple features geometry column present")
	sf_column = which(sf)
	if (length(sf_column) > 1)
		warning("more than one geometry column not allowed, picking first")
	attr(df, "sf_column") = which(sf)[1]
	class(df) = c("sf", class(df))
	df
}
