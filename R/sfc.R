bb = function(xmin, xmax, ymin, ymax) {
	c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
	# or return matrix instead, compatible to sp::bbox?
}

bbox.Mtrx = function(obj) {
	mn = apply(obj, 2, min)
	mx = apply(obj, 2, max)
	bb(xmin = mn[1], xmax = mx[1], ymin = mn[2], ymax = mx[2])
}
bbox.MtrxSet = function(obj) {
	s = sapply(obj, bbox.Mtrx)
	bb(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}
bbox.MtrxSetSet = function(obj) {
	s = sapply(obj, bbox.MtrxSet)
	bb(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}
bbox.MtrxSetSetSet = function(obj) {
	s = sapply(obj, bbox.MtrxSetSet)
	bb(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}

#' Return bounding of a simple feature or simple feature set
#'
#' Return bounding of a simple feature or simple feature set
#' @param obj object to compute the bounding box from
#' @export
bbox = function(obj) UseMethod("bbox") # not needed if sp exports bbox

#' @export
bbox.POINT = function(obj) bb(xmin = obj[1], xmax = obj[1], ymin = obj[2], ymax = obj[2])
#' @export
bbox.MULTIPOINT = bbox.Mtrx
#' @export
bbox.LINESTRING = bbox.Mtrx
#' @export
bbox.POLYGON = bbox.MtrxSet
#' @export
bbox.MULTILINESTRING = bbox.MtrxSet
#' @export
bbox.MULTIPOLYGON = bbox.MtrxSetSet
#' @export
bbox.GEOMETRYCOLLECTION = function(obj) {
	s = sapply(obj, bbox) # dispatch on class
	c(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
}

#' @export
bbox.sfc = function(obj) {
	switch(attr(obj, "type"),
		"POINT" = , "POINT Z" = , "POINT M" = , "POINT ZM" = bbox.Mtrx(do.call(rbind, obj)),
		"MULTIPOINT" = , "MULTIPOINT Z" = , "MULTIPOINT M" = , "MULTIPOINT ZM" = bbox.MtrxSet(obj),
		"LINESTRING" = , "LINESTRING Z" = , "LINESTRING M" = , "LINESTRING ZM" = bbox.MtrxSet(obj),
		"POLYGON" = , "POLYGON Z" = , "POLYGON M" = , "POLYGON ZM" = bbox.MtrxSetSet(obj),
		"MULTILINESTRING" = , "MULTILINESTRING Z" = , "MULTILINESTRING M" = , 
			"MULTILINESTRING ZM" = bbox.MtrxSetSet(obj),
		"MULTIPOLYGON" = , "MULTIPOLYGON Z" = , "MULTIPOLYGON M" = , 
			"MULTIPOLYGON ZM" = bbox.MtrxSetSetSet(obj),
		"GEOMETRYCOLLECTION" = , "GEOMETRYCOLLECTION Z" = , "GEOMETRYCOLLECTION M" = , 
			"GEOMETRYCOLLECTION ZM" = { 
				s = sapply(obj, bbox)
				bb(xmin = min(s[1,]), xmax = max(s[2,]), ymin = min(s[3,]), ymax = max(s[4,]))
			},
		stop("simple feature type not supported")
	)
}

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
#' pt1 = POINT(c(0,1))
#' pt2 = POINT(c(1,1))
#' (sfc = sfc(list(pt1, pt2)))
#' d = data.frame(a = 1:2)
#" d$geom = sfc
#' @export
sfc = function(lst, epsg = -1, proj4string = as.character(NA)) {
	stopifnot(is.list(lst))
	lst = checkTypes(lst)
	class(lst) = "sfc"
	attr(lst, "type") = class(lst[[1]])[1] # after checkTypes, they all are identical
	attr(lst, "epsg") = epsg
	attr(lst, "bbox") = bbox(lst)
	if (missing(proj4string) && epsg > 0)
		proj4string = CRS(paste0("+init=epsg:", epsg))@projargs
	attr(lst, "proj4string") = proj4string
	lst
}

checkTypes = function(lst) { # breaks on errors, or returns the list
	sfi = sapply(lst, function(x) inherits(x, "sfi"))
	if (any(!sfi))
		stop(paste("list item", which(sfi)[1], "is not of class sfi"))
	cls = unique(sapply(lst, function(x) class(x)[1]))
	# sync XX and MULTIXX to uniform MULTIXX set, just like PostGIS does:
	if (all(cls %in% c("POINT", "MULTIPOINT")))
		return(lapply(lst, function(x) if (inherits(x, "POINT")) POINT2MULTIPOINT(x) else x))
	if (all(cls %in% c("POLYGON", "MULTIPOLYGON")))
		return(lapply(lst, function(x) if (inherits(x, "POLYGON")) POLYGON2MULTIPOLYGON(x) else x))
	if (all(cls %in% c("LINESTRING", "MULTILINESTRING")))
		return(lapply(lst, function(x) if (inherits(x, "LINESTRING")) LINESTRING2MULTILINESTRING(x) else x))
	if (length(cls) > 1)
		stop("multiple simple feature types not allowed in a simple feature list column")
	cls
}

#' @export
"[.sfc" = function(x, i, j, ...) {
	recompute_bb = ! missing(i)
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

