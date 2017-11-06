# grid graphics utilities

#' Convert sf* object to a grob
#'
#' Convert sf* object to an grid graphics object (grob)
#' @param x object to be converted into an object class \code{grob}
#' @param units units; see \link[grid]{unit}
#' @param ... passed on to the xxxGrob function, e.g. \code{gp = gpar(col = 'red')}
#' @export
st_as_grob = function(x, ..., units = "native") UseMethod("st_as_grob")

#' @export
st_as_grob.POINT = function(x, ..., default.units = "native") {
	if (any(is.na(x)))
		nullGrob()
	else
		pointsGrob(x[1], x[2], ..., default.units = default.units)
}

#' @export
st_as_grob.MULTIPOINT = function(x, ..., default.units = "native") {
	if (nrow(x) == 0)
		nullGrob()
	else
		pointsGrob(x[,1], x[,2], ..., default.units = default.units)
}

#' @export
st_as_grob.LINESTRING = function(x, ..., default.units = "native") {
	if (nrow(x) == 0)
		nullGrob()
	else
		linesGrob(x[,1], x[,2], ..., default.units = default.units)
}

#' @export
st_as_grob.CIRCULARSTRING = function(x, y, ...) {
	st_as_grob(st_cast(x, "LINESTRING"),  ...)
}

#' @export
st_as_grob.MULTILINESTRING = function(x, ..., default.units = "native") {
	if (length(x) == 0)
		nullGrob()
	else {
		get_x = function(x) unlist(sapply(x, function(y) y[,1]))
		get_y = function(x) unlist(sapply(x, function(y) y[,2]))
		polylineGrob(get_x(x), get_y(x), id.lengths = vapply(x, nrow, 0L), ...,
			default.units = default.units)
	}
}

#' @export
st_as_grob.POLYGON = function(x, ..., default.units = "native", rule = "evenodd") {
	if (length(x) == 0)
		nullGrob()
	else {
		get_x = function(x) unlist(sapply(x, function(y) y[,1]))
		get_y = function(x) unlist(sapply(x, function(y) y[,2]))
		pathGrob(get_x(x), get_y(x), id.lengths = vapply(x, nrow, 0L), ..., default.units = default.units, rule = rule)
	}
}

#' @export
st_as_grob.MULTIPOLYGON = function(x, ..., default.units = "native", rule = "evenodd") {
	if (length(x) == 0)
		nullGrob()
	else {
		get_x = function(x) unlist(sapply(x, function(y) sapply(y, function(z) z[,1])))
		get_y = function(x) unlist(sapply(x, function(y) sapply(y, function(z) z[,2])))
		get_l = function(x) unlist(sapply(x, function(y) vapply(y, nrow, 0L)))
		pathGrob(get_x(x), get_y(x), id.lengths = get_l(x), ..., default.units = default.units, rule = rule)
	}
}

#' @export
st_as_grob.GEOMETRYCOLLECTION = function(x, ..., default.units = "native") {
	if (length(x) == 0)
		nullGrob()
	else
		do.call(grid::grobTree, lapply(x, st_as_grob, ..., default.units = default.units))
}

#' @export
st_as_grob.MULTISURFACE = st_as_grob.GEOMETRYCOLLECTION

#' @export
st_as_grob.CURVEPOLYGON = st_as_grob.GEOMETRYCOLLECTION

#' @export
st_as_grob.COMPOUNDCURVE = st_as_grob.GEOMETRYCOLLECTION


#' Create viewport from sf, sfc or sfg object
#'
#' Create viewport from sf, sfc or sfg object
#' @param x object of class sf, sfc or sfg object
#' @param bbox the bounding box used for aspect ratio
#' @param asp numeric; target aspect ratio (y/x), see Details
#' @param ... parameters passed on to \link[grid]{viewport}
#' @details parameters \code{width}, \code{height}, \code{xscale} and \code{yscale} are set such that aspect ratio is honoured and plot size is maximized in the current viewport; others can be passed as \code{...}
#' @return The output of the call to \link[grid]{viewport}
#' @details If \code{asp} is missing, it is taken as 1, except when \code{isTRUE(st_is_longlat(x))}, in which case it is set to \code{1.0 /cos(y)}, with \code{y} the middle of the latitude bounding box.
#' @examples
#' library(grid)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' grid.newpage()
#' pushViewport(viewport(width = 0.8, height = 0.8))
#' pushViewport(st_viewport(nc))
#' invisible(lapply(st_geometry(nc), function(x) grid.draw(st_as_grob(x, gp = gpar(fill = 'red')))))
#' @export
st_viewport = function(x, ..., bbox = st_bbox(x), asp) {
	xscale = bbox[c(1,3)]
	yscale = bbox[c(2,4)]

	# from rgdal2/R/graphics.R:
	current.viewport.size = function(units = "inches") {
    	vp = current.viewport()
    	vi = convertUnit(vp$height, units)
    	wi = convertUnit(vp$width, units)
    	c(width = wi, height = vi)
	}
	current.viewport.aspect = function() {
    	sz = current.viewport.size()
    	unclass(sz[2]) / unclass(sz[1])
	}
	vp.asp = current.viewport.aspect()
	if (missing(asp))
		asp = if (isTRUE(st_is_longlat(x)))
			1.0 / cos((mean(yscale) * pi)/180)
		else
			1.0

   	obj.asp = asp * diff(yscale) / diff(xscale)
	height = obj.asp / vp.asp
   	width = 1
   	width = width / max(width, height)
   	height = height / max(width, height)
	viewport(width = unit(width, "npc"), height = unit(height, "npc"),
    		xscale = unit(xscale, "native"), yscale = unit(yscale, "native"), ...)
}
