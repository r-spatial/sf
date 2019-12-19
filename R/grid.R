#' Convert sf* object to a grob
#'
#' Convert sf* object to an grid graphics object (grob)
#' @param x object to be converted into an object class \code{grob}
#' @param ... passed on to the xxxGrob function, e.g. \code{gp = gpar(col = 'red')}
#' @export
st_as_grob = function(x, ...) UseMethod("st_as_grob")

#' @export
st_as_grob.POINT = function(x, pch = 1, size = unit(1, "char"), default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	if (any(is.na(x)))
		nullGrob()
	else
		pointsGrob(x[1], x[2], pch = pch, size = size, default.units = default.units, name = name, gp = gp, vp = vp)
}

#' @export
st_as_grob.MULTIPOINT = function(x, pch = 1, size = unit(1, "char"), default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	if (nrow(x) == 0)
		nullGrob()
	else
		pointsGrob(x[,1], x[,2], pch = pch, size = size, default.units = default.units, name = name, gp = gp, vp = vp)
}

#' @export
st_as_grob.LINESTRING = function(x, arrow = NULL, default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	if (nrow(x) == 0)
		nullGrob()
	else
		linesGrob(x[,1], x[,2], arrow = NULL, default.units = default.units, name = name, gp = gp, vp = vp)
}

#' @export
st_as_grob.CIRCULARSTRING = function(x, y, ...) {
	st_as_grob(st_cast(x, "LINESTRING"),  ...)
}

#' @export
st_as_grob.MULTILINESTRING = function(x, arrow = NULL, default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	if (length(x) == 0)
		nullGrob()
	else {
		get_x = function(x) unlist(sapply(x, function(y) y[,1]))
		get_y = function(x) unlist(sapply(x, function(y) y[,2]))
		polylineGrob(get_x(x), get_y(x), id.lengths = vapply(x, nrow, 0L), arrow = NULL,
			default.units = default.units, name = name, gp = gp, vp = vp)
	}
}

#' @export
st_as_grob.POLYGON = function(x, default.units = "native", rule = "evenodd", name = NULL, gp = gpar(), vp = NULL, ...) {
	if (length(x) == 0)
		nullGrob()
	else {
		get_x = function(x) unlist(sapply(x, function(y) y[,1]))
		get_y = function(x) unlist(sapply(x, function(y) y[,2]))
		pathGrob(get_x(x), get_y(x), id.lengths = vapply(x, nrow, 0L), default.units = default.units, rule = rule, name = name, gp = gp, vp = vp)
	}
}

#' @export
st_as_grob.MULTIPOLYGON = function(x, default.units = "native", rule = "evenodd", name = NULL, gp = gpar(), vp = NULL, ...) {
	if (length(x) == 0)
		nullGrob()
	else {
		get_x = function(x) unlist(sapply(x, function(y) sapply(y, function(z) z[,1])))
		get_y = function(x) unlist(sapply(x, function(y) sapply(y, function(z) z[,2])))
		get_l = function(x) unlist(sapply(x, function(y) vapply(y, nrow, 0L)))
		pathGrob(get_x(x), get_y(x), id.lengths = get_l(x), default.units = default.units, rule = rule, name = name, gp = gp, vp = vp)
	}
}

#' @export
st_as_grob.GEOMETRYCOLLECTION = function(x, ...) {
	if (length(x) == 0)
		nullGrob()
	else
		do.call(grid::grobTree, lapply(x, st_as_grob, ...))
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
		vi = convertHeight(vp$height, units, valueOnly = TRUE)
		wi = convertWidth(vp$width, units, valueOnly = TRUE)
    	c(width = wi, height = vi)
	}
	current.viewport.aspect = function() {
    	sz = current.viewport.size()
    	sz[2] / sz[1]
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

#' @export
st_as_grob.sfc_POINT <- function(x, pch = 1, size = unit(1, "char"), default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	x <- matrix(unlist(x, use.names = FALSE), ncol = length(x))
	pointsGrob(x[1, ], x[2, ], pch = pch, size = size, default.units = default.units, name = name, gp = gp, vp = vp)
}
#' @export
#' @importFrom grid gpar
st_as_grob.sfc_MULTIPOINT <- function(x, pch = 1, size = unit(1, "char"), default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	x <- unclass(x)
	n_points <- vapply(x, nrow, integer(1))
	gp <- expand_gp(gp, n_points)
	if (length(pch) != 1) pch <- rep(rep(pch, length.out = length(x)), n_points)
	if (length(size) != 1) size <- rep(rep(size, length.out = length(x)), n_points)
	x <- do.call(rbind, x)
	pointsGrob(x[, 1], x[, 2], pch = pch, size = size, default.units = default.units, name = name, gp = gp, vp = vp)
}
#' @export
st_as_grob.sfc_LINESTRING <- function(x, arrow = NULL, default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	x <- unclass(x)
	n_points <- vapply(x, nrow, integer(1))
	x <- do.call(rbind, x)
	polylineGrob(x[, 1], x[, 2], id.lengths = n_points, arrow = arrow, default.units = default.units, name = name, gp = gp, vp = vp)
}
#' @export
st_as_grob.sfc_MULTILINESTRING <- function(x, arrow = NULL, default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	x <- unclass(x)
	n_lines <- vapply(x, length, integer(1))
	gp <- expand_gp(gp, n_lines)
	if (!is.null(arrow) && length(arrow) != 1) arrow <- rep(rep(arrow, length.out = length(x)), n_lines)
	x <- unlist(x, recursive = FALSE)
	n_points <- vapply(x, nrow, integer(1))
	x <- do.call(rbind, x)
	polylineGrob(x[, 1], x[, 2], id.lengths = n_points, arrow = arrow, default.units = default.units, name = name, gp = gp, vp = vp)
}
#' @export
st_as_grob.sfc_POLYGON <- function(x, rule = "evenodd", default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	if (utils::packageVersion("grid") < "3.6") {
		return(scalar_grobs(x, rule = rule, default.units = default.units, name = name, gp = gp, vp = vp, ...)) # nocov
	}
	x <- unclass(x) # nocov start
	n_poly <- vapply(x, length, integer(1))
	x <- unlist(x, recursive = FALSE)
	n_points <- vapply(x, nrow, integer(1))
	n_paths <- tapply(n_points, rep(seq_along(n_poly), n_poly), sum)
	x <- do.call(rbind, x)
	pathGrob(x[, 1], x[, 2], id.lengths = n_points, pathId.lengths = n_paths, rule = rule, default.units = default.units, name = name, gp = gp, vp = vp) # nocov end
}
#' @export
st_as_grob.sfc_MULTIPOLYGON <- function(x, rule = "evenodd", default.units = "native", name = NULL, gp = gpar(), vp = NULL, ...) {
	if (utils::packageVersion("grid") < "3.6") {
		return(scalar_grobs(x, rule = rule, default.units = default.units, name = name, gp = gp, vp = vp, ...)) # nocov
	}
	x <- unclass(x) # nocov start
	n_poly <- vapply(x, length, integer(1))
	gp <- expand_gp(gp, n_poly)
	x <- unlist(x, recursive = FALSE)
	n_poly <- vapply(x, length, integer(1))
	x <- unlist(x, recursive = FALSE)
	n_points <- vapply(x, nrow, integer(1))
	n_paths <- tapply(n_points, rep(seq_along(n_poly), n_poly), sum)
	x <- do.call(rbind, x)
	pathGrob(x[, 1], x[, 2], id.lengths = n_points, pathId.lengths = n_paths, rule = rule, default.units = default.units, name = name, gp = gp, vp = vp) # nocov end
}
#' @export
st_as_grob.sfc_CIRCULARSTRING <- function(x, ...) {
	st_as_grob(st_cast(x, 'LINESTRING'), ...) # nocov
}
#' @export
#' @importFrom grid gList
st_as_grob.sfc <- function(x, pch = 1, size = unit(1, "char"), arrow = NULL, gp = gpar(), ...) {
	x <- st_cast_sfc_default(x)
	if (class(x)[1] %in% c('sfc_MULTIPOINT', 'sfc_MULTILINESTRING', 'sfc_MULTIPOLYGON')) {
		return(st_as_grob(x, gp = gp, ...))
	}
	scalar_grobs(x, pch, size, arrow, gp, ...)
}

scalar_grobs <- function(x, pch = 1, size = unit(1, "char"), arrow = NULL, gp = gpar(), ...) {
	gp <- split_gp(gp, length(x))
	pch <- rep(pch, length.out = length(x))
	size <- rep(size,  length.out = length(x))
	if (!is.null(arrow)) arrow <- rep(arrow, length.out = length(x))
	do.call(gList, lapply(seq_along(x), function(i) {
		st_as_grob(x[[i]], pch = pch[i], size = size[i], arrow = arrow[i], gp = gp[[i]], ...)
	}))
}

expand_gp <- function(gp, n) {
	if (length(gp) == 0) return(gp)
	gp <- unclass(gp)
	n_gp <- vapply(gp, length, integer(1))
	gp[n_gp > 1] <- lapply(gp[n_gp > 1], rep, n)
	`class<-`(gp, 'gpar')
}
split_gp <- function(gp, n) {
	gp <- unclass(gp)
	gp <- lapply(gp, rep_len, n)
	lapply(seq_len(n), function(i) {
		`class<-`(lapply(gp, `[`, i), 'gpar')
	})
}
