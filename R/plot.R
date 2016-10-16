## projected: now handled by sp in plot.Spatial
#projected = function(x) {
#	if (inherits(x, "sf"))
#		x = st_geometry(x)
#	p4str = attr(x, "proj4string")
#	if (is.na(p4str))
#		NA
#	else
#		length(grep("longlat", p4str, fixed = TRUE)) == 0
#}

#' plot sf object
#'
#' @param x object of class sf
#' @param y ignored
#' @param ... further specifications, see \link{plot}
#' @param pch plotting symbol
#' @param cex symbol size
#' @param bg symbol background color
#' @param lty line type
#' @param lwd line width
#' @param col color
#' @param border color of polygon border
#' @param add logical; add to current plot?
#' @param type plot type: 'p' for points, 'l' for lines, 'b' for both
#' @method plot sf
#' @name plot
#' @examples
#' # plot linestrings:
#' l1 = st_linestring(matrix(runif(6)-0.5,,2))
#' l2 = st_linestring(matrix(runif(6)-0.5,,2))
#' l3 = st_linestring(matrix(runif(6)-0.5,,2))
#' s = st_sf(a=2:4, b=st_sfc(l1,l2,l3))
#' plot(s, col = s$a, axes = FALSE)
#' plot(s, col = s$a)
#' ll = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' attr(s$b, "proj4string") = ll
#' plot(s, col = s$a, axes = TRUE)
#' plot(s, col = s$a, lty = s$a, lwd = s$a, pch = s$a, type = 'b')
#' l4 = st_linestring(matrix(runif(6),,2))
#' plot(st_sf(a=1,b=st_sfc(l4)), add = TRUE)
#' # plot multilinestrings:
#' ml1 = st_multilinestring(list(l1, l2))
#' ml2 = st_multilinestring(list(l3, l4))
#' ml = st_sf(a = 2:3, b = st_sfc(ml1, ml2))
#' plot(ml, col = ml$a, lty = ml$a, lwd = ml$a, pch = ml$a, type = 'b')
#' # plot points:
#' p1 = st_point(c(1,2))
#' p2 = st_point(c(3,3))
#' p3 = st_point(c(3,0))
#' p = st_sf(a=2:4, b=st_sfc(p1,p2,p3))
#' plot(p, col = s$a, axes = TRUE)
#' plot(p, col = s$a)
#' plot(p, col = p$a, pch = p$a, cex = p$a, bg = s$a, lwd = 2, lty = 2, type = 'b')
#' p4 = st_point(c(2,2))
#' plot(st_sf(a=1, st_sfc(p4)), add = TRUE)
#' # multipoints:
#' mp1 = st_multipoint(matrix(1:4,2))
#' mp2 = st_multipoint(matrix(5:8,2))
#' mp = st_sf(a = 2:3, b = st_sfc(mp1, mp2))
#' plot(mp)
#' plot(mp, col = mp$a, pch = mp$a, cex = mp$a, bg = mp$a, lwd = mp$a, lty = mp$a, type = 'b')
#' # polygon:
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pl1 = st_polygon(list(outer, hole1, hole2))
#' pl2 = st_polygon(list(outer+10, hole1+10, hole2+10))
#' po = st_sf(a = 2:3, st_sfc(pl1,pl2))
#' plot(po, col = po$a, border = rev(po$a), lwd=3)
#' # multipolygon
#' r10 = matrix(rep(c(0,10),each=5),5)
#' pl1 = list(outer, hole1, hole2)
#' pl2 = list(outer+10, hole1+10, hole2+10)
#' pl3 = list(outer+r10, hole1+r10, hole2+r10)
#' mpo1 = st_multipolygon(list(pl1,pl2))
#' mpo2 = st_multipolygon(list(pl3))
#' mpo = st_sf(a=2:3, b=st_sfc(mpo1,mpo2))
#' plot(mpo, col = mpo$a, border = rev(mpo$a), lwd = 2)
#' # geometrycollection:
#' gc1 = st_geometrycollection(list(mpo1, st_point(c(21,21)), l1 * 2 + 21))
#' gc2 = st_geometrycollection(list(mpo2, l2 - 2, l3 - 2, st_point(c(-1,-1))))
#' gc = st_sf(a=2:3, b = st_sfc(gc1,gc2))
#' plot(gc, cex = gc$a, col = gc$a, border = rev(gc$a) + 2, lwd = 2)
#' @export
plot.sf <- function(x, y, ...) {
	stopifnot(missing(y))
	plot(st_geometry(x), ...)
}

#' @name plot
#' @export
plot.sfc_POINT = function(x, y, ..., pch = 1, cex = 1, col = 1, bg = 0, lwd = 1, lty = 1,
		type = 'p', add = FALSE) {
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	npts = length(x)
	pch = rep(pch, length.out = npts)
	col = rep(col, length.out = npts)
	bg = rep(bg, length.out = npts)
	cex = rep(cex, length.out = npts)
	points(do.call(rbind, x), pch = pch, col = col, bg = bg, cex = cex, lwd = lwd, lty = lty,
		type = type)
}

#' @name plot
#' @export
plot.sfc_MULTIPOINT = function(x, y, ..., pch = 1, cex = 1, col = 1, bg = 0, lwd = 1, lty = 1,
		type = 'p', add = FALSE) {
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	n = length(x)
	pch = rep(pch, length.out = n)
	col = rep(col, length.out = n)
	bg = rep(bg, length.out = n)
	cex = rep(cex, length.out = n)
	lwd = rep(lwd, length.out = n)
	lty = rep(lty, length.out = n)
	lapply(seq_along(x), function(i) points(x[[i]], pch = pch[i], col = col[i], bg = bg[i], 
		cex = cex[i], lwd = lwd[i], lty = lty[i], type = type))
	invisible(NULL)
}

#' @name plot
#' @export
plot.sfc_LINESTRING = function(x, y, ..., lty = 1, lwd = 1, col = 1, pch = 1, type = 'l', 
		add = FALSE) {
# FIXME: take care of lend, ljoin, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	pch  = rep(pch, length.out = length(x))
	lapply(seq_along(x), function(i)
		lines(x[[i]], lty = lty[i], lwd = lwd[i], col = col[i], pch = pch[i], type = type))
	invisible(NULL)
}

#' @name plot
#' @export
plot.sfc_MULTILINESTRING = function(x, y, ..., lty = 1, lwd = 1, col = 1, pch = 1, type = 'l',
		add = FALSE) {
# FIXME: take care of lend, ljoin, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	pch  = rep(pch, length.out = length(x))
	lapply(seq_along(x), function(i)
		lapply(x[[i]], function(L)
			lines(L, lty = lty[i], lwd = lwd[i], col = col[i], pch = pch[i], type = type)))
	invisible(NULL)
}

# sf (list) -> polypath (mtrx) : rbind polygon rings with NA rows inbetween
p_bind = function(lst) {
	if (length(lst) == 1)
		return(lst[[1]])
	ret = vector("list", length(lst) * 2 - 1)
	ret[seq(1, length(lst) * 2 - 1, by = 2)] = lst # odd elements
	ret[seq(2, length(lst) * 2 - 1, by = 2)] = NA  # even elements
	do.call(rbind, ret) # replicates the NA to form an NA row
}

#' @name plot
#' @export
plot.sfc_POLYGON = function(x, y, ..., lty = 1, lwd = 1, col = NA, border = 1, add = FALSE) {
# FIXME: take care of lend, ljoin, xpd, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	border = rep(border, length.out = length(x))
	lapply(seq_along(x), function(i)
		polypath(p_bind(x[[i]]), border = border[i], lty = lty[i], lwd = lwd[i], col = col[i]))
	invisible(NULL)
}

#' @name plot
#' @export
plot.sfc_MULTIPOLYGON = function(x, y, ..., lty = 1, lwd = 1, col = NA, border = 1, add = FALSE) {
# FIXME: take care of lend, ljoin, xpd, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	border = rep(border, length.out = length(x))
	lapply(seq_along(x), function(i)
		lapply(x[[i]], function(L)
			polypath(p_bind(L), border = border[i], lty = lty[i], lwd = lwd[i], col = col[i])))
	invisible(NULL)
}

# plot single geometrycollection:
plot_gc = function(x, pch, cex, bg, border = 1, lty, lwd, col) {
	lapply(x, function(subx) {
		args = list(list(subx), pch = pch, cex = cex, bg = bg, border = border, 
			lty = lty, lwd = lwd, col = col, add = TRUE)
		fn = switch(class(subx)[2],
			POINT = plot.sfc_POINT,
			MULTIPOINT = plot.sfc_MULTIPOINT,
			LINESTRING = plot.sfc_LINESTRING,
			MULTILINESTRING = plot.sfc_MULTILINESTRING,
			POLYGON = plot.sfc_POLYGON,
			MULTIPOLYGON = plot.sfc_MULTIPOLYGON,
			GEOMETRYCOLLECTION = plot_gc,
			stop(paste("plotting of", class(x)[2], "not yet supported: please file an issue"))
		)
		do.call(fn, args)
	})
	invisible(NULL)
}

#' @name plot
#' @export
plot.sfc_GEOMETRYCOLLECTION = function(x, y, ..., pch = 1, cex = 1, bg = 0, lty = 1, lwd = 1, 
	col = 1, border = 1, add = FALSE) {
# FIXME: take care of lend, ljoin, xpd, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	cex = rep(cex, length.out = length(x))
	pch = rep(pch, length.out = length(x))
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	border = rep(border, length.out = length(x))
	lapply(seq_along(x), function(i) plot_gc(x[[i]], 
			pch = pch[i], cex = cex[i], bg = bg[i], border = border[i], lty = lty[i], 
			lwd = lwd[i], col = col[i]))
	invisible(NULL)
}

#' @name plot
#' @export
plot.sfc_GEOMETRY = function(x, y, ..., pch = 1, cex = 1, bg = 0, lty = 1, lwd = 1, 
	col = 1, border = 1, add = FALSE) {
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	cex = rep(cex, length.out = length(x))
	pch = rep(pch, length.out = length(x))
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	border = rep(border, length.out = length(x))
	plot_gc(x, pch = pch, cex = cex, bg = bg, border = border, lty = lty, 
			lwd = lwd, col = col)
	invisible(NULL)
}

#' @name plot
#' @export
plot.sfg = function(x, ...) {
	plot(st_sfc(x), ...)
}

# set up plotting area & axes; reuses sp:::plot.Spatial
plot_sf = function(x, xlim = NULL, ylim = NULL, asp = NA, axes = FALSE, bg = par("bg"), ..., 
    xaxs, yaxs, lab, setParUsrBB = FALSE, bgMap = NULL, expandBB = c(0,0,0,0)) {

	bb = matrix(st_bbox(x), 2, dimnames = list(c("x", "y"), c("min", "max")))
	if (!requireNamespace("sp", quietly = TRUE))
		stop("package sp required, please install it first")
	sp = new("Spatial", bbox = bb, proj4string = sp::CRS(attr(x, "proj4string")))
	sp::plot(sp, ..., xlim = xlim, ylim = ylim, asp = asp, axes = axes, bg = bg, 
    	xaxs = xaxs, yaxs = yaxs, lab = lab, setParUsrBB = setParUsrBB, bgMap = bgMap, 
		expandBB = expandBB)
}

