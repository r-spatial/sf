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

#' Plot sf object
#'
#' @param x object of class sf
#' @param y ignored
#' @param ... further specifications, see \link{plot}
#' @param ncol integer; default number of colors to be used.
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
#' st_crs(s) = ll
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
plot.sf <- function(x, y, ..., ncol = 10, col = NULL) {
	stopifnot(missing(y))
	dots = list(...)

	if (ncol(x) > 2) {
		cols = names(x)[names(x) != attr(x, "sf_column")]
		opar = par(mfrow = get_mfrow(st_bbox(x), length(cols), par("din")), mar = c(0,0,1,0))
		lapply(cols, function(cname) plot(x[, cname], main = cname, col = col, ...))
		par(opar)
	} else {
		if (is.null(col) && ncol(x) == 2)
			col = sf.colors(ncol, x[[1]])
		if (is.null(col))
			plot(st_geometry(x), ...)
		else 
			plot(st_geometry(x), col = col, ...)
		if (is.null(dots$main) && !isTRUE(dots$add))
			title(names(x)[names(x) != attr(x, "sf_column")])
	} 
}

#' @name plot
#' @method plot sfc_POINT
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
#' @method plot sfc_MULTIPOINT
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
#' @method plot sfc_LINESTRING
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
#' @method plot sfc_MULTILINESTRING
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
		lst[[1]]
	else {
		ret = vector("list", length(lst) * 2 - 1)
		ret[seq(1, length(lst) * 2 - 1, by = 2)] = lst # odd elements
		ret[seq(2, length(lst) * 2 - 1, by = 2)] = NA  # even elements
		do.call(rbind, ret) # replicates the NA to form an NA row
	}
}

#' @name plot
#' @param rule see \link[graphics]{polypath}
#' @method plot sfc_POLYGON
#' @export
plot.sfc_POLYGON = function(x, y, ..., lty = 1, lwd = 1, col = NA, border = 1, add = FALSE, rule = "winding") {
# FIXME: take care of lend, ljoin, xpd, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	border = rep(border, length.out = length(x))
	lapply(seq_along(x), function(i)
		polypath(p_bind(x[[i]]), border = border[i], lty = lty[i], lwd = lwd[i], col = col[i], rule = rule))
	invisible(NULL)
}

#' @name plot
#' @method plot sfc_MULTIPOLYGON
#' @export
plot.sfc_MULTIPOLYGON = function(x, y, ..., lty = 1, lwd = 1, col = NA, border = 1, add = FALSE, rule = "winding") {
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
			polypath(p_bind(L), border = border[i], lty = lty[i], lwd = lwd[i], col = col[i], rule = rule)))
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
#' @method plot sfc_GEOMETRYCOLLECTION
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
#' @method plot sfc_GEOMETRY
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
	lapply(seq_along(x), function(i) plot_gc(st_sfc(x[[i]]), 
			pch = pch[i], cex = cex[i], bg = bg[i], border = border[i], lty = lty[i], 
			lwd = lwd[i], col = col[i]))
	invisible(NULL)
}

#' @name plot
#' @method plot sfg
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
	sp = new("Spatial", bbox = bb, proj4string = sp::CRS(attr(x, "crs")$proj4string))
	sp::plot(sp, ..., xlim = xlim, ylim = ylim, asp = asp, axes = axes, bg = bg, 
    	xaxs = xaxs, yaxs = yaxs, lab = lab, setParUsrBB = setParUsrBB, bgMap = bgMap, 
		expandBB = expandBB)
}


#' blue-pink-yellow color scale
#'
#' blue-pink-yellow color scale
#' @param n integer; number of colors
#' @param cutoff.tails numeric, in [0,0.5] start and end values
#' @param alpha numeric, in [0,1], transparency
#' @param categorical logical; should a categorical color ramp be returned? if \code{x} is a factor, yes.
#' @param xc factor or numeric vector, for which colors need to be returned
#' @name plot
#' @export
#' @details \code{sf.colors} was taken from \link[sp]{bpy.colors}, with modified \code{cutoff.tails} defaults; for categorical, colors were taken from \code{http://www.colorbrewer2.org/} (if n < 9, Set2, else Set3).
#' @examples
#' sf.colors(10)
sf.colors = function (n = 10, xc, cutoff.tails = c(0.35, 0.2), alpha = 1, categorical = FALSE) {
	if (missing(xc) || length(xc) == 1) {
		if (missing(n))
			n = xc
		if (categorical) {
			cb = if (n <= 8)
			# 8-class Set2:
			c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494','#b3b3b3')
			# 12-class Set3:
			else c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')
			rep(cb, length.out = n)
		} else {
			i = seq(0.5 * cutoff.tails[1], 1 - 0.5 * cutoff.tails[2], length = n)
    		r = ifelse(i < .25, 0, ifelse(i < .57, i / .32 - .78125, 1))
    		g = ifelse(i < .42, 0, ifelse(i < .92, 2 * i - .84, 1))
    		b = ifelse(i < .25, 4 * i, ifelse(i < .42, 1,
        		ifelse(i < .92, -2 * i + 1.84, i / .08 - 11.5)))
    		rgb(r, g, b, alpha)
		}
	} else {
		if (is.factor(xc))
			sf.colors(nlevels(xc), categorical = TRUE)[as.numeric(xc)]
		else {
			safe_cut = function(x,n) { 
				if (all(is.na(x)) || all(range(x, na.rm = TRUE) == 0))
					rep(1, length(x))
				else
					cut(x, n)
			}
			sf.colors(n)[safe_cut(xc, n)]
		}
	}
}

get_mfrow = function(bb, n, total_size = c(1,1)) {
	asp = diff(bb[c(1,3)])/diff(bb[c(2,4)])
	size = function(nrow, n, asp) {
		ncol = ceiling(n / nrow)
		xsize = total_size[1] / ncol
		ysize = xsize  / asp
		if (xsize * ysize * n > prod(total_size)) {
			ysize = total_size[2] / nrow
			xsize = ysize * asp
		}
		xsize * ysize
	}
	sz = sapply(1:n, function(x) size(x, n, asp))
	nrow = which.max(sz)
	ncol = ceiling(n / nrow)
	structure(c(nrow, ncol), names = c("nrow", "ncol"))
}
