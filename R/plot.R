#' Plot sf object
#'
#' @param x object of class sf
#' @param y ignored
#' @param ... further specifications, see \link{plot_sf} and \link{plot}
#' @param main title for plot (\code{NULL} to remove)
#' @param pal palette function, similar to \link[grDevices]{rainbow}; if omitted, \link{sf.colors} is used
#' @param nbreaks number of colors breaks (ignored for \code{factor} or \code{character} variables)
#' @param breaks either a numeric vector with the actual breaks, or a name of a method accepted by the \code{style} argument of \link[classInt]{classIntervals}
#' @param max.plot integer; lower boundary to maximum number of attributes to plot; the default value (9) can be overriden by setting the global option \code{sf_max.plot}, e.g. \code{options(sf_max.plot=2)}
#' @param key.pos integer; which side to plot a color key: 1 bottom, 2 left, 3 top, 4 right. Set to \code{NULL} for no key. Currently ignored if multiple columns are plotted.
#' @param key.size amount of space reserved for the key (labels)
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
#' @details \code{plot.sf} maximally plots \code{max.plot} maps with colors following from attribute columns,
#' one map per attribute. It uses \code{sf.colors} for default colors. For more control over individual maps,
#' set parameter \code{mfrow} with \code{par} prior to plotting,  and plot single maps one by one.
#'
#' \code{plot.sfc} plots the geometry, additional parameters can be passed on
#' to control color, lines or symbols.
#'
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
plot.sf <- function(x, y, ..., col = NULL, main, pal = NULL, nbreaks = 10, breaks = "pretty", 
		max.plot = if(is.null(n <- options("sf_max.plot")[[1]])) 9 else n, 
		key.pos = if (ncol(x) > 2) NULL else 4, key.size = lcm(1.8)) {
	stopifnot(missing(y))
	breaks.missing = missing(breaks)
	dots = list(...)

	if (ncol(x) > 2 && !isTRUE(dots$add)) { # multiple maps to plot...
		max_plot_missing = missing(max.plot)
		cols = setdiff(names(x), attr(x, "sf_column"))
		mfrow = get_mfrow(st_bbox(x), min(max.plot, length(cols)), par("din"))
		opar = if (isTRUE(dots$axes))
				par(mfrow = mfrow, mar = c(2.1, 2.1, 1.2, 0))
			else
				par(mfrow = mfrow, mar = c(0,0,1.2,0))
		on.exit(par(opar))

		if (max_plot_missing)
			max.plot = prod(mfrow)

		if (isTRUE(is.finite(max.plot)) && ncol(x) - 1 > max.plot) {
			if (max_plot_missing && is.null(options("sf_max.plot")[[1]]))
				warning(paste("plotting the first", max.plot, "out of", ncol(x)-1,
					"attributes; use max.plot =", ncol(x) - 1, "to plot all"), call. = FALSE)
			x = x[, 1:max.plot]
		}
		# col selection may have changed; set cols again:
		cols = setdiff(names(x), attr(x, "sf_column"))
		# loop over each map to plot:
		invisible(lapply(cols, function(cname) plot(x[, cname], main = cname, col = col,
			pal = pal, nbreaks = nbreaks, breaks = breaks, key.pos = NULL, ...)))
	} else { # single map, or dots$add=TRUE:
		if (is.null(col) && ncol(x) == 1) # no colors, no attributes to choose colors from: plot geometry
			plot(st_geometry(x), ...)
		else { # generate plot with colors and possibly key
			if (ncol(x) > 2) { # add = TRUE
				warning("ignoring all but the first attribute")
				x = x[,1]
			}

			# store attribute in "values":
			values = x[[setdiff(names(x), attr(x, "sf_column"))]]

			if (inherits(values, "POSIXt"))
				values = as.numeric(values)

			if (is.character(values))
				values = as.factor(values)

			if (is.null(pal))
				pal = function(n) sf.colors(n, categorical = is.factor(values))
			else if (! is.null(col))
				stop("specify only one of col and pal")

			if (is.null(col)) { # compute colors from values:
				col = if (is.factor(values)) {
						colors = if (is.function(pal))
								pal(nlevels(values))
							else
								pal
						colors[as.numeric(values)]
					} else {
						if (is.character(breaks)) { # compute breaks from values:
							breaks = if (! all(is.na(values)) && length(unique(na.omit(values))) > 1)
								classInt::classIntervals(values, nbreaks, breaks)$brks
							else
								range(values, na.rm = TRUE) # lowest and highest!
						}
						# this is necessary if breaks were specified either as character or as numeric
						# "pretty" takes nbreaks as advice only:
						nbreaks = length(breaks) - 1
						cuts = if (all(is.na(values)))
								rep(NA_integer_, length(values))
							else if (diff(range(values, na.rm = TRUE)) == 0)
								values * 0 + 1  # preserves NA's
							else
								cut(as.numeric(values), breaks, include.lowest = TRUE)
						colors = if (is.function(pal))
								pal(nbreaks)
							else
								pal
						colors[cuts]
					}
			} else { 
				if (is.factor(values)) {
					which.first = function(x) which(x)[1]
					fnum = as.numeric(values)
					colors = col[ sapply(unique(fnum), function(i) which.first(i == fnum)) ]
				} else # no key:
					key.pos = NULL
			}
			
			if (! is.null(key.pos) && !all(is.na(values)) &&
					(is.factor(values) || length(unique(na.omit(values))) > 1) &&
					length(col) > 1) { # plot key?
				switch(key.pos,
					layout(matrix(c(2,1), nrow = 2, ncol = 1), widths = 1, heights = c(1, key.size)),  # 1 bottom
					layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(key.size, 1), heights = 1),  # 2 left
					layout(matrix(c(1,2), nrow = 2, ncol = 1), widths = 1, heights = c(key.size, 1)),  # 3 top
					layout(matrix(c(2,1), nrow = 1, ncol = 2), widths = c(1, key.size), heights = 1)   # 4 right
				)
				if (is.factor(values)) {
					image.scale.factor(levels(values), colors, key.pos = key.pos, 
						axes = isTRUE(dots$axes), key.size = key.size)
				} else
					image.scale(values, colors, breaks = breaks, key.pos = key.pos, axes = isTRUE(dots$axes))
			}
			# plot the map:
			mar = c(1, 1, 1.2, 1)
			if (isTRUE(dots$axes))
				mar[1:2] = 2.1
			par(mar = mar)
			plot(st_geometry(x), col = col, ...)
		}
		if (! isTRUE(dots$add)) { # title?
			if (missing(main))
				title(setdiff(names(x), attr(x, "sf_column")))
			else
				title(main)
		}
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
	mat = do.call(rbind, x)
	if (!is.null(mat)) {
		ne = apply(mat, 1, function(x) all(is.finite(x))) # ne: not empty
		points(mat[ne,, drop = FALSE], pch = pch[ne], col = col[ne], bg = bg[ne],
			cex = cex[ne], lwd = lwd, lty = lty, type = type)
	}
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
	non_empty = !is.na(st_dimension(x))
	lapply(seq_along(x), function(i)
	  if (non_empty[i])
		points(x[[i]], pch = pch[i], col = col[i], bg = bg[i],
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
	non_empty = !is.na(st_dimension(x))
	lapply(seq_along(x), function(i)
	  if (non_empty[i])
		lines(x[[i]], lty = lty[i], lwd = lwd[i], col = col[i], pch = pch[i], type = type))
	invisible(NULL)
}

#' @name plot
#' @method plot sfc_CIRCULARSTRING
#' @export
plot.sfc_CIRCULARSTRING = function(x, y, ...) {
	plot(st_cast(x, "LINESTRING"),  ...)
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
	non_empty = !is.na(st_dimension(x))
	lapply(seq_along(x), function(i)
	  if (non_empty[i])
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
#' @export
plot.sfc_POLYGON = function(x, y, ..., lty = 1, lwd = 1, col = NA, cex = 1, pch = NA, border = 1,
		add = FALSE, rule = "winding") {
# FIXME: take care of lend, ljoin, xpd, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	border = rep(border, length.out = length(x))
	non_empty = !is.na(st_dimension(x))
	lapply(seq_along(x), function(i)
	  if (non_empty[i])
		polypath(p_bind(x[[i]]), border = border[i], lty = lty[i], lwd = lwd[i], col = col[i], rule = rule))
#	if (any(!is.na(pch))) {
#		pch = rep(pch, length.out = length(x))
#		cex = rep(cex, length.out = length(x))
#		lapply(seq_along(x), function(i)
#		  if (non_empty[i])
#			points(p_bind(x[[i]]), pch = pch[i], cex = cex[i], type = 'p'))
#	}
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
	non_empty = !is.na(st_dimension(x))
	lapply(seq_along(x), function(i)
	  if (non_empty[i])
		lapply(x[[i]], function(L)
			polypath(p_bind(L), border = border[i], lty = lty[i], lwd = lwd[i], col = col[i], rule = rule)))
	invisible(NULL)
}

# plot single geometrycollection:
plot_gc = function(x, pch, cex, bg, border = 1, lty, lwd, col, add) {
	lapply(x, function(subx) {
		args = list(st_sfc(subx), pch = pch, cex = cex, bg = bg, border = border,
			lty = lty, lwd = lwd, col = col, add = TRUE)
		fn = switch(class(subx)[2],
			POINT = plot.sfc_POINT,
			MULTIPOINT = plot.sfc_MULTIPOINT,
			LINESTRING = plot.sfc_LINESTRING,
			MULTILINESTRING = plot.sfc_MULTILINESTRING,
			POLYGON = plot.sfc_POLYGON,
			CIRCULARSTRING = plot.sfc_CIRCULARSTRING,
			MULTIPOLYGON = plot.sfc_MULTIPOLYGON,
			MULTISURFACE = plot.sfc_GEOMETRYCOLLECTION,
			CURVEPOLYGON = plot.sfc_GEOMETRYCOLLECTION,
			COMPOUNDCURVE = plot.sfc_GEOMETRYCOLLECTION,
			GEOMETRYCOLLECTION = plot.sfc_GEOMETRYCOLLECTION,
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
#' @name plot
#' @param xlim see \link{plot.window}
#' @param ylim see \link{plot.window}
#' @param asp see below, and see \link{par}
#' @param axes logical; should axes be plotted? (default FALSE)
#' @param bgc background color
#' @param xaxs see \link{par}
#' @param yaxs see \link{par}
#' @param lab see \link{par}
#' @param setParUsrBB default FALSE; set the \code{par} \dQuote{usr} bounding box; see below
#' @param bgMap object of class \code{ggmap}, or returned by function \code{RgoogleMaps::GetMap}
#' @param expandBB numeric; fractional values to expand the bounding box with,
#' in each direction (bottom, left, top, right)
#' @param graticule logical, or object of class \code{crs} (e.g., \code{st_crs(4326)} for a WGS84 graticule), or object created by \link{st_graticule}; \code{TRUE} will give the WGS84 graticule
#' or object returned by \link{st_graticule}
#' @param col_graticule color to used for the graticule (if present)
#' @export
#' @details \code{plot_sf} sets up the plotting area, axes, graticule, or webmap background; it
#' is called by all \code{plot} methods before anything is drawn.
#'
#' The argument \code{setParUsrBB} may be used to pass the logical value \code{TRUE} to functions within \code{plot.Spatial}. When set to \code{TRUE}, par(\dQuote{usr}) will be overwritten with \code{c(xlim, ylim)}, which defaults to the bounding box of the spatial object. This is only needed in the particular context of graphic output to a specified device with given width and height, to be matched to the spatial object, when using par(\dQuote{xaxs}) and par(\dQuote{yaxs}) in addition to \code{par(mar=c(0,0,0,0))}.
#'
#' The default aspect for map plots is 1; if however data are not
#' projected (coordinates are long/lat), the aspect is by default set to
#' 1/cos(My * pi)/180) with My the y coordinate of the middle of the map
#' (the mean of \code{ylim}, which defaults to the y range of bounding box). This
#' implies an \href{https://en.wikipedia.org/wiki/Equirectangular_projection}{Equirectangular projection}.
#'
plot_sf = function(x, xlim = NULL, ylim = NULL, asp = NA, axes = FALSE, bgc = par("bg"), ...,
    xaxs, yaxs, lab, setParUsrBB = FALSE, bgMap = NULL, expandBB = c(0,0,0,0), graticule = NA_crs_,
	col_graticule = 'grey') {

# sp's bbox: matrix
#   min max
# x
# y
	bbox = matrix(st_bbox(x), 2, dimnames = list(c("x", "y"), c("min", "max")))
	# expandBB: 1=below, 2=left, 3=above and 4=right.
	expBB = function(lim, expand) c(lim[1] - expand[1] * diff(lim), lim[2] + expand[2] * diff(lim))
	if (is.null(xlim))
		xlim <- expBB(bbox[1,], expandBB[c(2,4)])
	if (is.null(ylim))
		ylim <- expBB(bbox[2,], expandBB[c(1,3)])
	if (is.na(asp))
		asp <- ifelse(isTRUE(st_is_longlat(x)), 1/cos((mean(ylim) * pi)/180), 1.0)

	plot.new()

	args = list(xlim = xlim, ylim = ylim, asp = asp)
	if (!missing(xaxs)) args$xaxs = xaxs
	if (!missing(yaxs)) args$yaxs = yaxs
	if (!missing(lab)) args$lab = lab
	do.call(plot.window, args)

	if (setParUsrBB)
		par(usr=c(xlim, ylim))
	pl_reg <- par("usr")
	rect(xleft = pl_reg[1], ybottom = pl_reg[3], xright = pl_reg[2],
		ytop = pl_reg[4], col = bgc, border = FALSE)
	linAxis = function(side, ..., lon, lat, ndiscr) axis(side = side, ...)
	if (! missing(graticule)) {
		g = if (isTRUE(graticule))
				st_graticule(pl_reg[c(1,3,2,4)], st_crs(x), st_crs(4326), ...)
			else if (inherits(graticule, "crs") && !is.na(graticule))
				st_graticule(pl_reg[c(1,3,2,4)], st_crs(x), graticule, ...)
			else
				graticule
		plot(st_geometry(g), col = col_graticule, add = TRUE)
		box()
		if (axes) {
			sel = g$type == "E" & g$plot12
			linAxis(1L, g$x_start[sel], parse(text = g$degree_label[sel]), ...)
			sel = g$type == "N" & g$plot12
			linAxis(2L, g$y_start[sel], parse(text = g$degree_label[sel]), ...)
		}
	} else if (axes) {
		box()
		if (isTRUE(st_is_longlat(x))) {
			degAxis(1, ...)
			degAxis(2, ...)
		} else {
			linAxis(1, ...)
			linAxis(2, ...)
		}
	}
	localTitle <- function(..., col, bgc, pch, cex, lty, lwd, lon, lat, ndiscr, at, labels) title(...)
	localTitle(...)
	if (!is.null(bgMap)) {
		mercator = FALSE
		if (inherits(bgMap, "ggmap")) {
			bb = bb2merc(bgMap, "ggmap")
			mercator = TRUE
		} else if (all(c("lat.center","lon.center","zoom","myTile","BBOX") %in% names(bgMap))) {
			# an object returned by RgoogleMaps::GetMap
			bb = bb2merc(bgMap, "RgoogleMaps")
			bgMap = bgMap$myTile
			mercator = TRUE
		} else
			bb = c(xlim[1], ylim[1], xlim[2], ylim[2]) # can be any crs!
		if (mercator &&  st_crs(x) != st_crs(3857))
			warning("crs of plotting object differs from that of bgMap, which is assumed to be st_crs(3857)") # nocov
		rasterImage(bgMap, bb[1], bb[2], bb[3], bb[4], interpolate = FALSE)
	}
}


#' blue-pink-yellow color scale
#'
#' blue-pink-yellow color scale
#' @param n integer; number of colors
#' @param cutoff.tails numeric, in [0,0.5] start and end values
#' @param alpha numeric, in [0,1], transparency
#' @param categorical logical; do we want colors for a categorical variable? (see details)
#' @name plot
#' @export
#' @details non-categorical colors from \code{sf.colors} were taken from \link[sp]{bpy.colors}, with modified \code{cutoff.tails} defaults
#' If categorical is \code{TRUE}, default colors are from \url{http://www.colorbrewer2.org/} (if n < 9, Set2, else Set3).
#' @examples
#' sf.colors(10)
sf.colors = function (n = 10, cutoff.tails = c(0.35, 0.2), alpha = 1, categorical = FALSE) {
	if (categorical) {
		cb = if (n <= 8)
		# 8-class Set2:
		c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f','#e5c494','#b3b3b3')
		# 12-class Set3:
		else c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')
		# TODO: deal with alpha
		rep(cb, length.out = n)
	} else {
		i = seq(0.5 * cutoff.tails[1], 1 - 0.5 * cutoff.tails[2], length = n)
   		r = ifelse(i < .25, 0, ifelse(i < .57, i / .32 - .78125, 1))
   		g = ifelse(i < .42, 0, ifelse(i < .92, 2 * i - .84, 1))
   		b = ifelse(i < .25, 4 * i, ifelse(i < .42, 1,
			ifelse(i < .92, -2 * i + 1.84, i / .08 - 11.5)))
		rgb(r, g, b, alpha)
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
	sz = vapply(1:n, function(x) size(x, n, asp), 0.0)
	nrow = which.max(sz)
	ncol = ceiling(n / nrow)
	structure(c(nrow, ncol), names = c("nrow", "ncol"))
}


bb2merc = function(x, cls = "ggmap") { # return bbox in the appropriate "web mercator" CRS
	wgs84 = st_crs(4326)
	merc =  st_crs(3857) # http://wiki.openstreetmap.org/wiki/EPSG:3857
	pts = if (cls == "ggmap") {
		b = vapply(attr(x, "bb"), c, 0.0)
		st_sfc(st_point(c(b[2:1])), st_point(c(b[4:3])), crs = wgs84)
	} else if (cls == "RgoogleMaps")
		st_sfc(st_point(rev(x$BBOX$ll)), st_point(rev(x$BBOX$ur)), crs = wgs84)
	else
		stop("unknown cls")
	st_bbox(st_transform(pts, merc))
}

degAxis = function (side, at, labels, ..., lon, lat, ndiscr) {
	if (missing(at))
       	at = axTicks(side)
	if (missing(labels)) {
		labels = FALSE
		if (side == 1 || side == 3)
			labels = parse(text = degreeLabelsEW(at))
		else if (side == 2 || side == 4)
			labels = parse(text = degreeLabelsNS(at))
	}
	axis(side, at = at, labels = labels, ...)
}

image.scale = function(z, col, breaks = NULL, key.pos, add.axis = TRUE,
	at = NULL, ..., axes = FALSE) {
	if (!is.null(breaks) && length(breaks) != (length(col) + 1))
		stop("must have one more break than colour")
	zlim = range(z, na.rm=TRUE)
	if (is.null(breaks))
		breaks = seq(zlim[1], zlim[2], length.out = length(col) + 1)
	if (key.pos %in% c(1,3)) {
		ylim = c(0, 1)
		xlim = range(breaks)
		mar = c(0, ifelse(axes, 2.1, 1), 0, 1)
	}
	if (key.pos %in% c(2,4)) {
		ylim = range(breaks)
		xlim = c(0, 1)
		mar = c(ifelse(axes, 2.1, 1), 0, 1.2, 0)
	}
	mar[key.pos] = 2.1
	par(mar = mar)

	poly = vector(mode="list", length(col))
	for (i in seq(poly))
		poly[[i]] = c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	plot(1, 1, t = "n", ylim = ylim, xlim = xlim, axes = FALSE,
		xlab = "", ylab = "", xaxs = "i", yaxs = "i", ...)
	for(i in seq_along(poly)) {
		if (key.pos %in% c(1,3))
			polygon(poly[[i]], c(0, 0, 1, 1), col=col[i], border=NA)
		if (key.pos %in% c(2,4))
			polygon(c(0, 0, 1, 1), poly[[i]], col=col[i], border=NA)
	}

	box()
	if (add.axis)
		axis(key.pos, at)
}

image.scale.factor = function(z, col, breaks = NULL, key.pos, add.axis = TRUE,
	at = NULL, ..., axes = FALSE, key.size) {

	n = length(z)
	ksz = as.numeric(gsub(" cm", "", key.size)) * 2
	breaks = (0:n) + 0.5
	if (key.pos %in% c(1,3)) {
		ylim = c(0, 1)
		xlim = range(breaks)
		mar = c(0, ifelse(axes, 2.1, 1), 0, 1)
		mar[key.pos] = 2.1
	} else {
		ylim = range(breaks)
		xlim = c(0, 1)
		mar = c(ifelse(axes, 2.1, 1), 0, 1.2, 0)
		#mar[key.pos] = 2.1
		mar[key.pos] = ksz - 1.3
	}
	par(mar = mar)

	poly = vector(mode="list", length(col))
	for (i in seq(poly))
		poly[[i]] = c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	plot(1, 1, t = "n", ylim = ylim, xlim = xlim, axes = FALSE,
		xlab = "", ylab = "", xaxs = "i", yaxs = "i", ...)
	for(i in seq_along(poly)) {
		if (key.pos %in% c(1,3))
			polygon(poly[[i]], c(0, 0, 1, 1), col = col[i], border = NA)
		if (key.pos %in% c(2,4))
			polygon(c(0, 0, 1, 1), poly[[i]], col = col[i], border = NA)
	}

	box()
	if (add.axis) {
		opar = par(las = 1)
		axis(key.pos, at = 1:n, labels = z)
		par(opar)
	}
}
