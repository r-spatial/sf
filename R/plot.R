#' plot sf object
#'
#' plot one or more attributes of an sf object on a map
#' Plot sf object
#'
#' @param x object of class sf
#' @param y ignored
#' @param ... further specifications, see \link{plot_sf} and \link{plot} and details.
#' @param main title for plot (\code{NULL} to remove)
#' @param pal palette function, similar to \link{rainbow}, or palette values; if omitted, \code{sf.colors} is used
#' @param nbreaks number of colors breaks (ignored for \code{factor} or \code{character} variables)
#' @param breaks either a numeric vector with the actual breaks, or a name of a method accepted by the \code{style} argument of \link[classInt]{classIntervals}
#' @param max.plot integer; lower boundary to maximum number of attributes to plot; the default value (9) can be overriden by setting the global option \code{sf_max.plot}, e.g. \code{options(sf_max.plot=2)}
#' @param key.pos integer; side to plot a color key: 1 bottom, 2 left, 3 top, 4 right; set to \code{NULL} to omit key completely, 0 to only not plot the key, or -1 to select automatically. If multiple columns are plotted in a single function call by default no key is plotted and every submap is stretched individually; if a key is requested (and \code{col} is missing) all maps are colored according to a single key. Auto select depends on plot size, map aspect, and, if set, parameter \code{asp}.
#' @param key.width amount of space reserved for the key (incl. labels), thickness/width of the scale bar
#' @param key.length amount of space reserved for the key along its axis, length of the scale bar
#' @param pch plotting symbol
#' @param cex symbol size
#' @param bg symbol background color
#' @param lty line type
#' @param lwd line width
#' @param col color for plotting features; if \code{length(col)} does not equal 1 or \code{nrow(x)}, a warning is emitted that colors will be recycled. Specifying \code{col} suppresses plotting the legend key.
#' @param border color of polygon border(s); using \code{NA} hides them
#' @param add logical; add to current plot? Note that when using \code{add=TRUE}, you may have to set \code{reset=FALSE} in the first plot command.
#' @param type plot type: 'p' for points, 'l' for lines, 'b' for both
#' @param reset logical; if \code{FALSE}, keep the plot in a mode that allows adding further map elements; if \code{TRUE} restore original mode after plotting \code{sf} objects with attributes; see details.
#' @param logz logical; if \code{TRUE}, use log10-scale for the attribute variable. In that case, \code{breaks} and \code{at} need to be given as log10-values; see examples.
#' @param extent object with an \code{st_bbox} method to define plot extent; defaults to \code{x}
#' @param xlim numeric; x-axis limits; overrides \code{extent}
#' @param ylim numeric; y-axis limits; overrides \code{extent}
#' @method plot sf
#' @name plot
#' @details \code{plot.sf} maximally plots \code{max.plot} maps with colors following from attribute columns,
#' one map per attribute. It uses \code{sf.colors} for default colors. For more control over placement of individual maps,
#' set parameter \code{mfrow} with \link{par} prior to plotting, and plot single maps one by one; note that this only works
#' in combination with setting parameters \code{key.pos=NULL} (no legend) and \code{reset=FALSE}.
#'
#' \code{plot.sfc} plots the geometry, additional parameters can be passed on
#' to control color, lines or symbols.
#'
#' When setting \code{reset} to \code{FALSE}, the original device parameters are lost, and the device must be reset using \code{dev.off()} in order to reset it.
#'
#' parameter \code{at} can be set to specify where labels are placed along the key; see examples.
#'
#' @examples
#' nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE)
#' # plot single attribute, auto-legend:
#' plot(nc["SID74"])
#' # plot multiple:
#' plot(nc[c("SID74", "SID79")]) # better use ggplot2::geom_sf to facet and get a single legend!
#' # adding to a plot of an sf object only works when using reset=FALSE in the first plot:
#' plot(nc["SID74"], reset = FALSE)
#' plot(st_centroid(st_geometry(nc)), add = TRUE)
#' # log10 z-scale:
#' plot(nc["SID74"], logz = TRUE, breaks = c(0,.5,1,1.5,2), at = c(0,.5,1,1.5,2))
#' # and we need to reset the plotting device after that, e.g. by
#' layout(1)
#' # when plotting only geometries, the reset=FALSE is not needed:
#' plot(st_geometry(nc))
#' plot(st_geometry(nc)[1], col = 'red', add = TRUE)
#' # add a custom legend to an arbitray plot:
#' layout(matrix(1:2, ncol = 2), widths = c(1, lcm(2)))
#' plot(1)
#' .image_scale(1:10, col = sf.colors(9), key.length = lcm(8), key.pos = 4, at = 1:10)
#' @export
plot.sf <- function(x, y, ..., main, pal = NULL, nbreaks = 10, breaks = "pretty",
		max.plot = if(is.null(n <- getOption("sf_max.plot"))) 9 else n,
		key.pos = get_key_pos(x, ...), key.length = .618, key.width = lcm(1.8),
		reset = TRUE, logz = FALSE, extent = x, xlim = st_bbox(extent)[c(1,3)],
		ylim = st_bbox(extent)[c(2,4)]) {

	stopifnot(missing(y))
	nbreaks.missing = missing(nbreaks)
	key.pos.missing = missing(key.pos)
	max_plot_missing = missing(max.plot)
	dots = list(...)
	col_missing = is.null(dots$col)
	breaks_numeric = is.numeric(breaks)

	x = swap_axes_if_needed(x)

	opar = par()
	if (ncol(x) > 2 && !isTRUE(dots$add)) { # multiple maps to plot...
		cols = setdiff(names(x), attr(x, "sf_column"))
		lt = .get_layout(st_bbox(x), min(max.plot, length(cols)), par("din"), key.pos, key.width)
		key.pos = lt$key.pos
		layout(lt$m, widths = lt$widths, heights = lt$heights, respect = FALSE)

		if (isTRUE(dots$axes))
			par(mar = c(2.1, 2.1, 1.2, 0))
		else
			par(mar = c(0, 0, 1.2, 0))

		if (max_plot_missing)
			max.plot = prod(lt$mfrow)

		if (isTRUE(is.finite(max.plot)) && ncol(x) - 1 > max.plot &&
				max_plot_missing && is.null(options("sf_max.plot")[[1]]))
			warning(paste("plotting the first", max.plot, "out of", ncol(x)-1,
				"attributes; use max.plot =", ncol(x) - 1, "to plot all"), call. = FALSE)

		# col selection may have changed; set cols again:
		cols = setdiff(names(x), attr(x, "sf_column"))
		if (length(cols) > max.plot)
			cols = cols[1:max.plot]

		if (!is.null(key.pos)) {
			values = do.call(c, as.data.frame(x)[cols])
			if (logz)
				values = log10(values)
			if (is.character(breaks)) { # compute breaks from values:
				v0 = values[!is.na(values)]
				n.unq = length(unique(v0))
				breaks = if (! all(is.na(values)) && n.unq > 1)
						classInt::classIntervals(v0, min(nbreaks, n.unq),
							breaks, warnSmallN = FALSE)$brks
					else
						range(values, na.rm = TRUE) # lowest and highest!
				nbreaks = length(breaks) - 1
			}
		}
		if (missing(nbreaks) && is.numeric(breaks))
			nbreaks = length(breaks) - 1

		# loop over each map to plot:
		lapply(cols, function(cname) plot(x[, cname], main = cname,
			pal = pal, nbreaks = nbreaks, breaks = breaks, key.pos = NULL, reset = FALSE,
			logz = logz, xlim = xlim, ylim = ylim,...))

		for (i in seq_len(prod(lt$mfrow) - length(cols))) # empty panels:
			plot.new()

		# plot key?
		if (!is.null(key.pos) && key.pos != 0 && col_missing) {
			if (is.null(pal))
				pal = function(n) sf.colors(n, categorical = is.factor(values))
			colors = if (is.function(pal))
					pal(nbreaks)
				else
					pal
			if (is.factor(values))
				.image_scale_factor(levels(values), colors, key.pos = key.pos,
					key.width = key.width, key.length = key.length, ...)
			else
				.image_scale(values, colors, breaks = breaks, key.pos = key.pos,
					key.length = key.length, logz = logz, ...)
		}

	} else { # single map, or dots$add == TRUE:
		if (!isTRUE(dots$add) && reset)
			layout(matrix(1)) # reset
		if (ncol(x) == 1) # no attributes to choose colors from: plot geometry
			plot(st_geometry(x), xlim = xlim, ylim = ylim, ...)
		else { # generate plot with colors and possibly key
			if (ncol(x) > 2) { # add = TRUE
				warning("ignoring all but the first attribute")
				x = x[,1]
			}
			# store attribute in "values":
			values = x[[setdiff(names(x), attr(x, "sf_column"))]]

			if (is.list(values))
				stop("plotting list-columns not supported") # nocov

			if (is.character(values))
				values = as.factor(values)
			else if (logz)
				values = log10(as.numeric(values))

			if (is.null(pal))
				pal = function(n) sf.colors(n, categorical = is.factor(values))
			else if (! col_missing)
				stop("specify only one of col and pal")

			if (col_missing) { # compute colors from values:
				col = if (is.factor(values)) {
						if (key.pos.missing && nlevels(values) > 30) # doesn't make sense:
							key.pos = NULL
						colors = if (is.function(pal))
								pal(nlevels(values))
							else
								pal
						colors[as.numeric(values)]
					} else {
						if (! inherits(values, c("POSIXt", "Date")))
							values = as.numeric(values) # drop units, if any
						if (is.character(breaks)) { # compute breaks from values:
							v0 = values[!is.na(values)]
							n.unq = length(unique(v0))
							breaks = if (! all(is.na(values)) && n.unq > 1)
									classInt::classIntervals(v0, min(nbreaks, n.unq),
										breaks, warnSmallN = FALSE)$brks
								else
									range(values, na.rm = TRUE) # lowest and highest!
						}
						# this is necessary if breaks were specified either as character or as numeric
						# "pretty" takes nbreaks as advice only:
						nbreaks = length(breaks) - 1

						cuts = if (all(is.na(values)))
								rep(NA_integer_, length(values))
							else if (!breaks_numeric && diff(range(values, na.rm = TRUE)) == 0)
								ifelse(is.na(values), NA_integer_, 1L)
							else
								cut(values, breaks, include.lowest = TRUE)
						colors = if (is.function(pal))
								pal(nbreaks)
							else
								pal
						colors[cuts]
					}
			} else {
				col = dots$col
				if (length(col) != 1 && length(col) != nrow(x))
					warning("col is not of length 1 or nrow(x): colors will be recycled; use pal to specify a color palette")
				key.pos = NULL # no key!
			}

			if (!isTRUE(dots$add) && !is.null(key.pos) && !all(is.na(values)) &&
					(is.factor(values) || length(unique(na.omit(values))) > 1 || breaks_numeric) && # 2065
					length(col) > 1) { # plot key?

				switch(key.pos,
					layout(matrix(c(2,1), nrow = 2, ncol = 1),
						widths = 1, heights = c(1, key.width)), # 1 bottom
					layout(matrix(c(1,2), nrow = 1, ncol = 2),
						widths = c(key.width, 1), heights = 1), # 2 left
					layout(matrix(c(1,2), nrow = 2, ncol = 1),
						widths = 1, heights = c(key.width, 1)), # 3 top
					layout(matrix(c(2,1), nrow = 1, ncol = 2),
						widths = c(1, key.width), heights = 1)  # 4 right
				)

				if (is.factor(values)) {
					.image_scale_factor(levels(values), colors, key.pos = key.pos,
						key.width = key.width, key.length = key.length, ...)
				} else
					.image_scale(values, colors, breaks = breaks, key.pos = key.pos,
						key.length = key.length, logz = logz, ...)
			}
			# plot the map:
			if (!isTRUE(dots$add)) {
				mar = c(1, 1, 1.2, 1)
				if (isTRUE(dots$axes))
					mar[1:2] = 2.1
				par(mar = mar)
			}
			if (col_missing)
				plot(st_geometry(x), col = col, xlim = xlim, ylim = ylim, ...)
			else
				plot(st_geometry(x), xlim = xlim, ylim = ylim, ...)
		}
		if (! isTRUE(dots$add)) { # title?
			if (missing(main)) {
				main = setdiff(names(x), attr(x, "sf_column"))
				if (length(main) && inherits(x[[main]], "units"))
					main = make_unit_label(main, x[[main]])
			}
			localTitle <- function(..., extent, col, bg, pch, cex, lty, lwd, axes, type, bgMap,
					border, graticule, xlim, ylim, asp, bgc, xaxs, yaxs, lab, setParUsrBB,
					expandBB, col_graticule, at, lon, lat, crs, datum, ndiscr, margin) # absorb
				title(...)
			localTitle(main, ...)
		}
	}
	if (!isTRUE(dots$add) && reset && ncol(x) > 1) { # reset device:
		layout(matrix(1))
		desel = which(names(opar) %in% c("cin", "cra", "csi", "cxy", "din", "page", "fig"))
		par(opar[-desel])
	}
}

swap_axes_if_needed = function(x) {
	crs = st_crs(x)
	if (st_axis_order() && !is.na(crs) && crs$yx)
		st_transform(x, pipeline = "+proj=pipeline +step +proj=axisswap +order=2,1")
	else
		x
}


#' @name plot
#' @export
get_key_pos = function(x, ...) {
	bb = st_bbox(x)
	if (any(is.na(bb)) || (inherits(x, "sf") && ncol(x) > 2))
		NULL
	else {
		pin = par("pin") # (width, height)
		asp_plt = pin[2]/pin[1] # y/x: < 1 means wide
		asp_box = diff(bb[c(4,2)]) / diff(bb[c(3,1)])
		asp = list(...)$asp
		if (is.null(asp))
			asp <- ifelse(isTRUE(st_is_longlat(x)), 1/cos((mean(bb[c(2,4)]) * pi)/180), 1.0)
		asp_box = asp_box * asp
		if (!is.finite(asp_box) || asp_box < asp_plt) # plot is wider than device: below
			1
		else # plot is taller than device: to the right
			4
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
	mat = t(matrix(unlist(x, use.names = FALSE), ncol = length(x))) #933
	if (!is.null(mat)) {
		ne = !is.na(rowMeans(mat))  ## faster than apply; #933
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
	non_empty = ! st_is_empty(x)
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
	non_empty = ! st_is_empty(x)
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
	non_empty = ! st_is_empty(x)
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
#' @param rule see \link[graphics]{polypath}; for \code{winding}, exterior ring direction should be opposite that of the holes; with \code{evenodd}, plotting is robust against misspecified ring directions
#' @param xpd see \link[graphics]{par}; sets polygon clipping strategy; only implemented for POLYGON and MULTIPOLYGON
#' @export
plot.sfc_POLYGON = function(x, y, ..., lty = 1, lwd = 1, col = NA, cex = 1, pch = NA, border = 1,
		add = FALSE, rule = "evenodd", xpd = par("xpd")) {
# FIXME: take care of lend, ljoin, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	border = rep(border, length.out = length(x))
	non_empty = ! st_is_empty(x)
	lapply(seq_along(x), function(i)
	  if (non_empty[i])
		polypath(p_bind(x[[i]]), border = border[i], lty = lty[i], lwd = lwd[i], col = col[i], rule = rule, xpd = xpd))
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
plot.sfc_MULTIPOLYGON = function(x, y, ..., lty = 1, lwd = 1, col = NA, border = 1, add = FALSE, 
		rule = "evenodd", xpd = par("xpd")) {
# FIXME: take care of lend, ljoin, and lmitre
	stopifnot(missing(y))
	if (! add)
		plot_sf(x, ...)
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	border = rep(border, length.out = length(x))
	non_empty = ! st_is_empty(x)
	lapply(seq_along(x), function(i)
	  if (non_empty[i])
		lapply(x[[i]], function(L)
			polypath(p_bind(L), border = border[i], lty = lty[i], lwd = lwd[i], col = col[i], rule = rule, xpd = xpd)))
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
	col = ifelse(st_dimension(x) == 2, NA, 1), border = 1, add = FALSE) {
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
#' 1/cos(My * pi/180) with My the y coordinate of the middle of the map
#' (the mean of \code{ylim}, which defaults to the y range of bounding box). This
#' implies an \href{https://en.wikipedia.org/wiki/Equirectangular_projection}{Equirectangular projection}.
#'
plot_sf = function(x, xlim = NULL, ylim = NULL, asp = NA, axes = FALSE, bgc = par("bg"), ...,
    xaxs, yaxs, lab, setParUsrBB = FALSE, bgMap = NULL, expandBB = c(0,0,0,0), graticule = NA_crs_,
	col_graticule = 'grey', border, extent = x) {

# sp's bbox: matrix
#   min max
# x
# y

	bbox = matrix(st_bbox(extent), 2, dimnames = list(c("x", "y"), c("min", "max")))
	# expandBB: 1=below, 2=left, 3=above and 4=right.
	expBB = function(lim, expand) c(lim[1] - expand[1] * diff(lim), lim[2] + expand[2] * diff(lim))
	if (is.null(xlim))
		xlim <- expBB(bbox[1,], expandBB[c(2,4)])
	if (is.null(ylim))
		ylim <- expBB(bbox[2,], expandBB[c(1,3)])
	if (is.na(asp))
		asp <- ifelse(isTRUE(st_is_longlat(x)), 1/cos((mean(ylim) * pi)/180), 1.0)

	if (any(is.na(bbox)))
		stop("NA value(s) in bounding box. Trying to plot empty geometries?")

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
	linAxis = function(side, ..., lon, lat, ndiscr, reset, at) axis(side = side, ...)
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
			local_degAxis = function(side, ..., at) .degAxis(side, ...) # absorb at
			local_degAxis(1, ...)
			local_degAxis(2, ...)
		} else {
			linAxis(1, ...)
			linAxis(2, ...)
		}
	}
	localTitle <- function(..., col, bgc, pch, cex, lty, lwd, lon, lat, ndiscr, at, labels, reset) title(...)
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


#' @param n integer; number of colors
#' @param cutoff.tails numeric, in [0,0.5] start and end values
#' @param alpha numeric, in [0,1], transparency
#' @param categorical logical; do we want colors for a categorical variable? (see details)
#' @name plot
#' @export
#' @details non-categorical colors from \code{sf.colors} were taken from \link[sp]{bpy.colors}, with modified \code{cutoff.tails} defaults
#' If categorical is \code{TRUE}, default colors are from \url{https://colorbrewer2.org/} (if n < 9, Set2, else Set3).
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
		if (alpha != 1.0)
			cb = paste0(cb, as.hexmode(ceiling(alpha * 255)))
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

#' @export
#' @name stars
#' @param bb ignore
#' @param n ignore
#' @param total_size ignore
#' @param key.length ignore
#' @param mfrow length-2 integer vector with number of rows, columns
.get_layout = function(bb, n, total_size, key.pos, key.length, mfrow = NULL) {
# return list with "m" matrix, "key.pos", "widths" and "heights" fields
# if key.pos = -1, it will be a return value, "optimally" placed
	asp = diff(bb[c(2,4)])/diff(bb[c(1,3)])
	if (!is.finite(asp)) # 0/0
		asp = 1
	if (isTRUE(st_is_longlat(bb)))
		asp = asp / cos(mean(bb[c(2,4)]) * pi /180)
	if (is.null(mfrow)) {
		size = function(nrow, n, asp) {
			ncol = ceiling(n / nrow)
			xsize = total_size[1] / ncol
			ysize = xsize  * asp
			if (xsize * ysize * n > prod(total_size)) {
				ysize = total_size[2] / nrow
				xsize = ysize / asp
			}
			xsize * ysize
		}
		sz = vapply(1:n, function(x) size(x, n, asp), 0.0)
		nrow = which.max(sz)
		ncol = ceiling(n / nrow)
	} else {
		stopifnot(is.numeric(mfrow), length(mfrow) == 2)
		nrow = mfrow[1]
		ncol = mfrow[2]
	}

	ret = list()
	ret$mfrow = c(nrow, ncol)

	# the following is right now only used by stars; FIXME:
	# nocov start
	ret$key.pos = if (!is.null(key.pos) && key.pos == -1L) { # figure out here: right or bottom?
			newasp = asp * ncol / nrow # of the composition
			dispasp = total_size[1] / total_size[2]
			ifelse(newasp > dispasp, 1, 4) # > or < ? oh dear,
		} else
			key.pos

	m = matrix(1 : (nrow * ncol), nrow, ncol, byrow = TRUE)
	if (!is.null(ret$key.pos) && ret$key.pos != 0) {
		k = key.length
		n = nrow * ncol + 1
		switch(ret$key.pos,
			{ ret$m = rbind(m, n); ret$widths = c(rep(1, ncol)); ret$heights = c(rep(1, nrow), k) },
			{ ret$m = cbind(n, m); ret$widths = c(k, rep(1, ncol)); ret$heights = c(rep(1, nrow)) },
			{ ret$m = rbind(n, m); ret$widths = c(rep(1, ncol)); ret$heights = c(k, rep(1, nrow)) },
			{ ret$m = cbind(m, n); ret$widths = c(rep(1, ncol), k); ret$heights = c(rep(1, nrow)) }
		)
	} else {
		ret$m = m
		ret$widths = rep(1, ncol)
		ret$heights = rep(1, nrow)
	}
	# nocov end
	ret
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

#' functions only exported to be used internally by stars
#' @name stars
#' @export
#' @param side ignore
#' @param at ignore
#' @param labels ignore
#' @param lon ignore
#' @param lat ignore
#' @param ndiscr ignore
#' @param reset ignore
.degAxis = function (side, at, labels, ..., lon, lat, ndiscr, reset) {
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

#' @name stars
#' @export
#' @param z ignore
#' @param col ignore
#' @param breaks ignore
#' @param key.pos ignore
#' @param add.axis ignore
#' @param axes ignore
#' @param logz ignore
#' @param ... ignore
.image_scale = function(z, col, breaks = NULL, key.pos, add.axis = TRUE,
		at = NULL, ..., axes = FALSE, key.length, logz = FALSE) {
	if (!is.null(breaks) && length(breaks) != (length(col) + 1))
		stop("must have one more break than colour")
	zlim = range(z, na.rm = TRUE)
	if (is.null(breaks))
		breaks = seq(zlim[1], zlim[2], length.out = length(col) + 1)
	if (is.character(key.length)) {
		kl = as.numeric(gsub(" cm", "", key.length))
		sz = if (key.pos %in% c(1,3))
				dev.size("cm")[1]
			else
				dev.size("cm")[2]
		key.length = kl/sz
	}
	if (is.null(at)) {
		br = range(breaks)
		at = pretty(br)
		at = at[at > br[1] & at < br[2]]
	}
	kl_lim = function(r, kl) { m = mean(r); (r - m)/kl + m }
	if (key.pos %in% c(1,3)) {
		ylim = c(0, 1)
		xlim = kl_lim(range(breaks), key.length)
		mar = c(0, ifelse(axes, 2.1, 1), 0, 1)
	}
	if (key.pos %in% c(2,4)) {
		ylim = kl_lim(range(breaks), key.length)
		xlim = c(0, 1)
		mar = c(ifelse(axes, 2.1, 1), 0, 1.2, 0)
	}
	mar[key.pos] = 2.1
	par(mar = mar)

	poly = vector(mode="list", length(col))
	for (i in seq(poly))
		poly[[i]] = c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	plot(1, 1, t = "n", ylim = ylim, xlim = xlim, axes = FALSE,
		xlab = "", ylab = "", xaxs = "i", yaxs = "i")
	offset = 0.2
	offs = switch(key.pos,
		c(0,0,-offset,-offset),
		c(0,0,-offset,-offset),
		c(offset,offset,0,0),
		c(offset,offset,0,0))
	for(i in seq_along(poly)) {
		if (key.pos %in% c(1,3))
			polygon(poly[[i]], c(0, 0, 1, 1) + offs, col = col[i], border = NA)
		if (key.pos %in% c(2,4))
			polygon(c(0, 0, 1, 1) + offs, poly[[i]], col = col[i], border = NA)
	}

	# box() now would draw around [0,1]:
	bx = c(breaks[1], rep(tail(breaks, 1), 2), breaks[1])
	if (key.pos %in% c(1,3))
		polygon(bx, c(0, 0, 1, 1) + offs, col = NA, border = 'black')
	if (key.pos %in% c(2,4))
		polygon(c(0, 0, 1, 1) + offs, bx, col = NA, border = 'black')

	labels = if (logz)
			parse(text = paste0("10^", at))
		else if (inherits(breaks, c("POSIXt", "Date")))
			format(at)
		else
			TRUE

	if (add.axis)
		axis(key.pos, at = at, labels = labels)
}

#' @name stars
#' @export
#' @param key.width ignore
.image_scale_factor = function(z, col, key.pos, add.axis = TRUE,
	..., axes = FALSE, key.width, key.length) {

	n = length(z)
	# TODO:
	ksz = as.numeric(gsub(" cm", "", key.width)) * 2
	breaks = (0:n) + 0.5
	if (is.character(key.length)) {
		kl = as.numeric(gsub(" cm", "", key.length))
		sz = if (key.pos %in% c(1,3))
				dev.size("cm")[1]
			else
				dev.size("cm")[2]
		key.length = kl/sz
	}
	kl_lim = function(r, kl) { m = mean(r); (r - m)/kl + m }
	if (key.pos %in% c(1,3)) {
		ylim = c(0, 1)
		xlim = kl_lim(range(breaks), key.length)
		mar = c(0, ifelse(axes, 2.1, 1), 0, 1)
		mar[key.pos] = 2.1
	} else {
		ylim = kl_lim(range(breaks), key.length)
		xlim = c(0, 1)
		mar = c(ifelse(axes, 2.1, 1), 0, 1.2, 0)
		#mar[key.pos] = 2.1
		mar[key.pos] = max(ksz - 1.3, 0.0)
	}
	par(mar = mar)

	poly = vector(mode="list", length(col))
	for (i in seq(poly))
		poly[[i]] = c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
	plot(1, 1, t = "n", ylim = ylim, xlim = xlim, axes = FALSE,
		xlab = "", ylab = "", xaxs = "i", yaxs = "i")
	for(i in seq_along(poly)) {
		if (key.pos %in% c(1,3))
			polygon(poly[[i]], c(0, 0, 1, 1), col = col[i], border = NA)
		if (key.pos %in% c(2,4))
			polygon(c(0, 0, 1, 1), poly[[i]], col = col[i], border = NA)
	}

	# box() now would draw around [0,1]:
	bx = c(breaks[1], rep(tail(breaks, 1), 2), breaks[1])
	if (key.pos %in% c(1,3))
		polygon(bx, c(0, 0, 1, 1), col = NA, border = 'black')
	if (key.pos %in% c(2,4))
		polygon(c(0, 0, 1, 1), bx, col = NA, border = 'black')

	if (add.axis) {
		opar = par(las = 1)
		axis(key.pos, at = 1:n, labels = z)
		par(opar)
	}
}

# nocov start
#' @export
identify.sfc = function(x, ..., n = min(10, length(x)), type = "n") {
	l = locator(n, type = type)
	pts = st_as_sf(as.data.frame(do.call(cbind, l)), coords = c("x", "y"), crs = st_crs(x))
	sapply(st_intersects(pts, x), function(x) if (length(x)) x[1] else NA_integer_)
}

#' @export
identify.sf = function(x, ...) {
	identify(st_geometry(x), ...)
}
# nocov end
