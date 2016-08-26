projected = function(x) {
	if (inherits(x, "sf"))
		x = geometry(x)
	p4str = attr(x, "proj4string")
	if (is.na(p4str))
		NA
	else
		length(grep("longlat", p4str, fixed = TRUE)) == 0
}

# another idea:
#plot.sf = function(x, y, ...) plot(as(x, "Spatial"), ...)

#' plot sf objects
#'
#' plot sf objects
#' @param x object of class sf
#' @param y ignored
##' @param xlim see \link{plot.window}
##' @param ylim see \link{plot.window}
##' @param xaxs see \link{plot.window}
##' @param yaxs see \link{plot.window}
##' @param lab see \link{plot.window}
##' @param asp aspect ratio, see \link{plot.window}
##' @param axes logical; plot axes?
##' @param add logical; add to existing plot?
#' @param ... passed on to plotting functions
#' @export
plot.sf = function(x, y, ..., xlim, ylim, xaxs, yaxs, lab, asp = NA, axes = TRUE, add = FALSE) {
	stopifnot(missing(y))
	if (!add) {
		plot.new()
		if (missing(xlim)) xlim = bbox(x)[c("xmin", "xmax")]
		if (missing(ylim)) ylim = bbox(x)[c("ymin", "ymax")]
    	if (is.na(asp))
        	asp <- if (is.na(projected(x)) || projected(x))
					1.0
				else
					1./cos((mean(ylim) * pi)/180)
    	args = list(xlim = xlim, ylim = ylim, asp = asp)
    	if (!missing(xaxs)) args$xaxs = xaxs
    	if (!missing(yaxs)) args$yaxs = yaxs
    	if (!missing(lab)) args$lab = lab
    	do.call(plot.window, args)
    	if (axes) { # set up default axes system & box:
        	box()
        	if (identical(projected(x), FALSE)) {
            	sp::degAxis(1, ...)
            	sp::degAxis(2, ...)
        	} else {
            	axis(1, ...)
            	axis(2, ...)
        	}
		}
	}
	g = geometry(x)
	switch(class(g)[1],
		sfc_POINT = points(g, ...),
		sfc_LINESTRING = , sfc_MULTILINESTRING = lines(g, ...),
		stop(paste("plotting of", class(g)[1], "not supported"))
	)
}

points.sfc_POINT = function(x, ..., pch = 1, col = 1, bg = 0, cex = 1, lwd = 1, lty = 1) {
	pch = rep(pch, length.out = length(x))
	col = rep(col, length.out = length(x))
	bg = rep(bg, length.out = length(x))
	cex = rep(cex, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	lty = rep(lty, length.out = length(x))
	lapply(1:length(x), 
		function(i) points(x[[i]][1], x[[i]][2], pch = pch[i], col = col[i], bg = bg[i], 
			cex = cex[i], lwd = lwd[i], lty = lty[i], ...))
}

lines.sfc_LINESTRING = function(x, ..., lty = 1, lwd = 1, col = 1, pch = 1) {
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	pch  = rep(pch, length.out = length(x))
	lapply(1:length(x), function(i)
		lines(x[[i]], lty = lty[i], lwd = lwd[i], col = col[i], pch = pch[i], ...))
	invisible(NULL)
}

lines.sfc_MULTILINESTRING = function(x, ..., lty = 1, lwd = 1, col = 1, pch = 1) {
	lty = rep(lty, length.out = length(x))
	lwd = rep(lwd, length.out = length(x))
	col = rep(col, length.out = length(x))
	pch  = rep(pch, length.out = length(x))
	lapply(1:length(x), function(i)
		lapply(x[[i]], function(L)
			lines(L, lty = lty[i], lwd = lwd[i], col = col[i], pch = pch[i], ...)))
	invisible(NULL)
}
