# window_polygons_from_edges = function (w) {
#	mw = as.matrix(w$ends)
#	lst1 = lapply(seq_len(NROW(mw)), function(i) st_linestring(matrix(mw[i,], 2, byrow = TRUE)))
#	p0 = st_polygonize(do.call(c, do.call(st_sfc, lst1)))
#	if (length(p0) > 1) # multiple POLYGONs, returned as sfc_
#		do.call(c, st_collection_extract(p0, "POLYGON")) # MULTIPOLYGON
#	else
#		st_cast(p0, "POLYGON")
# }

wrp = function(x) paste(strwrap(x), collapse = "\n")

check_spatstat_ll = function(x) {
	if (isTRUE(st_is_longlat(x)))
		stop(wrp("Only projected coordinates may be converted to spatstat class objects"), call. = FALSE)
}

check_spatstat <- function(pkg, X = NULL) {
	if (!requireNamespace(pkg, quietly = TRUE))
		stop("package ", pkg, " required, please install it (or the full spatstat package) first", call. = FALSE)
	spst_ver <- try(packageVersion("spatstat"), silent = TRUE)
	if (!inherits(spst_ver, "try-error") && spst_ver < 2.0-0)
		stop(wrp(paste("You have an old version of spatstat installed which is incompatible with ", pkg, 
			". Please update spatstat (or uninstall it).")), call. = FALSE)
	if (!is.null(X))
		check_spatstat_ll(X)
}

#' @name st_as_sf
#' @export
#' @examples
#' if (require(spatstat.geom)) {
#'   g = st_as_sf(gorillas)
#'   # select only the points:
#'   g[st_is(g, "POINT"),]
#' }
st_as_sf.ppp = function(x, ...) {
	check_spatstat("spatstat.geom")
	# window:
	win = st_sf(label = "window", geom = st_as_sfc(spatstat.geom::as.owin(x)))

	# points:
	m = as.matrix(data.frame(x$x, x$y))
	pointwork = st_sfc(lapply(seq_len(NROW(m)), function(i) st_point(m[i,])))
	points_sf = st_sf(label = rep("point", NROW(m)), geom = pointwork)

	# merge window and points:
	ret = rbind(win, points_sf)
	if (spatstat.geom::is.marked(x)) {
		# add marks:
		m = as.data.frame(spatstat.geom::marks(x))
		cbind.sf(m[c(NA, seq_len(nrow(m))), , drop = FALSE], ret)
	} else
		ret
}

#' @export
st_as_sf.ppplist = function(x, ...) {
	w = st_geometry(st_as_sf(x[[1]]))[1]
	sim = st_sfc(lapply(x, function(p) do.call(c, st_geometry(st_as_sf(p))[-1])))
	st_sf(label = c("window", names(x)), geom = c(w, sim))
}


#' @name st_as_sf
#' @export
st_as_sf.psp = function(x, ...) {
	check_spatstat("spatstat.geom")
	# line segments:
	m = as.matrix(x$ends)
	lst1 = lapply(seq_len(NROW(m)), function(i) st_linestring(matrix(m[i,], 2, byrow = TRUE)))

	# window:
	win = st_as_sfc(spatstat.geom::as.owin(x))[[1]]

	label = c("window", rep("segment", NROW(m)))
	ret = st_sf(label = label, geom = st_sfc(c(list(win), lst1)))
	if (spatstat.geom::is.marked(x)) { # add marks:
		m = as.data.frame(spatstat.geom::marks(x))
		cbind.sf(m[c(NA, seq_len(nrow(m))), , drop = FALSE], ret)
	} else
		ret
}

# 111117 from psp to SpatialLines, Rolf Turner, Adrian Baddeley, Mathieu Rajerison
#' @export
st_as_sfc.psp <- function(x, ...) {

#	ends2line <- function(x) matrix(x, ncol=2, byrow=TRUE)
#	munch <- function(z) { list(ends2line(as.numeric(z[1:4]))) }
#	ends <- as.data.frame(x)[,1:4]
#	y <- lapply(seq_len(nrow(ends)), function(i) munch(ends[i,]))
#	st_sfc(st_multilinestring(y))
	st_geometry(st_as_sf(x, ...))
}


#' @name st_as_sf
#' @export
#' @examples
#' if (require(spatstat.linnet)) {
#'  data(chicago)
#'  plot(st_as_sf(chicago)["label"])
#'  plot(st_as_sf(chicago)[-1,"label"])
#' }
st_as_sf.lpp = function(x, ...) {
	check_spatstat("spatstat.linnet")
	# lines, polygon:
	linework_sf = st_as_sf(spatstat.geom::as.psp(spatstat.geom::domain(x)))
	# points:
	m = as.matrix(as.data.frame(x$data)[1:2])
	pointwork = st_sfc(lapply(seq_len(NROW(m)), function(i) st_point(m[i,])))
	sf = rbind(linework_sf, st_sf(label = rep("point", NROW(m)), geom = pointwork))
	# de-select point coordinates
	m = as.data.frame(x$data)[c(rep(NA,nrow(linework_sf)),seq_len(nrow(m))), -(1:2)]
	structure(cbind.sf(sf, m), row.names = seq_len(nrow(m)))
}

# as.ppp etc methods: from maptools/pkg/R/spatstat1.R

as.ppp.sfc = function(X, W = NULL, ..., check = TRUE) {
	check_spatstat("spatstat.geom", X)
	d = st_dimension(X)
	if (is.null(W)) {
		if (d[1] == 2 && all(d[-1] == 0)) {
			W = spatstat.geom::as.owin(X[1])
			X = X[-1]
		} else if (all(d == 0)) { # no window in first feature geometry:
			bb <- st_bbox(X)
			W = spatstat.geom::owin(bb[c("xmin", "xmax")], bb[c("ymin", "ymax")])
			if (missing(check))
				check = FALSE
		} else
			stop("sfc object does not consist of points, or a window followed by points")
	}
	cc = st_coordinates(X)
	spatstat.geom::ppp(cc[,1], cc[,2], window = W, marks = NULL, check = check)
}

as.ppp.sf = function(X) {
	check_spatstat("spatstat.geom", X)
	pp = spatstat.geom::as.ppp(st_geometry(X))
	if (st_dimension(X[1,]) == 2)
		X = X[-1,]
	st_geometry(X) = NULL # remove geometry column
	if (ncol(X) > 1)
		warning("only first attribute column is used for marks")

	if (ncol(X) == 0)
		pp
	else 
		spatstat.geom::setmarks(pp, X[1])
}

as.owin.POLYGON = function(W, ..., fatal, check_polygons = TRUE) {
	check_spatstat("spatstat.geom", W)
	if (check_polygons)
		W = check_ring_dir(W)
	bb = st_bbox(W)
	spatstat.geom::owin(bb[c("xmin", "xmax")], bb[c("ymin", "ymax")], poly = W)
}

as.owin.MULTIPOLYGON = function(W, ..., fatal, check_polygons = TRUE) {
	check_spatstat("spatstat.geom", W)
	if (check_polygons)
		W = check_ring_dir(W)
	bb = st_bbox(W)
	spatstat.geom::owin(bb[c("xmin", "xmax")], bb[c("ymin", "ymax")], 
		poly = unlist(W, recursive = FALSE))
}

as.owin.sfc_POLYGON = function(W, ..., fatal, check_polygons = TRUE) {
	check_spatstat_ll(W)
	if (check_polygons)
		W = check_ring_dir(W)
	as.owin.MULTIPOLYGON(W, check_polygons = FALSE)
	# I know, this looks wrong, but isn't: sfc_POLYGON is a logically a MULTIPOLYGON
}

as.owin.sfc_MULTIPOLYGON = function(W, ..., fatal, check_polygons = TRUE) {
	check_spatstat_ll(W)
	if (check_polygons)
		W = check_ring_dir(W)
	as.owin.sfc_POLYGON(st_cast(W, "POLYGON"), check_polygons = FALSE)
}

as.owin.sfc = function(W, ...) {
	if (!all(st_dimension(W) == 2))
		stop("as.owin.sfc needs polygonal geometries")
	as.owin.sfc_MULTIPOLYGON(st_cast(W, "MULTIPOLYGON"), ...)
}

as.owin.sf = function(W, ...) {
	as.owin.sfc(st_geometry(W), ...)
}

#' @export
st_as_sfc.owin = function(x, ...) {
# FROM: methods for coercion to Spatial Polygons by Adrian Baddeley, pkg maptools
	check_spatstat("spatstat.geom")
	# Check internal spatstat multiplier:
	mult <- x$units$multiplier
	if(is.null(mult)){
		mult <- 1
	}
	if(mult!=1){
		warning("The spatstat object has an measurement unit multiplier != 1. Consider rescaling before converting.")
	}
	# Enforce polygonal format and proceed from there:
	x <- spatstat.geom::as.polygonal(x)
	closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
	pieces <- lapply(x$bdry,
		function(p) st_polygon(list(closering(cbind(p$x,p$y)))))
	h = sapply(x$bdry, spatstat.utils::is.hole.xypolygon)   # holes
	holes = do.call(st_sfc, pieces[h])
	exteriors = pieces = do.call(st_sfc, pieces[!h])
	# assign each hole to the smallest exterior it is covered by:
	cb = st_covered_by(holes, pieces)
	for (i in seq_along(cb)) {
		w = which.min(st_area(exteriors[ cb[[i]] ]))
		pieces[[w]] = st_polygon(c(unclass(pieces[[w]]), unclass(holes[[i]])))
	}
	if (length(pieces) > 1) # multiple POLYGONs, collapse:
		st_sfc(do.call(c, pieces))
	else
		pieces
}

#' @export
st_as_sf.owin = function(x, ...) {
	st_sf(geom = st_as_sfc(x, ...))
}


#' @export
st_as_sfc.tess <- function(x, ...) {
	check_spatstat("spatstat.geom")
	stopifnot(spatstat.geom::is.tess(x))
	y <- spatstat.geom::tiles(x)
	nam <- names(y)
	z <- list()
	for(i in seq_along(y)) {
		zi <- try(st_as_sfc(y[[i]], nam[i]), silent=TRUE)
		if (inherits(zi, "try-error"))
			warning(paste("tile", i, "defective\n", as.character(zi)))
		else
			z[[i]] <- zi
	}
	do.call(c, z)
}

# methods for 'as.psp' for sp classes by Adrian Baddeley
as.psp.LINESTRING <- function(from, ..., window=NULL, marks=NULL, fatal) {
	check_spatstat("spatstat.geom")
	xy <- unclass(from)
	df <- as.data.frame(cbind(xy[-nrow(xy), , drop=FALSE], xy[-1, , drop=FALSE]))
	if (is.null(window)) {
		xrange <- range(xy[,1])
		yrange <- range(xy[,2])
		window <- spatstat.geom::owin(xrange, yrange)
	}
	spatstat.geom::as.psp(df, window=window, marks=marks)
}

as.psp.MULTILINESTRING <- function(from, ..., window=NULL, marks=NULL, fatal) {
	check_spatstat("spatstat.geom")
	y <- lapply(from, as.psp.LINESTRING, window=window)
	z <- do.call(spatstat.geom::superimpose,c(y,list(W=window)))
	if(!is.null(marks))
		spatstat.geom::setmarks(z, marks)
	else
		z
}

as.psp.sfc_MULTILINESTRING <- function(from, ..., window=NULL, marks=NULL,
			characterMarks=FALSE, fatal) {

	check_spatstat("spatstat.geom", from)
	if(is.null(window)) {
		bb = st_bbox(from)
		window = spatstat.geom::owin(bb[c("xmin", "xmax")], bb[c("ymin", "ymax")]) 
	}
	lin <- unclass(from)
	y <- lapply(lin, as.psp.MULTILINESTRING, window=window)
	z <- do.call(spatstat.geom::superimpose, c(y, list(W = window)))
	if(!is.null(marks))
		spatstat.geom::setmarks(z, marks)
	else
		z
}

as.psp.sfc = function(from, ...) {
	as.psp.sfc_MULTILINESTRING(st_cast(from, "MULTILINESTRING"))
}

as.psp.sf <- function(from, ..., window=NULL, marks=NULL, fatal) {
	check_spatstat("spatstat.geom", from)

	y <- st_geometry(from)
	if (!inherits(y, "sfc_MULTILINESTRING"))
		stop("geometries should be of type LINESTRING")
	z <- spatstat.geom::as.psp(y, window=window, marks=marks)
	if(is.null(marks)) {
		# extract marks from first column of data frame
		st_geometry(from) = NULL # remove geometry column
		nseg.LINESTRING  <- function(x) { nrow(x) - 1 }
		nseg.MULTILINESTRING <- function(x) { sum(unlist(lapply(x, nseg.LINESTRING))) }
		nrep <- unlist(lapply(y, nseg.MULTILINESTRING))
		spatstat.geom::setmarks(z, from[rep(seq_len(nrow(from)), nrep),])
	} else
		z
}
