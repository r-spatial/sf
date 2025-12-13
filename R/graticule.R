st_datum = function(x) {
	stopifnot(inherits(x, c("crs", "sf", "sfc")))
	if (inherits(x, "crs"))
		x = st_sfc(st_point(), crs = x)
	x = st_geometry(x) # in case inherits(x, "sf")
	datum = st_crs(st_crs(x, parameters = TRUE)$gcs_crs)
	if (is.na(datum))
		st_crs('OGC:CRS84') # + message or warning?
	else
		datum
}

#' Compute graticules and their parameters
#'
#' Compute graticules and their parameters
#'
#' @section Use of graticules:
#'  In cartographic visualization, the use of graticules is not advised, unless
#'  the graphical output will be used for measurement or navigation, or the
#'  direction of North is important for the interpretation of the content, or
#'  the content is intended to display distortions and artifacts created by
#'  projection. Unnecessary use of graticules only adds visual clutter but
#'  little relevant information. Use of coastlines, administrative boundaries
#'  or place names permits most viewers of the output to orient themselves
#'  better than a graticule.
#'
#' @export
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg} or numeric vector with bounding box given as (minx, miny, maxx, maxy).
#' @param crs object of class \code{crs}, with the display coordinate reference system
#' @param datum either an object of class \code{crs} with the coordinate reference system for the graticules, or \code{NULL} in which case a grid in the coordinate system of \code{x} is drawn, or \code{NA}, in which case an empty \code{sf} object is returned. If missing and \code{x} has a crs with a datum, the geographic coordinate system (datum) of \code{x} is taken.
#' @param lon numeric; values in degrees East for the meridians, associated with \code{datum}
#' @param lat numeric; values in degrees North for the parallels, associated with \code{datum}
#' @param ndiscr integer; number of points to discretize a parallel or meridian
#' @param margin numeric; small number to trim a longlat bounding box that touches or
#'  crosses +/-180 long or +/-90 latitude.
#' @param ... ignored
#' @return an object of class \code{sf} with additional attributes describing the type
#' (E: meridian, N: parallel) degree value, label, start and end coordinates and angle;
#' see example.
#' @examples
#' library(sf)
#' if (require(maps, quietly = TRUE)) {
#'
#' usa = st_as_sf(map('usa', plot = FALSE, fill = TRUE))
#' laea = st_crs("+proj=laea +lat_0=30 +lon_0=-95") # Lambert equal area
#' usa <- st_transform(usa, laea)
#'
#' bb = st_bbox(usa)
#' bbox = st_linestring(rbind(c( bb[1],bb[2]),c( bb[3],bb[2]),
#'    c( bb[3],bb[4]),c( bb[1],bb[4]),c( bb[1],bb[2])))
#'
#' g = st_graticule(usa)
#' plot(usa, xlim = 1.2 * c(-2450853.4, 2186391.9), reset = FALSE)
#' plot(g[1], add = TRUE, col = 'grey')
#' plot(bbox, add = TRUE)
#' points(g$x_start, g$y_start, col = 'red')
#' points(g$x_end, g$y_end, col = 'blue')
#'
#' invisible(lapply(seq_len(nrow(g)), function(i) {
#'	if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000)
#'		text(g$x_start[i], g$y_start[i], labels = parse(text = g$degree_label[i]), 
#'			srt = g$angle_start[i], pos = 2, cex = .7)
#'	if (g$type[i] == "E" && g$y_start[i] - min(g$y_start) < 1000)
#'		text(g$x_start[i], g$y_start[i], labels = parse(text = g$degree_label[i]), 
#'			srt = g$angle_start[i] - 90, pos = 1, cex = .7)
#'	if (g$type[i] == "N" && g$x_end[i] - max(g$x_end) > -1000)
#'		text(g$x_end[i], g$y_end[i], labels = parse(text = g$degree_label[i]), 
#'			srt = g$angle_end[i], pos = 4, cex = .7)
#'	if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000)
#'		text(g$x_end[i], g$y_end[i], labels = parse(text = g$degree_label[i]), 
#'			srt = g$angle_end[i] - 90, pos = 3, cex = .7)
#' }))
#' plot(usa, graticule = st_crs(4326), axes = TRUE, lon = seq(-60,-130,by=-10))
#' }
st_graticule = function(x = c(-180, -90, 180, 90), crs = st_crs(x),
	datum = st_crs('OGC:CRS84'), ..., lon = NULL, lat = NULL, ndiscr = 100,
	margin = 0.001)
{
	s2 = sf_use_s2()
	on.exit(suppressMessages(sf_use_s2(s2)))
	suppressMessages(sf_use_s2(FALSE))

	if (missing(x)) {
		crs = datum
		if (is.null(lon))
			lon = seq(-180, 180, by = 20)
		if (is.null(lat))
			lat = seq(-80, 80, by = 20)
	} else if (missing(datum)) {
		datum = if (inherits(x, c("sf", "sfc")))
				st_datum(x)
			else
				st_datum(crs)
	}

	if (is.null(crs))
		crs = NA_crs_

	if (is.null(datum))
		datum = crs

	if (is.na(datum))
		return(st_graticule()[0,]) # empty set, but with all variables

	# Get the bounding box of the plotting space, in crs
	bb = if (inherits(x, "sf") || inherits(x, "sfc") || inherits(x, "sfg"))
		st_bbox(x)
	else
		x
	stopifnot(is.numeric(bb), length(bb) == 4)

	if (isTRUE(st_is_longlat(crs)))
		bb = trim_bb(bb, margin)

	ls1 = st_linestring(rbind(c(bb[1],bb[2]), c(bb[3],bb[2]), c(bb[3],bb[4]),
		c(bb[1],bb[4]), c(bb[1],bb[2])))
	ls2 = st_linestring(rbind(c(bb[1],bb[2]), c(bb[3],bb[4]), c(bb[1],bb[4]),
		c(bb[3],bb[2]), c(bb[1],bb[2])))
	box = st_sfc(ls1, ls2)

	# without crs, we segmentize in planar coordinates --
	# segmentizing along great circles doesn't give parallels:
	box = st_segmentize(box, st_length(box)[1] / ndiscr)

	# and only now set the crs:
	st_crs(box) = crs

	# Now, in case we're not already in longlat, we convert to longlat:
	box_ll = if (! is.na(crs))
		st_transform(box, datum, partial = TRUE)
	else {
		datum = NA_crs_ # nocov - remove when geom_sf is on CRAN
		box             # nocov
	}

	# as in https://github.com/r-spatial/sf/issues/198 :
	# recreate, and ignore bbox_ll:
	if (any(!is.finite(st_bbox(box_ll)))) { # nocov start -- checked manually
		x = st_transform(st_graticule(datum = datum, ndiscr = ndiscr), crs)
		x$degree_label = NA_character_
		return(x)
	} # nocov end

	bb = st_bbox(box_ll)
	if (is.null(lon)) {
		lon = if (bb[3] <= 180 && bb[1] < -170 && bb[3] > 170) # global, -180,180:
			seq(-180, 180, by = 60)
		else
			pretty(bb[c(1,3)], n = 6)
	}
	if (is.null(lat))
		lat = pretty(bb[c(2,4)], n = 6)

	# sanity:
	if (isTRUE(st_is_longlat(datum))) {
		lon = if (min(lon) >= -15 && max(lon) > 195) # +- 4%
			lon[lon >= 0 & lon <= 360] # assume 0,360
		else
			lon[lon >= -180 & lon <= 180]
		lat = lat[lat > -90 & lat < 90]
	}

	# widen bb if pretty() created values outside the box_ll:
	bb = c(min(bb[1], min(lon)), min(bb[2],min(lat)), max(bb[3], max(lon)), max(bb[4], max(lat)))

	long_list <- vector(mode="list", length=length(lon))
	for (i in seq_along(long_list))
		long_list[[i]] <- st_linestring(cbind(rep(lon[i], ndiscr), seq(bb[2], bb[4], length.out=ndiscr)))

	lat_list <- vector(mode="list", length=length(lat))
	for (i in seq_along(lat_list))
		lat_list[[i]] <- st_linestring(cbind(seq(bb[1], bb[3], length.out=ndiscr), rep(lat[i], ndiscr)))

	df = data.frame(degree = c(lon, lat))
	df$type = c(rep("E", length(lon)), rep("N", length(lat)))
	df$degree_label = if (is.na(crs) || !isTRUE(st_is_longlat(datum)))
			c(format(lon), format(lat))
		else
			c(degreeLabelsEW(lon), degreeLabelsNS(lat))

	geom = st_sfc(c(long_list, lat_list), crs = datum)

	# Now we're moving the straight lines back to curves in crs:
	if (!is.na(crs))
		geom = st_transform(geom, crs)

	st_geometry(df) = geom
	st_agr(df) = "constant"

	if (! missing(x)) # cut out box:
		df = suppressMessages(st_intersection(df, st_polygonize(box[1])))

	df = st_cast(st_cast(df, "MULTILINESTRING"), "LINESTRING", warn = FALSE)
	graticule_attributes(df)
}

graticule_attributes = function(df) {
	object = st_geometry(df)

	if (nrow(df) == 0)
		return(df)

	xy = matrix(NA, nrow = length(object), ncol = 4)
	for (i in seq_along(object)) {
		pts = unclass(object[[i]])
		xy[i, 1:2] = pts[1,] # start
		xy[i, 3:4] = pts[nrow(pts),] # end
	}
	df$x_start = xy[,1]
	df$y_start = xy[,2]
	df$x_end   = xy[,3]
	df$y_end   = xy[,4]
	dxdy = do.call(rbind, lapply(object, function(x) { apply(x[1:2,], 2, diff) } ))
	df$angle_start = apply(dxdy, 1, function(x) atan2(x[2], x[1])*180/pi)
	dxdy = do.call(rbind, lapply(object,
		function(x) { n = nrow(x); apply(x[(n-1):n,], 2, diff) } ))
	df$angle_end = apply(dxdy, 1, function(x) atan2(x[2], x[1])*180/pi)
	bb = st_bbox(df)
	selE = df$type == "E" & df$y_start < min(df$y_start) + 0.001 * (bb[3] - bb[1])
	selN = df$type == "N" & df$x_start < min(df$x_start) + 0.001 * (bb[4] - bb[2])
	df$plot12 = selE | selN
	df
}

trim_bb = function(bb = c(-180, -90, 180, 90), margin, wrap=c(-180,180)) {
	stopifnot(margin > 0, margin <= 1.0)
	fr = 1.0 - margin
	if (min(bb[c(1,3)]) >= -15. && max(bb[c(1,3)]) > 195.) { # 0-360 span:
		wrap=c(0., 360.)
		bb[1] = max(bb[1], .5 * wrap[1] * fr)
		bb[3] = min(bb[3], wrap[2] * fr)
	} else {
		bb[1] = max(bb[1], wrap[1] * fr)
		bb[3] = min(bb[3], wrap[2] * fr)
	}
	bb[2] = max(bb[2], -90. * fr)
	bb[4] = min(bb[4],  90. * fr)
	bb
}

# copied from sp:
degreeLabelsNS = function(x) {
	pos = sign(x) + 2
	dir = c("*S", "", "*N")
	paste0('"', format(abs(x), digits = 10), '"', "*degree", dir[pos])
}

degreeLabelsEW = function(x) {
	x <- ifelse(x > 180, x - 360, x)
	pos = sign(x) + 2
	if (any(x == -180))
		pos[x == -180] = 2
	if (any(x == 180))
		pos[x == 180] = 2
	dir = c("*W", "", "*E")
	paste0('"', format(abs(x), digits = 10), '"', "*degree", dir[pos])
}
