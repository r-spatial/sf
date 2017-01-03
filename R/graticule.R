#' compute graticules and their parameters
#' 
#' compute graticules and their parameters
#' @export
#' @param x object of class \code{sf}, \code{sfc} or \code{sfg} or numeric vector with bounding box (minx,miny,maxx,maxy).
#' @param crs object of class \code{crs}, with the display coordinate reference system
#' @param datum object of class \code{crs}, with the coordinate reference system for the graticules
#' @param easts numeric; degrees east for the meridians
#' @param norths numeric; degrees north for the parallels
#' @param ndiscr integer; number of points to discretize a parallel or meridian
#' @value an object of class \code{sf} with additional attributes describing the type 
#' (E: meridian, N: parallel) degree value, label, start and end coordinates and angle;
#' see example.
#' @examples 
#' library(sp)
#' library(maps)
#' 
#' m = map('usa', plot = FALSE, fill = TRUE)
#' ID0 <- sapply(strsplit(m$names, ":"), function(x) x[1])
#' 
#' library(maptools)
#' m <- map2SpatialPolygons(m, IDs=ID0, proj4string = CRS("+init=epsg:4326"))
#' 
#' library(sf)
#' 
#' laea = st_crs("+proj=laea +lat_0=30 +lon_0=-95") # Lambert equal area
#' m <- st_transform(st_as_sf(m), laea)
#' 
#' bb = st_bbox(m)
#' bbox = st_linestring(rbind(c( bb[1],bb[2]),c( bb[3],bb[2]),c( bb[3],bb[4]),c( bb[1],bb[4]),c( bb[1],bb[2])))
#' 
#' g = st_graticule(m)
#' plot(m, xlim = 1.2 * c(-2450853.4, 2186391.9))
#' plot(g[1], add = TRUE, col = 'grey')
#' plot(bbox, add = TRUE)
#' points(g$x_start, g$y_start, col = 'red')
#' points(g$x_end, g$y_end, col = 'blue')
#' 
#' invisible(lapply(seq_len(nrow(g)), function(i) {
#'	if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000)
#'		text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]), 
#'			srt = g$angle_start[i], pos = 2, cex = .7)
#'	if (g$type[i] == "E" && g$y_start[i] - min(g$y_start) < 1000)
#'		text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]), 
#'			srt = g$angle_start[i] - 90, pos = 1, cex = .7)
#'	if (g$type[i] == "N" && g$x_end[i] - max(g$x_end) > -1000)
#'		text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]), 
#'			srt = g$angle_end[i], pos = 4, cex = .7)
#'	if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000)
#'		text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]), 
#'			srt = g$angle_end[i] - 90, pos = 3, cex = .7)
#' }))
st_graticule = function(x, crs = st_crs(x), datum = st_crs(4326),
	easts = pretty(st_bbox(box)[c(1,3)]),
	norths = pretty(st_bbox(box)[c(2,4)]), ndiscr = 100)
{
	bb = if (inherits(x, "sf") || inherits(x, "sfc") || inherits(x, "sfg"))
		st_bbox(x)
	else
		x
	stopifnot(is.numeric(bb) && length(bb) == 4)

	ls = st_linestring(rbind(c(bb[1],bb[2]), c(bb[3],bb[2]), c(bb[3],bb[4]), 
		c(bb[1],bb[4]), c(bb[1],bb[2])))
	box = st_sfc(ls, crs = crs)

	box = st_segmentize(box, st_length(ls) / 400)

	if (!is.na(crs))
		box = st_transform(box, datum)

	bb = st_bbox(box) # in the longlat CRS
	# FIXME: need to widen? pretty() might take values outside range
	bb = c(min(bb[1], min(easts)), min(bb[2],min(norths)), max(bb[3], max(easts)), max(bb[4], max(norths)))

	eastlist <- vector(mode="list", length=length(easts))
	for (i in seq_along(eastlist))
		eastlist[[i]] <- st_linestring(cbind(rep(easts[i], ndiscr), seq(bb[2], bb[4], length.out=ndiscr)))

	northlist <- vector(mode="list", length=length(norths))
	for (i in seq_along(northlist))
		northlist[[i]] <- st_linestring(cbind(seq(bb[1], bb[3], length.out=ndiscr), rep(norths[i], ndiscr)))
	
	df = data.frame(degree = c(easts, norths))
	df$type = c(rep("E", length(easts)), rep("N", length(norths)))
	df$degree_label = c(degreeLabelsEW(easts), degreeLabelsNS(norths)) 

	geom = st_sfc(c(eastlist, northlist, box), crs = datum)
	if (!is.na(crs))
		geom = st_transform(geom, crs)
	box = geom[length(geom)]
	geom = geom[-length(geom)]
	st_geometry(df) = geom

	df = st_cast(st_intersection(df, st_polygonize(box)), "MULTILINESTRING")
	graticule_attributes(df)
}

graticule_attributes = function(df) {
	object = st_geometry(df)
	xy = cbind(
		do.call(rbind, lapply(object, function(x) { y = x[[1]]; y[1,] } )),
		do.call(rbind, lapply(object, function(x) { y = x[[length(x)]]; y[nrow(y),] } ))
	)
	names(xy) = c("x_start", "y_start", "x_end", "y_end")
	df$x_start = xy[,1]
	df$y_start = xy[,2]
	df$x_end   = xy[,3]
	df$y_end   = xy[,4]
	dxdy = do.call(rbind, lapply(object, function(x) { y = x[[1]]; apply(y[1:2,], 2, diff) } ))
	df$angle_start = apply(dxdy, 1, function(x) atan2(x[2], x[1])*180/pi)
	dxdy = do.call(rbind, lapply(object, 
		function(x) { y = x[[length(x)]]; n = nrow(y); apply(y[(n-1):n,], 2, diff) } ))
	df$angle_end = apply(dxdy, 1, function(x) atan2(x[2], x[1])*180/pi)
	df
}

# copied from sp:
degreeLabelsNS = function(x) {
	pos = sign(x) + 2
	dir = c("*S", "", "*N")
	paste0(abs(x), "*degree", dir[pos])
}
degreeLabelsEW = function(x) {
	x <- ifelse(x > 180, x - 360, x)
	pos = sign(x) + 2
	if (any(x == -180))
		pos[x == -180] = 2
	if (any(x == 180))
		pos[x == 180] = 2
	dir = c("*W", "", "*E")
	paste0(abs(x), "*degree", dir[pos])
}
