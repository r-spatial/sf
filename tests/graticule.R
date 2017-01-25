library(maps)
m = map('usa', plot = FALSE, fill = TRUE)
suppressPackageStartupMessages(library(sf))
m0 <- st_as_sfc(m)
m <- st_as_sf(m)

laea = st_crs("+proj=laea +lat_0=30 +lon_0=-95") # Lambert equal area
m <- st_transform(st_as_sf(m), laea)

bb = st_bbox(m)
bbox = st_linestring(rbind(c( bb[1],bb[2]),c( bb[3],bb[2]),c( bb[3],bb[4]),c( bb[1],bb[4]),c( bb[1],bb[2])))

g = st_graticule(m)
plot(m, xlim = 1.2 * c(-2450853.4, 2186391.9))
plot(g[1], add = TRUE, col = 'grey')
plot(bbox, add = TRUE)
points(g$x_start, g$y_start, col = 'red')
points(g$x_end, g$y_end, col = 'blue')

invisible(lapply(seq_len(nrow(g)), function(i) {
	if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000)
		text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]), 
			srt = g$angle_start[i], pos = 2, cex = .7)
	if (g$type[i] == "E" && g$y_start[i] - min(g$y_start) < 1000)
		text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]), 
			srt = g$angle_start[i] - 90, pos = 1, cex = .7)
	if (g$type[i] == "N" && g$x_end[i] - max(g$x_end) > -1000)
		text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]), 
			srt = g$angle_end[i], pos = 4, cex = .7)
	if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000)
		text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]), 
			srt = g$angle_end[i] - 90, pos = 3, cex = .7)
}))

plot(m, graticule = st_crs(4326))
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
# options(warn=2)
g = st_graticule(nc, datum = st_crs(nc))
#g = st_graticule(nc)

plot(nc[1], graticule = st_crs(nc))

plot(nc[1], graticule = st_crs(nc), axes = TRUE)

g = st_graticule()
