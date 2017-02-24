library(sf)
library(maptools)
library(animation)
data(wrld_simpl)
w <- st_as_sf(wrld_simpl) %>% st_cast("POLYGON")
w$f = factor(sample(1:12, nrow(w), replace = TRUE))
w <- st_cast(w, "POLYGON")

m = st_make_grid()
m = st_segmentize(m, 4e5)
saveGIF(
for (i in 0:100) {
	par(mar = rep(0,4))
	lat=30+i  #+rnorm(1)
	lon=-10-i #+rnorm(1)
	p4s=paste0("+proj=ortho +lat_0=", lat, " +lon_0=", lon)
	plot(st_transform(m, st_crs(p4s), check = TRUE), col = 'lightblue', border = 'grey')
	plot(st_transform(w["f"], st_crs(p4s), check = TRUE), add = TRUE)
}
, interval = 0.1)
