suppressPackageStartupMessages(library(sf))
bb = st_bbox(c(xmin=0, ymin=1, xmax=3, ymax=2))
xx <- st_make_grid(st_as_sfc(bb), square = FALSE, what = "polygons")
plot(xx, border = 'green', main = "pointy topped")
x <- st_make_grid(st_as_sfc(bb), square = FALSE, what = "points")
plot(x, add = TRUE)
x <- st_make_grid(st_as_sfc(bb), square = FALSE, what = "corners")
plot(x, col = 'orange', add = TRUE)
plot(st_as_sfc(bb), add = TRUE, border = 'red')
st_overlaps(xx) %>% 
	lengths() %>% 
	sum()

st_as_sfc(bb) %>%
	st_difference(st_union(xx)) %>%
	st_area()

xx <- st_make_grid(st_as_sfc(bb), square = FALSE, what = "polygons", flat_topped = TRUE)
plot(xx, border = 'green', main = "flat topped")
x <- st_make_grid(st_as_sfc(bb), square = FALSE, what = "points", flat_topped = TRUE)
plot(x, add = TRUE)
x <- st_make_grid(st_as_sfc(bb), square = FALSE, what = "corners", flat_topped = TRUE)
plot(x, col = 'orange', add = TRUE)
plot(st_as_sfc(bb), add = TRUE, border = 'red')
st_overlaps(xx) %>% 
	lengths() %>% 
	sum()

st_as_sfc(bb) %>%
	st_difference(st_union(xx)) %>%
	st_area()
