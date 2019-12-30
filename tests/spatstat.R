# put this test last, because of spatstat side effect on units:
suppressPackageStartupMessages(library(spatstat))
suppressPackageStartupMessages(library(sf))
data(chicago)
st_as_sf(chicago)
# ppp:
g = gorillas
st_as_sf(g)
marks(g) = NULL
st_as_sf(g)

# multipolygon: https://github.com/r-spatial/sf/issues/1161
if (require(maptools)) {
#> Loading required package: sp
#> Checking rgeos availability: TRUE
  window = read_sf(system.file("shape/nc.shp", package = "sf")) %>%
  	st_transform(32119)

  win = spatstat::as.owin(as(window, "Spatial"))

  set.seed(1331)
  pp2a = runifpoint(n = 50, win = win)
  print(st_as_sf(pp2a))
}

# st_sample going the spatstat way
x <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 0)))))
try(pts <- st_sample(x, type = "thomas"))
try(pts <- st_sample(x, kappa = 1, mu = 10, type = "Thomas"))
# points expected
set.seed(1331)
pts <- st_sample(x, kappa = 1, mu = 10, scale = 0.1, type = "Thomas")
#plot(x)
#plot(pts, add = TRUE)
pts
