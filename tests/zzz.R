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
