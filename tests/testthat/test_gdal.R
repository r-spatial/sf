context("sf: gdal tests")

test_that("st_transform works", {
  library(sf)
  (s = st_sfc(st_point(c(1,1)), st_point(c(10,10)), st_point(c(5,5)), crs = 4326))
  toCrs = "+init=epsg:3857"
  (s1.tr = st_transform(s, toCrs))
  
  library(sp)
  sp = as(s, "Spatial")
  sp.tr = spTransform(sp, CRS(toCrs))
  (s2.tr = st_as_sfc(sp.tr))
  attr(s2.tr, "crs")$proj4string = "" # they take different tours to fill proj4string from epsg
  attr(s1.tr, "crs")$proj4string = ""
  expect_equal(s1.tr, s2.tr)

  toCrs = 3857
  (s1.tr = st_transform(s, toCrs))
  attr(s1.tr, "crs")$proj4string = ""
  expect_equal(s1.tr, s2.tr)

  sf.tr = st_transform(st_sf(a=1:3, s), toCrs) # for sf
  sfg.tr = st_transform(structure(s[[1]], proj4string="+proj=longlat +datum=WGS84 +no_defs"), toCrs) # sfg
})

test_that("gdal can be loaded, unloaded, and loaded", {
  library(sf)
  sf:::.onLoad()
  sf:::.onUnload()
  sf:::.onLoad()
})
