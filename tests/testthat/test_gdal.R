context("sf: gdal tests")

test_that("st_transform works", {
  library(sp)
  
  s = st_sfc(st_point(c(1,1)), st_point(c(10,10)), st_point(c(5,5)), crs = 4326)
  s1.tr = st_transform(s, 3857)
  
  sp = as(s, "Spatial")
  sp.tr = spTransform(sp, CRS("+init=epsg:3857"))
  s2.tr = st_as_sfc(sp.tr)
  #attr(s1.tr, "crs")$proj4string = ""
  #attr(s2.tr, "crs")$proj4string = ""
  st_crs(s1.tr) = NA_crs_
  st_crs(s2.tr) = NA_crs_
  expect_equal(s1.tr, s2.tr)

  toCrs = 3857
  s1.tr = st_transform(s, toCrs)
  #attr(s1.tr, "crs")$proj4string = ""
  st_crs(s1.tr) = NA_crs_
  st_crs(s2.tr) = NA_crs_
  expect_equal(s1.tr, s2.tr)

  expect_silent({
    sf.tr = st_transform(st_sf(a=1:3, s), toCrs) # for sf
    sfg.tr = st_transform(structure(s[[1]], proj4string="+proj=longlat +datum=WGS84 +no_defs"), toCrs) # sfg
  })
})

test_that("gdal can be loaded, unloaded, and loaded", {
  expect_silent({
  sf:::.onLoad()
  sf:::.onUnload()
  sf:::.onLoad()
  }
  )
})

test_that("st_wrap_dateline works", {
	expect_silent(x <- st_wrap_dateline(st_sfc(st_linestring(rbind(c(-179,0),c(179,0))), crs = 4326)))
})
