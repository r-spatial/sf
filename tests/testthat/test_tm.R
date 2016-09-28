context("st_bbox")

test_that("st_read and write handle date and time", {
  library(sf)
  Sys.setenv(TZ="UTC")
  x = st_sf(a = 1:2, b=c(5.6,3), dt = Sys.Date()+1:2, tm = Sys.time()+2:3, 
	  geom = st_sfc(st_point(c(1,1)),st_point(c(2,2))))
  st_write(x, "x.gpkg", "x", "GPKG")
  x2 = st_read("x.gpkg", "x")
  expect_equal(x, x2)
})
