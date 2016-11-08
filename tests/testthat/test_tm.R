context("st_read")

test_that("st_read and write handle date and time", {
  library(sf)
  Sys.setenv(TZ="UTC")
  x = st_sf(a = 1:2, b=c(5.6,3), dt = Sys.Date()+1:2, tm = Sys.time()+2:3, 
	  geometry = st_sfc(st_point(c(1,1)),st_point(c(2,2))))
  st_write(x[-4], "x.shp")
  x2 = st_read("x.shp")
  expect_equal(x[-4], x2)
  if (Sys.getenv("USER") %in% c("edzer", "travis")) { # 
     x = st_sf(a = 1:2, b=c(5.6,3), dt = Sys.Date()+1:2, tm = Sys.time()+2:3, 
	    geom = st_sfc(st_point(c(1,1)),st_point(c(2,2))))
     st_write(x, "x.gpkg")
	 x2 = st_read("x.gpkg")
     expect_equal(x, x2)
  }
})
