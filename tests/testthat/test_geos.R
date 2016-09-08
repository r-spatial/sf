context("sf: geos tests")

test_that("st_g_relate works", {
  r1 = sf:::st_g_relate(st_sfc(st_point(c(0,0))), st_sfc(st_linestring(rbind(c(0,0),c(1,1)))))
  library(sp)
  p = SpatialPoints(matrix(0,1,2))
  l = Lines(list(Line(rbind(c(0,0),c(1,1)))), "ID")
  sl = SpatialLines(list(l))
  library(rgeos)
  r2 = gRelate(p, sl)
  expect_true(all(r1 == r2))
})

test_that("st_g_isValid works", {
  expect_true( sf:::st_g_isValid(
  	st_sfc(st_polygon(list(cbind(c(0,1,1,0,0), c(0,0,1, 1,0)))))))
  expect_false(sf:::st_g_isValid(
  	st_sfc(st_polygon(list(cbind(c(0,1,1,.5,0),c(0,0,1,-1,0)))))))
})
