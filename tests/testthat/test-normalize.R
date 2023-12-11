set.seed(1)
test_that("normalize", {
  p0 <- st_point(c(0,1))
  p0_norm <- st_normalize(p0, c(0,0,10,10))
  expect_equal(p0_norm, st_point(c(0,0.1)))

  p1 <- st_multipoint(matrix(runif(20, max = 25), ncol = 2))
  p1_norm <- st_normalize(p1)
  expect_equal(unclass(st_bbox(p1_norm)), c(0,0,1,1), check.attributes = FALSE)

  p2 <- st_polygon(list(matrix(runif(10, max = 100), ncol = 2)[c(1:5, 1), ]))
  sfc <- st_sfc(p1, p2)
  sfc_norm <- st_normalize(sfc)
  expect_equal(unclass(st_bbox(sfc_norm)), c(0,0,1,1), check.attributes = FALSE)

  sf <- st_sf(geometry = sfc)
  sf_norm <- st_normalize(sf)
  expect_equal(unclass(st_bbox(sf_norm)), c(0,0,1,1), check.attributes = FALSE)
  expect_equal(sfc_norm, sf_norm$geometry)

})

test_that("ops", {
  p0 <- st_point(c(0, 1, 2))
  expect_equal(st_sfc(p0) - st_sfc(st_point(c(1,2,3))), st_sfc(st_point(c(-1,-1,-1))))
  expect_equal(st_sfc(p0) * st_sfc(st_point(c(1,2,3))), st_sfc(st_point(c(0,2,6))))
})

test_that("grob stuff", {
  p0 <- st_point(c(0,1))
  g <- st_as_grob(st_sfc(p0))
  p1 <- st_linestring(matrix(runif(20, max = 25), ncol = 2))
  g <- st_as_grob(st_sfc(p1))
  p2 <- st_polygon(list(matrix(runif(10, max = 100), ncol = 2)[c(1:5, 1), ]))
  g <- st_as_grob(st_sfc(p2))
})
