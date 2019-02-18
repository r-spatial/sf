context("normalize")
set.seed(1)
test_that("normalize", {
  p1 <- st_multipoint(matrix(runif(20, max = 25), ncol = 2))
  p1_norm <- st_normalize(p1)
  expect_true(all(st_bbox(p1_norm) == c(0,0,1,1)))

  p2 <- st_polygon(list(matrix(runif(10, max = 100), ncol = 2)[c(1:5, 1), ]))
  sfc <- st_sfc(p1, p2)
  sfc_norm <- st_normalize(sfc)
  expect_true(all(st_bbox(sfc_norm) == c(0,0,1,1)))

  sf <- st_sf(geometry = sfc)
  sf_norm <- st_normalize(sf)
  expect_true(all(st_bbox(sf_norm) == c(0,0,1,1)))
  expect_equal(sfc_norm, sf_norm$geometry)
})
