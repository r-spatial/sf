context("sf_project tests")

test_that("sf_project works", {
  sf_project(pts = matrix(0:1, 1, 2), st_crs(4326), st_crs(3587))
})
