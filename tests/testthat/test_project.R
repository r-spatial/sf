test_that("sf_project works", {
  # local modification msquinn@: Requires a newer version of stars package
  testthat::skip_on_google()
  sf_project(pts = matrix(0:1, 1, 2), st_crs(4326), st_crs(3587))
})
