context("sf: read tests")

test_that("we can read a shapefile using st_read", {
  s = st_read(system.file("shapes/", package="maptools"), "sids")
  expect_identical(class(s), c("sf", "data.frame"))
  expect_equal(dim(s), c(100, 15))
})
