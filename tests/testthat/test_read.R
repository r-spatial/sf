context("sf: read tests")

test_that("we can read a shapefile using st_read", {
  nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267)
  expect_identical(class(nc), c("sf", "data.frame"))
  expect_equal(dim(nc), c(100, 15))
})
