context("sf: read tests")

test_that("we can read a shapefile using st_read", {
  nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg", crs = 4267)
  expect_identical(class(nc), c("sf", "data.frame"))
  expect_equal(dim(nc), c(100, 15))
})
