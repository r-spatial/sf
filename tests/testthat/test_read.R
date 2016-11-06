context("sf: read tests")

test_that("we can read a shapefile using st_read", {
  nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267)
  expect_identical(class(nc), c("sf", "data.frame"))
  expect_equal(dim(nc), c(100, 15))
})

test_that("stringsAsFactors = FALSE produces a data.frame with no factors", {
  nc <-  st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267,
                 stringsAsFactors = FALSE)
  expect_false(any(sapply(nc, class) == "factor"))
})

test_that("stringsAsFactors = TRUE produces a data.frame with factors", {
  nc <-  st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267,
                 stringsAsFactors = TRUE)
  expect_true(any(sapply(nc, class) == "factor"))
})
