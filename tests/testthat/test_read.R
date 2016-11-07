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

test_that("guess_driver works", {
  # hard to predict what these will do:
  drv = "ESRI Shapefile"
  names(drv) = "shp"
  expect_equal(sf:::guess_driver("nc.shp"), drv)
  try(sf:::guess_driver("nc.e00"))
  try(sf:::guess_driver("nc.gxt"))
  try(sf:::guess_driver("nc.gps"))
  try(sf:::guess_driver("nc.gtm"))
  try(sf:::guess_driver("nc.nc"))
  try(sf:::guess_driver("nc.map"))
  try(sf:::guess_driver("nc.sh"))
  try(sf:::guess_driver("nc.osm"))
  try(sf:::guess_driver("nc.pbf"))
  try(sf:::guess_driver("nc"))
})
