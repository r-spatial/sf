context("sf: st_crs tests")

test_that("st_crs works", {
  library(sf)
  nc1 = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg", crs = 4267)
  nc2 = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg")
  st_crs(nc2) <- 4267
  expect_equal(nc1, nc2)

  expect_warning(st_crs(nc2) <- 3857)
  expect_silent(st_crs(nc1) <- st_crs(nc1))
})
