context("sf: st_crs tests")

test_that("st_crs works", {
  nc1 = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267, quiet = TRUE)
  nc2 = st_read(system.file("shape/nc.shp", package="sf"), "nc", quiet = TRUE)
  nc3 = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = NA, quiet = TRUE)
  crs_4267 <- structure(
    list(epsg = 4267L, proj4string = "+proj=longlat +datum=NAD27 +no_defs"), 
    class = "crs")
  crs_na <- structure(
    list(epsg = NA_integer_, 
         proj4string = NA_character_), class = "crs")
  expect_equal(st_crs(nc2), crs_4267)
  expect_equal(st_crs(nc3), crs_na)
  expect_equal(st_set_crs(nc3, 4267) %>% st_crs, crs_4267)
  expect_equal(nc1, nc2)

  expect_warning(st_crs(nc2) <- 3857, "replacing crs does not reproject data")
  expect_silent(st_crs(nc2) <- 3857)
  expect_warning(st_crs(nc2) <- 0, "Failed to lookup UOM CODE")
  expect_warning(st_crs(nc2) <- 1000, "not found in EPSG")
  expect_silent(st_crs(nc1) <- st_crs(nc1))

  expect_error(st_crs("+proj=ll"), "invalid crs")
  expect_error(st_crs("+proj=longlat +datum=NAD26"), "invalid crs")
  expect_silent(st_crs("+proj=longlat"))
  expect_silent(st_crs("+proj=longlat +datum=NAD27"))
  a <- st_crs(4326)
  expect_silent(wkt <- st_as_text(a, pretty = TRUE))
  expect_silent(wkt <- st_as_text(a))
  expect_silent(b <- st_crs(wkt = wkt))
  expect_equal(a, b)
  expect_warning(sf:::CPL_crs_from_proj4string("foo"))
})
