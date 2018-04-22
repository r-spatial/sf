context("sf: st_crs tests")

test_that("st_crs works", {
  nc1 = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267, quiet = TRUE)
  nc2 = st_read(system.file("shape/nc.shp", package="sf"), "nc", quiet = TRUE)
  nc3 = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = NA, quiet = TRUE)
  crs_4267 <- st_crs(4267)
  expect_equal(st_crs(nc2), crs_4267)
  expect_equal(st_crs(nc3), NA_crs_)
  expect_equal(st_set_crs(nc3, 4267) %>% st_crs, crs_4267)
  expect_equal(nc1, nc2)

  expect_warning(st_crs(nc2) <- 3857, "replacing crs does not reproject data")
  expect_silent(st_crs(nc2) <- 3857)
  #expect_warning(st_crs(nc2) <- 0, "Failed to lookup UOM CODE") -> changes in gdal 2.2:
  #expect_warning(st_crs(nc2) <- 0)
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
  # expect_equal(a, b) # -> breaks on CRAN/fedora
  expect_warning(sf:::CPL_crs_from_proj4string("foo"))
  # expect_true(st_crs("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") == st_crs("+proj=longlat +datum=WGS84 +no_defs"))
})

test_that("st_proj_info works", {
  expect_silent(x <- st_proj_info("proj"))
  expect_silent(x <- st_proj_info("ellps"))
  expect_silent(x <- st_proj_info("datum"))
  expect_silent(x <- st_proj_info("units"))
  expect_silent(x <- st_proj_info("have_datum_files"))
})

test_that("$.crs works", {
  expect_true(is.numeric(st_crs("+init=epsg:3857")$epsg))
  expect_true(is.character(st_crs("+init=epsg:3857")$proj4string))
  expect_true(is.numeric(st_crs("+init=epsg:3857 +units=km")$b)) 
  expect_true(is.character(st_crs("+init=epsg:3857 +units=km")$units))
})

test_that("CRS comparison uses ellipsoid and datum (#180)", {
	expect_equal(
		st_crs("+proj=tmerc +lat_0=0 +lon_0=0 +k=0.9999 +x_0=304800 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"),
		st_crs("+proj=tmerc +lat_0=0 +lon_0=0 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m +no_defs"))
})

test_that("Can create dummy crs", {
    expect_equal(st_crs(0, valid = FALSE), structure(list(epsg = 0, proj4string = ""), class = "crs"))
    expect_equal(st_crs(991115, proj4text = "+random", valid = FALSE),
                 structure(list(epsg = 991115, proj4string = "+random"), class = "crs"))
})

test_that("Warning if trying to supply proj4 with numeric", {
    expect_warning(st_crs(2939, proj4text = "+random"), "`proj4text` is not used to validate crs")
})
