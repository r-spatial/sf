test_that("st_crs works", {
  nc1 = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267, quiet = TRUE)
  nc2 = st_read(system.file("shape/nc.shp", package="sf"), "nc", quiet = TRUE)
  nc3 = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = NA, quiet = TRUE)
  crs_4267 <- st_crs(4267)
  expect_equal(st_crs(nc1)[1:2], crs_4267[1:2])
  # expect_equal(st_crs(nc2)[1:2], crs_4267[1:2]) 
  expect_equal(st_crs(nc3), NA_crs_)
  expect_equal(st_set_crs(nc3, 4267) %>% st_crs, crs_4267)
  # expect_equal(st_crs(nc1)[1:2], st_crs(nc2)[1:2])

  expect_warning(st_crs(nc2) <- 3857, "replacing crs does not reproject data")
  expect_silent(st_crs(nc2) <- 3857)
  #expect_warning(st_crs(nc2) <- 0, "Failed to lookup UOM CODE") -> changes in gdal 2.2:
  #expect_warning(st_crs(nc2) <- 0)
  #expect_warning(st_crs(nc2) <- 1000, "not found in EPSG") -> changes in gdal 2.5.0
  expect_silent(st_crs(nc1) <- st_crs(nc1))

  if (sf_extSoftVersion()[["GDAL"]] > "2.2.3") {
    expect_error(st_crs("+proj=ll"), "invalid crs")
  	# expect_error(st_crs("+proj=longlat +datum=NAD26"))
  }
  expect_silent(st_crs("+proj=longlat"))
  expect_silent(st_crs("+proj=longlat +datum=NAD27"))
  a <- st_crs(4326)
  expect_silent(wkt <- st_as_text(a, pretty = TRUE))
  expect_silent(wkt <- st_as_text(a))
  expect_silent(b <- st_crs(wkt))
  # expect_equal(a, b) # -> breaks on CRAN/fedora
  expect_true(st_crs("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs") != st_crs("+proj=longlat +datum=WGS84 +no_defs"))
})

test_that("sf_proj_info works", {
  expect_silent(x <- sf_proj_info("proj"))
  expect_silent(x <- sf_proj_info("ellps"))
  expect_silent(x <- sf_proj_info("datum"))
  expect_silent(x <- sf_proj_info("units"))
  expect_silent(path <- sf_proj_info("path")[1])
  expect_true(is.logical(sf_proj_info(path = path)))
  expect_true(is.logical(sf_proj_info("network")))
})

test_that("sf_proj_info works for datum files", {
  skip_if_not(sf_extSoftVersion()[["proj.4"]] < "6.0.0")
  expect_silent(x <- sf_proj_info("have_datum_files"))
})

test_that("$.crs works", {
  skip_if_not(sf_extSoftVersion()[["proj.4"]] < "6.0.0")
  expect_true(!is.null(st_crs("+init=epsg:3857")$epsg))
  expect_true(is.character(st_crs("+init=epsg:3857")$proj4string))
})

test_that("$.crs works with +units", {
  skip_if_not(sf_extSoftVersion()[["proj.4"]] < "6.0.0")
  expect_true(is.numeric(st_crs("+init=epsg:3857 +units=m")$b)) 
  expect_true(is.character(st_crs("+init=epsg:3857 +units=m")$units))
})

test_that("$.crs works 2", {
  skip_if_not(sf_extSoftVersion()[["GDAL"]] < "2.5.0" && sf_extSoftVersion()[["proj.4"]] < "6.0.0")
  expect_true(is.numeric(st_crs("+init=epsg:3857 +units=km")$b)) 
  expect_true(is.character(st_crs("+init=epsg:3857 +units=km")$units))
})

test_that("CRS comparison uses ellipsoid and datum (#180)", {
#  skip_if_not(sf_extSoftVersion()[["GDAL"]] < "2.5.0")
#  expect_equal(
#    st_crs("+proj=tmerc +lat_0=0 +lon_0=0 +k=0.9999 +x_0=304800 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"),
#    st_crs("+proj=tmerc +lat_0=0 +lon_0=0 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m +no_defs"))
})

#test_that("Can create dummy crs", {
#    #expect_equal(st_crs(0, valid = FALSE), structure(list(epsg = 0, proj4string = ""), class = "crs"))
#    #expect_equal(st_crs(991115, proj4text = "+random", valid = FALSE),
#    #             structure(list(epsg = 991115, proj4string = "+random"), class = "crs"))
#})

#test_that("Warning if trying to supply proj4 with numeric", {
#    expect_warning(st_crs(2939, proj4text = "+random"), "`proj4text` is not used to validate crs")
#})

test_that("old-style crs are repaired", {
  x = structure(list(proj4string = "+proj=longlat", epsg = 4326), class = "crs")
  x_new = st_crs(x)
  expect_warning(x$proj4string)
})

test_that("sp-style CRS objects are accepted", {
  skip_if_not_installed("sp")
  library(sp)
  x = CRS("+proj=longlat")
  x_crs = st_crs("+proj=longlat")
  expect_equal(x_crs$wkt, st_crs(x)$wkt)
  comment(x) = NULL
  expect_equal(x_crs$wkt, st_crs(x)$wkt)
})

test_that("print.crs works", {
  x = st_crs(4326)
  print(x)
  x$input = NA
  print(x)
})

test_that("crs.Raster works", {
  skip_if_not_installed("raster")
  library(raster)
  r = raster()
  x = st_crs(r)
  expect_equal(class(x), "crs")
})
