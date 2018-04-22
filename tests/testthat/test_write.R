context("sf: write")

data(meuse, package = "sp")
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992)

test_that("sf can write to all writable formats", {
    # write to all formats available
    tf <- tempfile()
    drvs <- st_drivers()$name[sapply(st_drivers()$name,
		function(x) is_driver_can(x, operation = "write"))] %>% as.character()
    excluded_drivers = c("gps", # requires options
                         "gtm", # doesn't handle attributes
                         "nc",  # requires appropriate datum -> but writes in 4326, see below
                         "map", # doesn't support points
						 "ods") # generates valgrind error
    for (ext in setdiff(names(extension_map[extension_map %in% drvs]), excluded_drivers)) {
        expect_silent(st_write(meuse, paste0(tf, ".", ext), quiet = TRUE))
	}
	if ("netCDF" %in% drvs) {
		expect_silent(st_write(st_transform(meuse, st_crs(4326)), paste0(tf, ".nc"), quiet = TRUE))
	}
})

test_that("sf can write units (#264)", {
    tf <- tempfile(fileext = ".gpkg")
    meuse[["length"]] <- meuse[["cadmium"]]
    units(meuse$length) <- units::as_units("km")
    st_write(meuse, tf, quiet = TRUE)
    disc <- st_read(tf, quiet = TRUE)
    expect_is(disc[["length"]], "numeric")
    expect_equal(as.numeric(meuse[["length"]]), disc[["length"]])
})

test_that("delete and update work (#304) ", {
  skip_if_not("GPKG" %in% st_drivers()$name)  # shapefiles can't write point+multipoint mix:
  skip_on_os("mac")

  gpkg <- tempfile(fileext = ".gpkg")
  shp <- tempfile(fileext = ".shp")

  x <- st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_multipoint(matrix(1:4,2,2))))
  expect_error(st_write(x, gpkg, layer = c("a", "b"), driver = "GPKG", quiet = TRUE)) # error
  expect_error(st_write(x, gpkg,  driver = "foo", quiet = TRUE)) # error
  expect_output(st_write(x, gpkg, delete_dsn = TRUE), "Deleting source")
  expect_error(st_write(x, gpkg, update = FALSE, quiet = TRUE), "Dataset already exists")
  expect_output(st_write(x, gpkg, delete_dsn = TRUE), "Writing layer ")
  expect_output(st_write(x, gpkg, layer = "foo", delete_layer = TRUE), "Deleting layer `foo' failed")
  expect_output(st_write(x, gpkg, layer = "foo", delete_layer = TRUE), "Deleting layer `foo' using")
  expect_output(st_write(x, gpkg, layer = "foo", delete_layer = TRUE), "Updating layer `foo' to data source")
  expect_warning(
  	expect_error(st_write(x, gpkg, layer = ".", quiet = TRUE),
  				 "Layer creation failed"),
  	"special characters")
  expect_silent(st_layers(gpkg))
  expect_output(st_write(x, gpkg, layer = "foo", delete_dsn = TRUE), "Deleting source")
  expect_silent(st_layers(gpkg))
  expect_warning(
  	expect_error(write_sf(x, shp, "x"), "Feature creation failed"),
    "non-point")                  # on osx el capitan: "c++ exception (unknown reason)"
  expect_silent(x <- st_read(gpkg, quiet = TRUE))
  x <- st_sf(a = 1:2, geom = st_sfc(st_linestring(matrix(1:4,2,2)),
	st_multilinestring(list(matrix(1:4,2,2), matrix(10:13,2,2)))))
  expect_silent(write_sf(x, shp, "x"))
  expect_silent(write_sf(x, shp))
  expect_silent(x <- st_read(shp, quiet = TRUE))
  expect_silent(x <- read_sf(shp))
  expect_error(st_write(x, shp, driver = character(0), quiet = TRUE)) # err
})

test_that("layer is deleted when fails to create features (#549)", {
	skip_on_os("mac")
	shp <- tempfile(fileext = ".shp")
	x <- st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_multipoint(matrix(1:4,2,2))))
	expect_warning(expect_error(st_write(x, shp, "x", quiet = TRUE), "Feature creation failed"),
				   "non-point")
	expect_warning(expect_error(st_write(x, shp, "x", quiet = TRUE), "Feature creation failed"),
				   "non-point")
})

test_that("esri shapefiles shorten long field names", {
  shpx <- tempfile(fileext = ".shp")
  shpy <- tempfile(fileext = ".shp")
  nc <- st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267, quiet = TRUE)
  nc$this.is.a.very.long.field.name = 1
  expect_warning(st_write(nc, shpx, quiet = TRUE), "Field names abbreviated for ESRI Shapefile driver")
  nc$this.is.a.very.long.field.name2 = 2
  expect_warning(st_write(nc, shpy, quiet = TRUE), "Field names abbreviated for ESRI Shapefile driver")
  # expect_error(st_write(nc, shpz, quiet = TRUE), "Non-unique field names")
})
