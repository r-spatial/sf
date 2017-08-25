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
        st_write(meuse, paste0(tf, ".", ext), quiet = TRUE)
		cat(paste(ext, "\n"))
	}
	if ("netCDF" %in% drvs) {
		st_write(st_transform(meuse, st_crs(4326)), paste0(tf, ".nc"), quiet = TRUE)
		cat(paste(".nc", "\n"))
	}
})

test_that("sf can write units (#264)", {
    tf <- tempfile(fileext = ".gpkg")
    meuse[["length"]] <- meuse[["cadmium"]]
    units(meuse$length) <- units::make_unit("km")
    st_write(meuse, tf, quiet = TRUE)
    disc <- st_read(tf, quiet = TRUE)
    expect_is(disc[["length"]], "numeric")
    expect_equal(as.numeric(meuse[["length"]]), disc[["length"]])
})

test_that("delete and update work (#304) ", { 
  skip_if_not("GPKG" %in% st_drivers()$name)  # shapefiles can't write point+multipoint mix:
  skip_on_os("mac")

  x <- st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_multipoint(matrix(1:4,2,2))))
  expect_error(st_write(x, "x.gpkg", layer = c("a", "b"), driver = "GPKG")) # error
  expect_error(st_write(x, "x.gpkg",  driver = "foo")) # error
  expect_output(st_write(x, "x.gpkg", delete_dsn = TRUE), "Deleting source")
  expect_error(st_write(x, "x.gpkg", update = FALSE, quiet = TRUE), "Dataset already exists") 
  expect_output(st_write(x, "x.gpkg", delete_dsn = TRUE), "Writing layer `x'")
  expect_output(st_write(x, "x.gpkg", layer = "foo", delete_layer = TRUE), "Deleting layer `foo' failed")
  expect_output(st_write(x, "x.gpkg", layer = "foo", delete_layer = TRUE), "Deleting layer `foo' using")
  expect_output(st_write(x, "x.gpkg", layer = "foo", delete_layer = TRUE), "Updating layer `foo' to data source")
  expect_error(st_write(x, "x.gpkg", layer = ".", quiet = TRUE), "Layer creation failed")
  expect_silent(st_layers("x.gpkg"))
  expect_output(st_write(x, "x.gpkg", layer = "foo", delete_dsn = TRUE), "Deleting source")
  expect_silent(st_layers("x.gpkg"))
  expect_error(write_sf(x, "x.shp", "x"), "Feature creation failed") # on osx el capitan: "c++ exception (unknown reason)"
  expect_error(write_sf(x, "x.shp", "x"))
  expect_silent(x <- st_read("x.gpkg", quiet = TRUE))
  x <- st_sf(a = 1:2, geom = st_sfc(st_linestring(matrix(1:4,2,2)), 
	st_multilinestring(list(matrix(1:4,2,2), matrix(10:13,2,2)))))
  # expect_output(st_write(x, "x.shp"))
  expect_silent(write_sf(x, "x.shp", "x"))
  expect_silent(write_sf(x, "x.shp"))
  expect_silent(x <- st_read("x.shp", quiet = TRUE))
  expect_silent(x <- read_sf("x.shp"))
  expect_error(st_write(x, "x.shp", driver = character(0))) # err
})

test_that("esri shapefiles shorten long field names", {
  nc <- st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267, quiet = TRUE)
  nc$this.is.a.very.long.field.name = 1
  expect_warning(st_write(nc, "ncx.shp", quiet = TRUE), "Field names abbreviated for ESRI Shapefile driver")
  nc$this.is.a.very.long.field.name2 = 2
  expect_warning(st_write(nc, "ncy.shp", quiet = TRUE), "Field names abbreviated for ESRI Shapefile driver")
  # expect_error(st_write(nc, "ncy.shp", quiet = TRUE), "Non-unique field names")
})
