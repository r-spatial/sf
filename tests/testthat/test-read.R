test_that("we can read a shapefile using st_read", {
  nc <- st_read(system.file("shape/nc.shp", package = "sf"), "nc", crs = 4267, quiet = TRUE)
  expect_s3_class(nc, c("sf", "data.frame"), exact = TRUE)
  expect_equal(dim(nc), c(100, 15))
})

test_that("we can read shapefiles with a query string", {
    nc <- st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267, quiet = TRUE)
    nc_all <- st_read(system.file("shape/nc.shp", package="sf"), query = "select * from nc", crs = 4267, quiet = TRUE)
    nc_some <- st_read(system.file("shape/nc.shp", package="sf"), query = "select * from nc where SID79 > 50", crs = 4267, quiet = TRUE)
})

test_that("st_read.default gives error messages", {
  expect_error(st_read(), "dsn should specify a data source or filename")
  expect_error(st_read(NULL), "no st_read method available for objects of class NULL")
})

test_that("st_write.sf gives error messages on unknown dsn", {
  expect_error(st_write(st_sf(st_sfc(st_point())), NULL), "no st_write method available for dsn of class NULL")
})

test_that("stringsAsFactors = FALSE produces a data.frame with no factors", {
  nc <-  st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267,
                 stringsAsFactors = FALSE, quiet = TRUE)
  expect_false(any(sapply(nc, class) == "factor"))
})

test_that("stringsAsFactors = TRUE produces a data.frame with factors", {
  nc <-  st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267,
                 stringsAsFactors = TRUE, quiet = TRUE)
  expect_true(any(sapply(nc, class) == "factor"))
})

test_that("drivers extensions are all lowercase", {
  expect_equal(names(extension_map), tolower(names(extension_map)))
  expect_equal(names(prefix_map), tolower(names(prefix_map)))
})

test_that("guess_driver is strict", {
  expect_error(guess_driver(c("PG:xyz", "x.shp")), "length")
  expect_error(guess_driver(1), "character")
})

test_that("guess_driver works on extensions", {
  # for repeatability, this is how I turned the list to tests
  # `^"(\w+)" = ("[\w\s]+"),?`
  # to `expect_equal(guess_driver("nc.\1"), c("\1" = \2))`

  expect_equal(guess_driver("nc.bna"), c("bna" = "BNA"))
  expect_equal(guess_driver("nc.csv"), c("csv" = "CSV"))
  expect_equal(guess_driver("nc.e00"), c("e00" = "AVCE00"))
  expect_equal(guess_driver("nc.gdb"), c("gdb" = "OpenFileGDB"))
  expect_equal(guess_driver("nc.geojson"), c("geojson" = "GeoJSON"))
  expect_equal(guess_driver("nc.gml"), c("gml" = "GML"))
  expect_equal(guess_driver("nc.gmt"), c("gmt" = "GMT"))
  expect_equal(guess_driver("nc.gpkg"), c("gpkg" = "GPKG"))
  expect_equal(guess_driver("nc.gps"), c("gps" = "GPSBabel"))
  expect_equal(guess_driver("nc.gtm"), c("gtm" = "GPSTrackMaker"))
  expect_equal(guess_driver("nc.gxt"), c("gxt" = "Geoconcept"))
  expect_equal(guess_driver("nc.kml"), c("kml" = "KML"))
  expect_equal(guess_driver("nc.jml"), c("jml" = "JML"))
  expect_equal(guess_driver("nc.map"), c("map" = "WAsP"))
  expect_equal(guess_driver("nc.mdb"), c("mdb" = "Geomedia"))
  expect_equal(guess_driver("nc.nc"), c("nc" = "netCDF"))
  expect_equal(guess_driver("nc.ods"), c("ods" = "ODS"))
  expect_equal(guess_driver("nc.osm"), c("osm" = "OSM"))
  expect_equal(guess_driver("nc.pbf"), c("pbf" = "OSM"))
  expect_equal(guess_driver("nc.shp"), c("shp" = "ESRI Shapefile"))
  expect_equal(guess_driver("nc.sqlite"), c("sqlite" = "SQLite"))
  expect_equal(guess_driver("nc.vdv"), c("vdv" = "VDV"))
  expect_equal(guess_driver("nc.xls"), c("xls" = "xls"))
  expect_equal(guess_driver("nc.xlsx"), c("xlsx" = "XLSX"))

  # unsuported
  expect_equal(guess_driver("nc.notsupported"), NA)
})

test_that("guess_driver works on suffixes", {
  # for repeatability, this is how I turned the list to tests
  # `^"(\w+)" = ("[\w\s]+"),?`
  # to `expect_equal(guess_driver("nc.\1"), c("\1" = \2))`

  expect_equal(guess_driver("couchdb:nc"), c("couchdb" = "CouchDB"))
  expect_equal(guess_driver("db2odbc:nc"), c("db2odbc" = "DB2ODBC"))
  expect_equal(guess_driver("dods:nc"), c("dods" = "DODS"))
  expect_equal(guess_driver("gft:nc"), c("gft" = "GFT"))
  expect_equal(guess_driver("mssql:nc"), c("mssql" = "MSSQLSpatial"))
  expect_equal(guess_driver("mysql:nc"), c("mysql" = "MySQL"))
  expect_equal(guess_driver("oci:nc"), c("oci" = "OCI"))
  expect_equal(guess_driver("odbc:nc"), c("odbc" = "ODBC"))
  expect_equal(guess_driver("pg:nc"), c("pg" = "PostgreSQL"))
  expect_equal(guess_driver("sde:nc"), c("sde" = "SDE"))

  # upper case
  expect_equal(guess_driver("CouchDB:nc"), c("couchdb" = "CouchDB"))
  expect_equal(guess_driver("db2ODBC:nc"), c("db2odbc" = "DB2ODBC"))
  expect_equal(guess_driver("DODS:nc"), c("dods" = "DODS"))
  expect_equal(guess_driver("GFT:nc"), c("gft" = "GFT"))
  expect_equal(guess_driver("MSSQL:nc"), c("mssql" = "MSSQLSpatial"))
  expect_equal(guess_driver("MYSQL:nc"), c("mysql" = "MySQL"))
  expect_equal(guess_driver("OCI:nc"), c("oci" = "OCI"))
  expect_equal(guess_driver("ODBC:nc"), c("odbc" = "ODBC"))
  expect_equal(guess_driver("PG:nc"), c("pg" = "PostgreSQL"))
  expect_equal(guess_driver("SDE:nc"), c("sde" = "SDE"))

  # unsuported
  expect_equal(guess_driver("notsupported:nc"), NA)
})

test_that("weird names are supported", {
  expect_equal(guess_driver("pg:nc.shp"), c("pg" = "PostgreSQL"))
  expect_equal(guess_driver("pg:nc.shp.e00"), c("pg" = "PostgreSQL"))
  expect_equal(guess_driver("nc.shp.e00"), c("e00" = "AVCE00"))
  expect_equal(guess_driver("couchdb:shp"), c("couchdb" = "CouchDB"))

  expect_equal(guess_driver("notsupported:nc.shp"), NA)
  expect_equal(guess_driver("notsupported"), NA)
})

test_that("driver utils work", {
  expect_true(is_driver_available("shp", data.frame(name = c("x", "y", "shp"))))
  expect_true(is_driver_available("shp", data.frame(name = c("shp"))))
  expect_false(is_driver_available("shp", data.frame(name = c("x", "y", "z"))))
  expect_false(is_driver_available("shp", data.frame(name = c("x", "y", "z"))))

  expect_error(is_driver_can("shp", data.frame(name = c("x", "y", "shp")), operation = "nothing"))
  expect_error(is_driver_can("shp", data.frame(name = c("x", "y")), operation = "nothing"))
  expect_true(is_driver_can("shp", data.frame(name = c("x", "y", "shp"), write = rep(TRUE, 3)), operation = "write"))
  expect_false(is_driver_can("shp", data.frame(name = c("x", "y", "shp"), write = c(TRUE, TRUE, FALSE)), operation = "write"))
})

test_that("guess_driver_can_write", {
  expect_error(guess_driver_can_write("x.e00", NA), "Could not guess")
  expect_error(guess_driver_can_write("x.not", c("nothing" = "nothing")), "not available")
  expect_equal(guess_driver_can_write("x.csv"), c("csv" = "CSV"))
  expect_equal(guess_driver_can_write("c:/x.csv"), c("csv" = "CSV"))

  expect_error(guess_driver_can_write("x.unsuported"), "Could not guess driver")
  expect_error(guess_driver_can_write("unsuported:x"), "Could not guess driver")
})

test_that("driver operations", {
  # These tests are driver specifics to GDAL version and OS.
  expect_error(guess_driver_can_write("x.e00"), "AVCE00 driver not available|cannot write")
  # expect_error(guess_driver_can_write("x.gdb"), "cannot write") -> no longer the case when GDAL >= 3.6.0

  expect_equal(guess_driver_can_write("x.geojson"), c("geojson" = "GeoJSON"))
  expect_equal(guess_driver_can_write("x.csv"), c("csv" = "CSV"))
  expect_equal(guess_driver_can_write("x.gml"), c("gml" = "GML"))
})

test_that("guess driver on windows with backslashes (#127)", {
    expect_identical(guess_driver("c:\\Temp\\this.shp"),
                     guess_driver("c:/Temp/this.shp"))
})

test_that("the first layer is selected with a warning", {
    skip_on_os("mac")

	expect_warning(st_read(system.file("osm/overpass.osm", package="sf"), quiet = TRUE),
		"automatically selected the first layer")
	expect_error(st_read(system.file("osm/overpass.osm", package="sf"), "foo", quiet = TRUE),
		"Opening layer failed")
})

test_that("we get a warning when not specifying one of multiple layers", {
 	skip_if_not("OSM" %in% st_drivers()$name && Sys.info()['sysname'] != "Darwin")

	Sys.setenv(OSM_USE_CUSTOM_INDEXING="NO")
	osm = system.file("osm/overpass.osm", package="sf")
	expect_warning(st_read(osm, quiet = TRUE),
		"automatically selected the first layer in a data source containing more than one.")
})

test_that("reading non-spatial table works", {
    skip_if_not(sf_extSoftVersion()[["GDAL"]] >= "2.2.0") # error on OSX for 2.1.3

    expect_warning(st_read(system.file("gpkg/nospatial.gpkg", package = "sf")),
                   "no simple feature geometries present")
    expect_warning(
        expect_s3_class(st_read(system.file("gpkg/nospatial.gpkg", package = "sf")),
                        "data.frame"),
        "no simple feature geometries"
    )
#   expect_warning(
#       expect_s3_class(read_sf(system.file("gpkg/nospatial.gpkg", package = "sf")),
#             "tbl_df"),
#       "no simple feature geometries"
#   )
})

test_that("Missing data sources have useful error message (#967)", {
	# write temporary file
	x <- tempfile(fileext = ".kml")
	cat("empty", file = x)

	expect_error(st_read(tempfile(fileext = ".csv")), "The file doesn't seem to exist.")
	expect_error(st_read("PG:host=wrong"), "Check connection parameters.")
	expect_error(st_read(x), "The source could be corrupt or not supported.")
	expect_error(st_read(""), "not an empty string.")

	# delete temp file
	file.remove(x)
})
