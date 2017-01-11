context("sf: read tests")

test_that("we can read a shapefile using st_read", {
  nc <- st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267, quiet = TRUE)
  expect_identical(class(nc), c("sf", "data.frame"))
  expect_equal(dim(nc), c(100, 15))
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
  expect_error(guess_driver_can_write("x.e00"), "cannot write")
  expect_error(guess_driver_can_write("x.gdb"), "cannot write")
  
  expect_equal(guess_driver_can_write("x.geojson"), c("geojson" = "GeoJSON"))
  expect_equal(guess_driver_can_write("x.csv"), c("csv" = "CSV"))
  expect_equal(guess_driver_can_write("x.gml"), c("gml" = "GML"))
})

test_that("guess driver on windows with backslashes (#127)", {
    expect_identical(guess_driver("c:\\Temp\\this.shp"), 
                     guess_driver("c:/Temp/this.shp"))
})
