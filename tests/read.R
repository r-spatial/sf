Sys.setenv(TZ="UTC")
suppressPackageStartupMessages(library(sf))
if ("GPKG" %in% st_drivers()$name) {
	tst = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg", crs = 4267, quiet = TRUE)
	tst = st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE) # default layer name
}

tst = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE) # no layer specified

# data from https://github.com/edzer/sfr/issues/6
tst = st_read(system.file("shape/storms_xyz.shp", package="sf"), quiet = TRUE)
class(st_geometry(tst))
class(st_geometry(tst)[[1]])
tst = st_read(system.file("shape/storms_xyzm.shp", package="sf"), quiet = TRUE)
class(st_geometry(tst))
class(st_geometry(tst)[[1]])
tst = st_read(system.file("shape/storms_xyz_feature.shp", package="sf"), quiet = TRUE)
class(st_geometry(tst))
class(st_geometry(tst)[[1]])
tst = st_read(system.file("shape/storms_xyzm_feature.shp", package="sf"), quiet = TRUE)
class(st_geometry(tst))
class(st_geometry(tst)[[1]])

if ("GPKG" %in% st_drivers()$name) { # read Int64
    print(st_read(system.file("gpkg/tl.gpkg", package="sf"), quiet = TRUE)$AWATER)
    print(st_read(system.file("gpkg/tl.gpkg", package="sf"), quiet = TRUE, int64_as_string = TRUE)$AWATER)
}

# see https://github.com/edzer/sfr/issues/45 :
if ("OSM" %in% st_drivers()$name && Sys.info()['sysname'] != "Darwin") {
	osm = system.file("osm/overpass.osm", package="sf")
	print(st_layers(osm))
	suppressWarnings(print(st_layers(osm, do_count = TRUE)))
	suppressWarnings(print(st_read(osm, "multipolygons", quiet = TRUE)))
}

# layer opening option:
st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE,
	options = c("ADJUST_TYPE=YES", "ENCODING=CPL_ENC_UTF8"))

x <- st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_multipoint(matrix(1:4,2,2))))

try(st_layers("foo")) # cannot open datasource
try(st_read("foo")) # cannot open datasource
try(st_read("x.gpkg", "xyz")) # cannot open layer
try(st_write(c("foo", "bar")))
try(st_write(x, c("foo", "bar")))
try(st_write(x, "foo", driver = "foo"))
if (Sys.getenv("USER") == "travis") {
	try(st_write(x, "/x", driver = "ESRI Shapefile"))
}

geom = st_sfc(st_point(0:1), st_multipoint(matrix(1:4,2,2)))
st_write(geom, "geom.gpkg")
st_write(geom, "geom1.gpkg", layer = "foo")

df <- data.frame(
    a = c(0, 1, NA, -Inf, Inf),
    b = c("a", "b", NA, "c", ""),
    c = c(as.Date("2001-01-01"), NA, -99, 0, 1),
    d = c(as.POSIXct("2001-01-01"), NA, -99, 0, 1),
    x = 1:5,
    y = 1:5)

x  = st_as_sf(df, coords = c("x", "y"))

if ("GPKG" %in% st_drivers()$name) {
 st_write(x, "x2.gpkg", quiet = TRUE)
 y = st_read("x2.gpkg", quiet = TRUE)
 print(y)
}

if ("SQLite" %in% st_drivers()$name && require(RSQLite)) {
	db = system.file("sqlite/meuse.sqlite", package = "sf")
	dbcon <- dbConnect(dbDriver("SQLite"), db)
	m = dbReadTable(dbcon, "meuse.sqlite")
	m$GEOMETRY = st_as_sfc(m$GEOMETRY, spatialite = FALSE) # ISO wkb
	print(st_sf(m), n = 3)
	dbDisconnect(dbcon)

	db = system.file("sqlite/nc.sqlite", package = "sf")
	dbcon <- dbConnect(dbDriver("SQLite"), db)
	m = dbReadTable(dbcon, "nc.sqlite")
	m$GEOMETRY = st_as_sfc(m$GEOMETRY, spatialite = FALSE) # ISO wkb
	print(st_sf(m), n = 3)
	dbDisconnect(dbcon)

	db = system.file("sqlite/b.sqlite", package = "sf") # has an INT8 field
	b = st_read(db, quiet = TRUE)
	print(b)
	b = st_read(db, int64_as_string = TRUE, quiet = TRUE)
	print(b)

	if (.Platform$endian == "little") {
	  db = system.file("sqlite/test3.sqlite", package = "sf")
	  dbcon <- dbConnect(dbDriver("SQLite"), db)
	  m = dbReadTable(dbcon, "HighWays")
	  m$Geometry = st_as_sfc(m$Geometry, spatialite = TRUE) # spatialite wkb
	  print(st_sf(m), n = 3)
	  m = dbReadTable(dbcon, "Towns")
	  m$Geometry = st_as_sfc(m$Geometry, spatialite = TRUE) # spatialite wkb
	  print(st_sf(m), n = 3)
	  m = dbReadTable(dbcon, "Regions")
	  m$Geometry = st_as_sfc(m$Geometry, spatialite = TRUE) # spatialite wkb
	  print(st_sf(m), n = 3)
	}
}

csv = system.file("csv/pt.csv", package = "sf")
identical(st_read(quiet = TRUE, csv, options = "AUTODETECT_TYPE=Yes")$Int64[3], NA_real_)
identical(st_read(quiet = TRUE, csv, int64_as_string = TRUE, stringsAsFactors = FALSE,
	options = "AUTODETECT_TYPE=Yes")$Int64[3], NA_character_)
identical(st_read(quiet = TRUE, csv, options = "AUTODETECT_TYPE=Yes")$Int32[3], NA_integer_)

if ("GML" %in% st_drivers()$name) {
  gml = system.file("gml/fmi_test.gml", package = "sf")
  print(st_read(gml, quiet = TRUE), n = 0)
  gml = system.file("gml/20170930_OB_530964_UKSH.xml.gz", package = "sf")
  print(st_read(gml, layer = "Parcely"), n = 0)
  print(st_read(gml, layer = "Parcely", int64_as_string=TRUE), n = 0)
}

# logical:
if ("GPKG" %in% st_drivers()$name) {
	tst = st_read(system.file("gpkg/nc.gpkg", package="sf"), quiet = TRUE) # default layer name
	tst$bool = tst$NWBIR79 > 800 # logical
	tst$bool[1:3] = NA
	st_write(tst, "tst__.gpkg")
	tst2 = st_read("tst__.gpkg")
	print(identical(tst$bool, tst2$bool))
}
