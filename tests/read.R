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
    print(st_read(system.file("gpkg/tl.gpkg", package="sf"), , quiet = TRUE, int64_as_string = TRUE)$AWATER)
}

# see https://github.com/edzer/sfr/issues/45 :
if ("OSM" %in% st_drivers()$name && Sys.info()['sysname'] != "Darwin") {
	osm = system.file("osm/overpass.osm", package="sf")
	print(st_layers(osm))
	suppressWarnings(print(st_layers(osm, do_count = TRUE)))
	suppressWarnings(print(st_read(osm, "multipolygons", quiet = TRUE)))
}

# layer opening option:
st_read(system.file("shape/nc.shp", package="sf"),  quiet = TRUE,
	options = c("ADJUST_TYPE=YES", "ENCODING=CPL_ENC_UTF8"))

if ("GPKG" %in% st_drivers()$name) { # shapefiles can't write point+multipoint mix:
  x <- st_sf(a = 1:2, geom = st_sfc(st_point(0:1), st_multipoint(matrix(1:4,2,2))))
  st_write(x, "x.gpkg")
  write_sf(x, "x.gpkg")
  x <- st_read("x.gpkg", quiet = TRUE)
  print(x)
}
x <- st_sf(a = 1:2, geom = st_sfc(st_linestring(matrix(1:4,2,2)), 
	st_multilinestring(list(matrix(1:4,2,2), matrix(10:13,2,2)))))
st_write(x, "x.shp")
x <- st_read("x.shp", quiet = TRUE)
x
x <- read_sf("x.shp", quiet = TRUE)

try(st_layers("foo")) # cannot open datasource
try(st_read("foo")) # cannot open datasource
try(st_read("x.gpkg", "foo")) # cannot open layer
try(st_write(c("foo", "bar")))
try(st_write(x, c("foo", "bar")))
try(st_write(x, "foo", driver = "foo"))
if (Sys.getenv("USER") %in% c("travis")) {
	try(st_write(x, "/x", driver = "ESRI Shapefile"))
}

library(sf)
#> Linking to GEOS 3.5.0, GDAL 2.1.1, proj.4 4.9.3

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

try(st_write(x, "x.shp", driver = character(0))) # err
