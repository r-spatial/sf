library(sf)
if ("GPKG" %in% st_drivers()$name) {
	tst = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg", crs = 4267)
	tst = st_read(system.file("gpkg/nc.gpkg", package="sf")) # default layer name
}

tst = st_read(system.file("shape/nc.shp", package="sf")) # no layer specified

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

sf:::guess_driver("nc.e00")
sf:::guess_driver("nc.shp")
sf:::guess_driver("nc.gxt")
sf:::guess_driver("nc.gps")
sf:::guess_driver("nc.gtm")
sf:::guess_driver("nc.nc")
sf:::guess_driver("nc.map")
try(sf:::guess_driver("nc.sh"))
try(sf:::guess_driver("nc.osm"))
try(sf:::guess_driver("nc.pbf"))
try(sf:::guess_driver("nc"))
