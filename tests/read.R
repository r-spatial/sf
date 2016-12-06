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
