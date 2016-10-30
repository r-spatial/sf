## this object was created as follows:
library(sf)
# nc = st_read(system.file("shapes/", package="maptools"), "sids")
# st_crs(nc) = 4267 # "+proj=longlat +ellps=clrk66" or "+proj=longlat +datum=NAD27"
# print(nc, n = 3)
# st_write(nc, "nc.gpkg", "nc.gpkg", driver = "GPKG")

datasource = { if ("GPKG" %in% st_drivers()$name)
	system.file("gpkg/nc.gpkg", package="sf")
else
	system.file("shapes/nc.shp", package="sf")
}

nc = st_read(datasource, crs = 4267,
	relation_to_geometry = c(AREA = "lattice", PERIMETER = "lattice", CNTY_ = "entity",
		CNTY_ID = "entity", NAME = "entity", FIPS = "entity", FIPSNO = "entity",
		CRESS_ID = "entity", BIR74 = "lattice", SID74 = "lattice", NWBIR74 = "lattice",
		BIR79 = "lattice", SID79 = "lattice", NWBIR79  = "lattice"))
rm(datasource)
