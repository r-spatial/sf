## this object was created as follows:
library(sf)
# nc = st_read(system.file("shapes/", package="maptools"), "sids")
# st_crs(nc) = 4267 # "+proj=longlat +ellps=clrk66" or "+proj=longlat +datum=NAD27"
# print(nc, n = 3)
# st_write(nc, "nc.gpkg", "nc.gpkg", driver = "GPKG")

# description of the dataset, see vignette in package spdep:
# https://cran.r-project.org/web/packages/spdep/vignettes/sids.pdf

datasource = { if ("GPKG" %in% st_drivers()$name)
	system.file("gpkg/nc.gpkg", package="sf")
else
	system.file("shape/nc.shp", package="sf")
}

rtg = c(AREA = "lattice", PERIMETER = "lattice", CNTY_ = "entity",
		CNTY_ID = "entity", NAME = "entity", FIPS = "entity", FIPSNO = "entity",
		CRESS_ID = "entity", BIR74 = "lattice", SID74 = "lattice", NWBIR74 = "lattice",
		BIR79 = "lattice", SID79 = "lattice", NWBIR79  = "lattice")

nc = st_read(datasource, relation_to_geometry = rtg)
