## this object was created as follows:
# library(sf)
# nc = st_read(system.file("shapes/", package="maptools"), "sids")
# st_crs(nc) = 4267 # "+proj=longlat +ellps=clrk66" or "+proj=longlat +datum=NAD27"
# print(nc, n = 3)
# st_write(nc, "nc.gpkg", "nc.gpkg", driver = "GPKG")

nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg")
suppressWarnings(st_crs(nc) <- 4267)
