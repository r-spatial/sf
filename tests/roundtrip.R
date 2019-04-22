#library(rgdal2)
#openOGRLayer("PG:dbname=postgis" , "meuse2")

suppressPackageStartupMessages(library(sf))
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pol1 = list(outer, hole1, hole2)
pol2 = list(outer + 12, hole1 + 12)
pol3 = list(outer + 24)
mp = list(pol1,pol2,pol3)
mp1 = st_multipolygon(mp)
sf = st_sf(a=1, st_sfc(mp1))
a = as(sf, "Spatial")
class(a)
b = st_as_sf(a)
a2 = as(a, "SpatialPolygonsDataFrame")
all.equal(a, a2) # round-trip

b1 = as(a, "sf")
all.equal(b, b1)
b = st_as_sfc(a)
b1 = as(a, "sfc")
all.equal(b, b1)

# SpatialMultiPoints
library(sp)
suppressWarnings(RNGversion("3.5.3"))
set.seed(1331)
example(SpatialMultiPoints, ask = FALSE, echo = FALSE) # loads mpdf
m = st_as_sf(mpdf)
all.equal(as(m, "Spatial"), mpdf) # TRUE

demo(meuse, ask = FALSE, echo = FALSE)
meuse = spTransform(meuse, CRS("+init=epsg:4326"))
pol.grd = as(meuse.grid, "SpatialPolygonsDataFrame")
meuse.grd = spTransform(meuse.grid, CRS("+init=epsg:4326"))
pol.grd = spTransform(pol.grd, CRS("+init=epsg:4326"))
meuse.area = spTransform(meuse.area, CRS("+init=epsg:4326"))
meuse.riv = spTransform(meuse.riv, CRS("+init=epsg:4326"))
summary(st_as_sf(meuse))
summary(st_as_sf(meuse.grd))
x <- st_as_sf(meuse.grid) # don't print: CRS variations.
summary(st_as_sf(meuse.area))
summary(st_as_sf(meuse.riv))
summary(st_as_sf(as(meuse.riv, "SpatialLines")))
summary(st_as_sf(pol.grd))
summary(st_as_sf(as(pol.grd, "SpatialLinesDataFrame")))

# roundtrip nc: sf -> sp -> sf
# nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg")
nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", quiet = TRUE)
p4s = "+proj=longlat +datum=NAD27 +no_defs +ellps=clrk66 +nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat"
suppressWarnings(st_crs(nc) <- p4s)
names(nc)[15] = "geometry"
attr(nc, "sf_column") = "geometry"
attr(nc$geometry, "crs")$epsg = NA_integer_
all.equal(nc, st_as_sf(as(nc, "Spatial")))

sp = as(nc, "Spatial")
comment(sp) = "FALSE"
all.equal(nc, st_as_sf(sp))
detach("package:sp")
unloadNamespace("rgeos")
