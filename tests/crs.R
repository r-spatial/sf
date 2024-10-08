suppressPackageStartupMessages(library(sf))
options(rgdal_show_exportToProj4_warnings = "none")
suppressWarnings(st_crs(-1))
suppressWarnings(st_crs(999999))
inherits(try(st_crs("error"), silent = TRUE), "try-error")
str = "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs"
x <- st_crs(str)
x<- st_crs(3857)
str = st_crs(3857)$proj4string
st_crs(3857)$units
x = st_crs("+proj=longlat +datum=WGS84")
x = st_crs(4326)
x = st_crs("+proj=laea") # no EPSG

x = st_sfc(st_point(0:1))
y = st_crs(x, parameters = TRUE)
st_crs(x) = 4326
y = st_crs(x, parameters = TRUE)

from = st_crs(4326)$proj4string
to = st_crs(3857)$proj4string
ret = sf_project(from, to, rbind(c(0,0), c(1,1)))
round(ret, 7)
# create Inf points: #1227/#1228
suppressWarnings(
  sf_project("+proj=longlat", "+proj=lcc +lat_1=30 +lat_2=60", cbind(c(0,0),c(-80,-90)), keep = TRUE)
)
sf_project(to, from, ret)
suppressWarnings(
  sf_project("+proj=longlat", "+proj=lcc +lat_1=30 +lat_2=60", cbind(c(0,0),c(-80,-90)), keep = TRUE)
)
st_transform(st_sfc(st_point(c(0,0)), st_point(c(1,1)), crs = 4326), 3857)
if (sf_extSoftVersion()["USE_PROJ_H"] == "true" || sf_proj_info("have_datum_files")) {
  "datum files installed"
} else {
  "datum files not installed"
}

# https://github.com/r-spatial/sf/issues/1170
g = st_as_sfc("POLYGON ((-61.66957 10.69214, -61.565 10.75728, -61.37453 10.77654, -61.40721 10.60681, -61.66957 10.69214))")
d = st_as_sf(data.frame(id=1, geometry=g), crs=4326)
st_area(d)
st_area(st_transform(d, 2314))

st_axis_order()
if (compareVersion(sf_extSoftVersion()["GDAL"], "2.5.0") > -1)
  st_axis_order(TRUE)
st_axis_order(FALSE)
