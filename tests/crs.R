suppressPackageStartupMessages(library(sf))
st_crs(-1)
st_crs(999999)
try(st_crs("error"))
str = "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs"
st_crs(str)
st_crs(3857)
str = st_crs(3857)$proj4string
st_crs(3857)$units
st_crs("+proj=longlat +datum=WGS84")
st_crs(4326)
st_crs("+proj=laea") # no EPSG

x = st_sfc(st_point(0:1))
st_crs(x, parameters = TRUE)
st_crs(x) = 4326
st_crs(x, parameters = TRUE)

from = st_crs(4326)$proj4string
to = st_crs(3857)$proj4string
(ret = sf_project(from, to, rbind(c(0,0), c(1,1))))
sf_project(to, from, ret)
st_transform(st_sfc(st_point(c(0,0)), st_point(c(1,1)), crs = 4326), 3857)
try(sf_project("+proj=longlat", "+proj=bar", matrix(1:4,2)))
try(sf_project("+proj=foo", "+proj=longlat", matrix(1:4,2)))

if (st_proj_info("have_datum_files")) {
  "datum files installed"
} else {
  "datum files not installed"
}

