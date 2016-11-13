library(sf)
st_crs(-1)
st_crs(999999)
try(st_crs("error"))
str = "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957388243134,0.343987817378283,-1.87740163998045,4.0725 +units=m +no_defs"
st_crs(str)
st_crs(28992)
str = st_crs(28992)$proj4string
st_crs("+proj=longlat +datum=WGS84")
st_crs(4326)
