library(sf)
p = st_point(c(1/3,1/6))
st_sfc(p, precision = 1000)
st_as_sfc(st_as_binary(st_sfc(p, precision = 0L)))
st_as_sfc(st_as_binary(st_sfc(p, precision = 1000)))
st_as_sfc(st_as_binary(st_sfc(p, precision = 1000000)))
st_as_sfc(st_as_binary(st_sfc(p, precision = 10L)))
st_as_sfc(st_as_binary(st_sfc(p, precision = -1)))

d = data.frame(a = 1:2)
d$geom = c("POINT(0 0)", "POINT(1 1)")

st_as_sf(d, wkt = "geom")
st_as_sf(d, wkt = 2)
st_as_sf(d, wkt = "geom", remove = FALSE)

st_as_sfc(c("POINT(0 0)", "POINT(1 1)"))
st_as_sfc(c("POINT(0 0)", "POINT(1 1)", "POLYGON((0 0,1 1,0 1,0 0))"))
st_as_sfc(character(0))
st_as_sfc(character(0), 4326)
try(st_as_sfc(c("POINT(0 0)", "POINT(1 1)", "POLYGON(0 0,1 1,0 1,0 0)"))) # causes small memory leak
st_as_sfc(c("POINT(0 0)", "POINT(1 1)", "POLYGON((0 0,1 1,0 1,0 0))"), 
	"+proj=longlat +datum=wgs84")
dg = st_as_sf(d, wkt = "geom")
print(dg, n = 1)

st_geometry(d) = st_as_sfc(d$geom)
d
