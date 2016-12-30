suppressPackageStartupMessages(library(sf))
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
	"+proj=longlat +datum=WGS84")
dg = st_as_sf(d, wkt = "geom")
print(dg, n = 1)

st_geometry(d) = st_as_sfc(d$geom)
d

x = st_sfc(list(st_point(0:1), st_point(0:1)), crs = 4326)
# don't warn when replacing crs with identical value:
st_sfc(x, crs = 4326)
st_sfc(x, crs = "+proj=longlat +datum=WGS84 +no_defs")
# but do when it changes:
st_sfc(x, crs = 3857)

# rbind:
x = st_sf(a = 1:2, geom = st_sfc(list(st_point(0:1), st_point(0:1)), crs = 4326))
rbind(x, x, x)


p = st_point(0:1)
st_cast(p, "MULTIPOINT")
mp = st_multipoint(rbind(c(0,1), c(2,2)))
st_cast(mp, "POINT")
st_cast(mp, "MULTIPOINT")

# geometry collection to its elements:
st_cast(st_geometrycollection(list(mp)), "POINT")
st_cast(st_geometrycollection(list(mp)), "MULTIPOINT")
st_cast(st_geometrycollection(list(p,mp)), "MULTIPOINT")

mp = st_multipoint(rbind(c(0,1)))
x = st_sfc(p, mp)
st_cast(x, "POINT")

sf = st_sf(a = 3:2, geom = x)
st_cast(sf, "POINT")

library(dplyr)

x %>% st_cast("POINT")

# points:
mp = st_multipoint(rbind(c(0,1))) # single-point multipoint
st_sfc(p,mp) %>% st_cast("POINT")
st_sfc(p,mp) %>% st_cast("MULTIPOINT")

# lines:
pts = rbind(c(0,0), c(1,1), c(2,1))
st_sfc(st_linestring(pts), st_multilinestring(list(pts))) %>% st_cast("LINESTRING")
st_sfc(st_linestring(pts), st_multilinestring(list(pts))) %>% st_cast("MULTILINESTRING")

# polygons:
pts = rbind(c(0,0), c(1,1), c(0,1), c(0,0))
st_sfc(st_polygon(list(pts)), st_multipolygon(list(list(pts)))) %>% st_cast("POLYGON")
st_sfc(st_polygon(list(pts)), st_multipolygon(list(list(pts)))) %>% st_cast("MULTIPOLYGON")


st_sfc(st_geometrycollection(list(p)), st_geometrycollection(list(mp))) %>% st_cast() 
st_sfc(st_geometrycollection(list(p)), st_geometrycollection(list(mp))) %>% 
	st_cast() %>% 
	st_cast("POINT")

p = rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))
pol = st_polygon(list(p))
# plot(pol)
try(plot(st_polygonize(pol))) # --> breaks
try(st_length(st_sfc(st_point(c(0,0))))) # breaks

try(as(st_sfc(st_linestring(matrix(1:9,3))), "Spatial"))

# check conus is present:
x = st_sfc(st_point(c(-90,35)), st_point(c(-80,36)), 
	crs = "+proj=longlat +datum=NAD27")
st_transform(x, 3857)

sf_extSoftVersion()

# Ops.sfc:
ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))))
ls * 2
ls - 2

str(x)
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
str(nc)
