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

suppressPackageStartupMessages( library(dplyr) )

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
st_length(st_sfc(st_point(c(0,0))))

try(as(st_sfc(st_linestring(matrix(1:9,3))), "Spatial"))

# check conus is present:
x = st_sfc(st_point(c(-90,35)), st_point(c(-80,36)), 
	crs = "+proj=longlat +datum=NAD27")
st_transform(x, 3857)

sf_extSoftVersion()[1:3]

# Ops.sfc:
ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))))
ls * 2
ls - 2
(ls + 2) %% 3

str(x)
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
str(nc)

st_agr("constant")
st_agr()
x <- st_sf(a = 1:2, b = 3:4, geom = x, agr = c("constant", "aggregate"))
suppressPackageStartupMessages(library(dplyr))
y <- x %>% st_set_agr("constant")
y

sf1 <- st_sf(a = c("x", "y"), geom = st_sfc(st_point(3:4), st_point(3:4)))
sf1[names(sf1)]

st_bbox(sf1)
bb = st_bbox(nc)
bb
st_crs(bb)

# merge:
a = data.frame(a = 1:3, b = 5:7)
st_geometry(a) = st_sfc(st_point(c(0,0)), st_point(c(1,1)), st_point(c(2,2)))
b = data.frame(x = c("a", "b", "c"), b = c(2,5,6))
merge(a, b)
merge(a, b, all = TRUE)

# joins:
inner_join(a, b)
left_join(a, b)
right_join(a, b)
full_join(a, b)
semi_join(a, b)
anti_join(a, b)

# st_joins:
a = st_sf(a = 1:3,
 geom = st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3))))
b = st_sf(a = 11:14,
 geom = st_sfc(st_point(c(10,10)), st_point(c(2,2)), st_point(c(2,2)), st_point(c(3,3))))
st_join(a, b)
st_join(a, b, left = FALSE)
# deprecated:
try(x <- st_join(a, b, FUN = mean))

# rbind:
x = st_sf(a = 1:2, geom = st_sfc(list(st_point(0:1), st_point(0:1)), crs = 4326))
rbind(x, x, x)
nc2 = rbind(nc[1:50, ], nc[51:100, ])
all.equal(nc, nc2)

# st_sample:
set.seed(131)
options(digits=6)
x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,90),c(0,90),c(0,0)))), crs = st_crs(4326))
(p <- st_sample(x, 10))
x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,90),c(0,90),c(0,0))))) # NOT long/lat:
st_sample(x, 10)
x = st_sfc(st_polygon(list(rbind(c(-180,-90),c(180,-90),c(180,90),c(-180,90),c(-180,-90)))),
 crs=st_crs(4326))
p <- st_sample(x, 10)
pt = st_multipoint(matrix(1:20,,2))
st_sample(p, 3)
ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
 st_linestring(rbind(c(0,0),c(.1,0))),
 st_linestring(rbind(c(0,1),c(.1,1))),
 st_linestring(rbind(c(2,2),c(2,2.00001))))
st_sample(ls, 80)
st_sample(nc[1:2,], size = c(10,20))

#class(st_bind_cols(nc, as.data.frame(nc)[1:3]))
class(dplyr::bind_cols(nc, as.data.frame(nc)[1:3]))
class(rbind(nc, nc))
class(cbind(nc, nc))
