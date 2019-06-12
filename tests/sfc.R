suppressPackageStartupMessages(library(sf))
library(testthat)

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
head(st_as_sf(d, wkt = "geom"), 1)

d$geom = st_as_sfc(d$geom)
d1 = d
attr(d1, "sf_col") = "geom"
st_geometry(d1) = d$geom

d$geometry = d$geom # second geometry list-column
expect_warning(st_geometry(d) <- d$geom)
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
ls / ls
p_ = st_point(0:1)
ll = st_sfc(ls[[1]], p_)
ll & st_sfc(p_)
ll | st_sfc(p_)
ll %/% st_sfc(p_)
ll == st_sfc(p_)
ll != st_sfc(p_)


str(x)
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
str(nc)
st_as_sfc(st_bbox(nc))

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
st_bbox(c(xmin = 16.1, xmax = 16.6, ymin = 48.6, ymax = 47.9), crs = st_crs(4326))
st_bbox(c(xmin = 16.1, xmax = 16.6, ymin = 48.6, ymax = 47.9), crs = 4326)

bb$xrange
bb$yrange
bb$xmin
bb$ymin
bb$xmax
bb$ymax
try(bb$foo)

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
left_join(a, data.frame(b, geometry = 1), by = "b")

# st_joins:
a = st_sf(a = 1:3,
 geom = st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3))))
b = st_sf(a = 11:14,
 geom = st_sfc(st_point(c(10,10)), st_point(c(2,2)), st_point(c(2,2)), st_point(c(3,3))))
st_join(a, b)
st_join(a, b, left = FALSE)
# deprecated:
try(x <- st_join(a, b, FUN = mean))
# st_join, largest = TRUE:
nc <- st_transform(st_read(system.file("shape/nc.shp", package="sf")), 2264)
gr = st_sf(
    label = apply(expand.grid(1:10, LETTERS[10:1])[,2:1], 1, paste0, collapse = " "),
    geom = st_make_grid(nc))
gr$col = sf.colors(10, categorical = TRUE, alpha = .3)
# cut, to check, NA's work out:
gr = gr[-(1:30),]
st_join(nc, gr, largest = TRUE)

# rbind:
x = st_sf(a = 1:2, geom = st_sfc(list(st_point(0:1), st_point(0:1)), crs = 4326))
rbind(x, x, x)
nc2 = rbind(nc[1:50, ], nc[51:100, ])
all.equal(nc, nc2)

# st_sample:
suppressWarnings(RNGversion("3.5.3"))
set.seed(131)
options(digits=6)
x = st_sfc(st_polygon(list(rbind(c(0,1),c(90,1),c(90,90),c(0,90),c(0,1)))), crs = st_crs(4326))
#if (sf_extSoftVersion()["proj.4"] >= "4.9.0")
  (p <- st_sample(x, 10))
x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,90),c(0,90),c(0,0))))) # NOT long/lat:
p <- st_sample(x, 10)
x = st_sfc(st_polygon(list(rbind(c(-180,-90),c(180,-90),c(180,90),c(-180,90),c(-180,-90)))),
 crs=st_crs(4326))
if (sf_extSoftVersion()["proj.4"] >= "4.9.0") # lwgeom breaks on this
  (p <- st_sample(x, 10))
pt = st_multipoint(matrix(1:20,,2))
st_sample(p, 3)
ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
 st_linestring(rbind(c(0,0),c(.1,0))),
 st_linestring(rbind(c(0,1),c(.1,1))),
 st_linestring(rbind(c(2,2),c(2,2.00001))))
st_sample(ls, 80)
st_sample(nc[1:2,], size = c(10,20))
# try with LINES, LongLat, should generate a warning:
nc[1:2,] %>% st_transform(4326) %>% st_cast("MULTILINESTRING") %>% st_sample(size = c(10,20))
st_sample(ls, 80, type = "regular")
p_sample = lapply(1:10, function(i) st_sample(nc[i, ], 100, exact = FALSE))
lengths(p_sample)
p_sample_exact = lapply(1:10, function(i) st_sample(nc[i, ], 100, exact = TRUE))
lengths(p_sample_exact)
#plot(nc$geometry[1])
#plot(p_sample[[1]], add = TRUE)
#plot(p_sample_exact[[1]], add = TRUE)

#class(st_bind_cols(nc, as.data.frame(nc)[1:3]))
class(dplyr::bind_cols(nc, as.data.frame(nc)[1:3]))
class(rbind(nc, nc))
class(cbind(nc, nc))

x = st_sfc(st_point(0:1), st_point(2:3))
x[c(NA,1,NA,2,NA)]

# jitter
pts = st_centroid(st_geometry(nc))
plot(pts)
plot(st_jitter(pts, .05), add = TRUE, col = 'red')
plot(st_geometry(nc))
plot(st_jitter(st_geometry(nc), factor = .01), add = TRUE, col = '#ff8888')
st_jitter(st_sfc(st_point(0:1)), amount = .1)

# st_bbox:
library(sp)
demo(meuse, ask = FALSE, echo = FALSE)
st_bbox(meuse)
st_crs(meuse)
library(raster)
st_bbox(raster(meuse.grid))
st_bbox(extent(raster()))

# st_to_s2
if (FALSE) { # stops working with GDAL 2.3.0 / PROJ 5.0.1:
 x = sf:::st_to_s2(nc)
 x1 = st_geometry(x)
 cc = st_coordinates(x1)
 summary(sqrt(cc[,1]^2+cc[,2]^2+cc[,3]^2))
}

# check_ring_dir
m = rbind(c(0,0), c(0,1), c(1,1), c(1,0), c(0,0))
mi = m[nrow(m):1,]
pol = st_polygon(list(m * 10, m + .5, mi + 1.5, mi + 3.5, m + 5, mi + 6.5))
st_sfc(pol)
x = st_sfc(pol, check_ring_dir=TRUE)
str(x)
x = st_sfc(st_polygon(), st_polygon(), check_ring_dir=TRUE)
str(x)
# empty ring/zero area:
x = st_sfc(st_polygon(list(m[c(1,3,1),])), check_ring_dir=TRUE)

mp = st_multipolygon(list(pol, pol))
try(x <- st_sfc(mp, st_polygon(), check_ring_dir=TRUE))
x <- st_sfc(mp, pol) %>% st_cast("MULTIPOLYGON") %>% st_sfc(check_ring_dir=TRUE)
x
str(x)

x = st_sfc(st_linestring(rbind(c(-179,0),c(179,0))), crs = 4326)
st_wrap_dateline(st_sf(a = 1, geometry = x))
st_wrap_dateline(x)
st_wrap_dateline(x[[1]])

geo <- c("{\"geodesic\":true,\"type\":\"Point\",\"coordinates\":[-118.68152563269095,36.43764870908927]}",
         "{\"geodesic\":true,\"type\":\"Point\",\"coordinates\":[-118.67408758213843,36.43366018922779]}",
         "{\"geodesic\":true,\"type\":\"Point\",\"coordinates\":[-118.67708346361097,36.44208638659282]}",
         "{\"geodesic\":true,\"type\":\"Point\",\"coordinates\":[-118.67886661944996,36.44110273135671]}",
         "{\"geodesic\":true,\"type\":\"Point\",\"coordinates\":[-118.68089232041565,36.44173155205561]}")
st_as_sfc(geo, GeoJSON = TRUE)
st_as_sfc(geo, GeoJSON = TRUE, crs = 4326)

st_as_sfc(st_as_binary(st_sfc(st_point(0:1)))[[1]], crs = 4326)

x = nc
x$geom = NULL
class(x)

st_as_sfc(list(st_point(0:1)), crs = 4326)

# crop:
box = c(xmin = 0, ymin = 0, xmax = 1, ymax = 1)

pol = st_sfc(st_buffer(st_point(c(.5, .5)), .65))
pol_sf = st_sf(a=1, geom=pol)

st_crop(pol, box)
st_crop(pol, st_bbox(box))
st_crop(pol_sf, box)
st_crop(pol_sf, st_bbox(box))

# new sample methods:
x = st_sfc(st_polygon(list(rbind(c(0,0),c(90,0),c(90,90),c(0,90),c(0,0))))) # NOT long/lat:
p <- st_sample(x, 10, type = "regular")
p <- st_sample(x, 10, type = "hexagonal")

all.equal(st_drop_geometry(pol_sf), st_set_geometry(pol_sf, NULL))

# https://github.com/r-spatial/sf/issues/1024
shape1 <-st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0)))))
shape2 <- st_sfc(st_polygon())
shape3 <- st_sfc(st_polygon())

shape4 = st_intersection(shape2, shape3) # has zero features

st_difference(shape1, shape4)
st_difference(shape4, shape1)
st_sym_difference(shape1, shape4)
st_union(shape1, shape4)
st_union(shape4, shape1)

# transform empty:
st_sf(geom=st_sfc()) %>% st_set_crs(3587) %>% st_transform(4326)

