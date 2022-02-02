suppressPackageStartupMessages(library(spatstat.core))
suppressPackageStartupMessages(library(sf))

data(chicago)
st_as_sf(chicago)
# ppp:
g = gorillas
st_as_sf(g)
marks(g) = NULL
st_as_sf(g)

# multipolygon: https://github.com/r-spatial/sf/issues/1161
window = read_sf(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(32119)

win = spatstat.geom::as.owin(window)

set.seed(1331)
pp2a = runifpoint(n = 50, win = win)
print(st_as_sf(pp2a))

# st_sample going the spatstat way
x <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 0)))))
try(pts <- st_sample(x, type = "thomas"))
try(pts <- st_sample(x, kappa = 1, mu = 10, type = "Thomas"))
# points expected
set.seed(1331)
pts <- st_sample(x, kappa = 1, mu = 10, scale = 0.1, type = "Thomas")
#plot(x)
#plot(pts, add = TRUE)
pts

# see https://github.com/r-spatial/sf/issues/1233
# png("/tmp/spa%03d.png")

p1 = st_point(0:1)
p2 = st_point(1:2)
p3 = st_point(c(-1,2))
p = st_sfc(p1, p2, p3)
as.ppp(p)
try(as.ppp(st_set_crs(p, 4326)))

sf = st_sf(geom = p)
try(as.ppp(sf))
sf = st_sf(a = 1:3, geom = p)
as.ppp(sf)
sf = st_sf(a = 1:3, b=3:1, geom = p)
as.ppp(sf) # warns

w = st_as_sfc(st_bbox(st_sfc(p1, p2)))
sf = st_sf(a = 1:3, geom = p)
(p0 = rbind(st_sf(a = 0, geom = w), sf))
try(as.ppp(p0)) # errors: one point outside window

w = st_as_sfc(st_bbox(p))
sf = st_sf(a = 1:3, geom = p)
(p0 = rbind(st_sf(a = 0, geom = w), sf))
as.ppp(p0)

# as.owin.sf, as.owin.sfc_*
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), check_ring_dir = TRUE, quiet = TRUE)
try(as.owin(nc)) # should be projected
nc = st_transform(nc, 32119)
plot(as.owin(nc), col = 'grey')
plot(as.owin(st_geometry(nc)), col = 'grey')

sq = rbind(c(-1,-1), c(1, -1), c(1,1), c(-1,1), c(-1,-1))
pol = st_polygon(list(0.5 * sq, sq[5:1,] * 0.45)) # w hole
plot(as.owin(pol), col = 'grey')
plot(as.owin(st_sfc(pol)), col = 'grey')
mpol = st_multipolygon(list(
	list(sq, sq[5:1,] * 0.9),
	list(sq * 2, sq[5:1,] * 1.8)))
plot(as.owin(mpol), col = 'grey')
plot(as.owin(st_sfc(mpol)), col = 'grey')
plot(as.owin(st_sfc(pol, mpol)), col = 'grey')
plot(as.owin(st_sf(a=1:2, st_sfc(pol, mpol))), col = 'grey')
(o = as.owin(st_sf(a=1:2, st_sfc(pol, mpol))))
st_as_sfc(o)

plot(st_as_sfc(o), col = 'blue', main = 'st_as_sfc(o)')
plot(st_as_sf(o), col = 'blue', main = 'st_as_sf(o)')

data(japanesepines)
st_as_sf(japanesepines) # warns about multiplier
jp = rescale(japanesepines)
st_as_sf(jp) # No warning

data(nztrees)
qNZ <- quadratcount(nztrees, nx=4, ny=3)
ts = as.tess(qNZ)
plot(st_as_sfc(ts))

ls = st_linestring(rbind(c(0,0), c(1,1), c(2,0)))
plot(as.psp(ls))
mls = st_multilinestring(list(rbind(c(0,0), c(1,1), c(2,0)), rbind(c(3,3), c(4,2))))
plot(as.psp(mls))

plot(as.psp(st_sfc(ls)))
plot(as.psp(st_sfc(mls)))
plot(as.psp(st_sfc(ls, mls)))

sf = st_sf(st_cast(st_sfc(ls, mls), "MULTILINESTRING"), marks = 1:2, foo = 2:1)
as.psp(sf) # picks marks itself
as.psp(sf, marks = 5:1)

(x = st_as_sf(as.psp(sf)))
(y = st_as_sfc(as.psp(sf)))
all.equal(st_geometry(x), y)
