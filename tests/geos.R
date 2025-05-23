suppressPackageStartupMessages(library(sf))
# nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc_checked = st_transform(nc, 32119, check = TRUE)
ncm = st_transform(nc, 32119)

x = st_transform(nc[1:10,], 32119)
st_distance(x)

st_is_valid(nc)

st_is_empty(st_sfc(st_point(), st_linestring()))

ops = c("intersects", #"disjoint", 
"touches", "crosses", "within", "contains", "overlaps", "equals", "covers", "covered_by", "equals_exact")
for (op in ops) {
	x = sf:::st_geos_binop(op, ncm[1:50,], ncm[51:100,], 0, NA_character_, FALSE)
	x = sf:::st_geos_binop(op, ncm[1:50,], ncm[51:100,], 0, NA_character_, TRUE)
}

ops = c("intersects", #"disjoint", 
"touches", "crosses", "within", "contains", "overlaps", "covers", "covered_by")
df = data.frame(ops = ops)
df$equal = NA
for (op in ops)
	df[df$ops == op, "equal"] = identical(
		sf:::st_geos_binop(op, ncm[1:50,], ncm[51:100,], 0, NA_character_, TRUE, FALSE),
		sf:::st_geos_binop(op, ncm[1:50,], ncm[51:100,], 0, NA_character_, TRUE,  TRUE)
	)
df

st_contains_properly(ncm[1:3,], ncm[1:3])

st_combine(nc)

st_dimension(st_sfc(st_point(0:1), st_linestring(rbind(c(0,0),c(1,1))), 
	st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))))

ncbb = st_as_sfc(st_bbox(nc))
g = st_make_grid(ncbb)
x = st_intersection(nc, g)
x = st_intersection(g, nc)

ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
st_linestring(rbind(c(0,0),c(10,0))))

suppressWarnings(RNGversion("3.5.3"))
set.seed(13531)

st_line_sample(ls, density = 1, type = "random")

g = st_make_grid(ncbb, n = c(20,10))

a1 = st_interpolate_aw(nc["BIR74"], g, FALSE)
sum(a1$BIR74) / sum(nc$BIR74) # not close to one: property is assumed spatially intensive
a2 = st_interpolate_aw(nc["BIR74"], g, extensive = TRUE)
sum(a2$BIR74) / sum(nc$BIR74)

# missing x:
g = st_make_grid(offset = c(0,0), cellsize = c(1,1), n = c(10,10))
g = st_make_grid(what = "centers")
length(g)
g = st_make_grid(what = "corners")
length(g)

g1 = st_make_grid(ncbb, 0.1, what = "polygons", square = FALSE)
g2 = st_make_grid(ncbb, 0.1, what = "points", square = FALSE)

# st_line_merge:
mls = st_multilinestring(list(rbind(c(0,0), c(1,1)), rbind(c(2,0), c(1,1))))
st_line_merge(mls)

if (isTRUE(try(compareVersion(sf_extSoftVersion()["GEOS"], "3.5.0") > -1, silent = TRUE))) {
 # voronoi:
 set.seed(1)
 m = matrix(runif(10),,2)
 x = st_multipoint(m)
 box = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
 v = st_sfc(st_voronoi(x, st_sfc(box)))
 plot(v, col = 0, border = 1, axes = TRUE)
 plot(box, add = TRUE, col = 0, border = 1) # a larger box is returned, as documented
 plot(x, add = TRUE, col = 'red', cex=2, pch=16)
 plot(st_intersection(st_cast(v), box)) # clip to smaller box
 plot(x, add = TRUE, col = 'red', cex=2, pch=16)
 v0 = st_sfc(st_voronoi(st_sfc(x), st_sfc(box)))
 pal <- c("black", "red", "green", "blue", "orange")
 opar = par(mfrow=c(1,2))
 plot(st_collection_extract(v0, "POLYGON"), col=pal)
 text(m[,1], m[,2], label=1:5, col="white")
 if (isTRUE(try(compareVersion(sf_extSoftVersion()["GEOS"], "3.12.0") > -1, silent = TRUE))) {
  v2 = st_sfc(st_voronoi(st_sfc(x), st_sfc(box), point_order=TRUE))
  plot(st_collection_extract(v2, "POLYGON"), col=pal)
  text(m[,1], m[,2], label=1:5, col="white")
 }
 par(opar)

 v = st_voronoi(x)
 print(class(v))
 v = st_sfc(st_voronoi(st_sfc(x)))
 print(class(v))
 v = st_voronoi(st_sf(a = 1, geom = st_sfc(x)))
 print(class(v))
}

i = st_intersects(ncm, ncm[1:88,])
all.equal(i, t(t(i)))

# check use of pattern in st_relate:
sfc = st_as_sfc(st_bbox(st_sfc(st_point(c(0,0)), st_point(c(3,3)))))
grd = st_make_grid(sfc, n = c(3,3))
st_intersects(grd)
st_relate(grd, pattern = "****1****")
st_relate(grd, pattern = "****0****")
st_rook = function(a, b = a, ...) st_relate(a, b, pattern = "F***1****", ...)
st_rook(grd, sparse = FALSE)

#if (Sys.getenv("USER") %in% c("edzer", "travis")) { # memory leaks:
  try(st_relate(st_point(), st_point(), pattern = "FF*FF****")) # error: use st_disjoint
#}

a = st_is_within_distance(nc[c(1:3,20),], nc[1:3,], 100000, sparse = FALSE)
b = st_is_within_distance(nc[c(1:3,20),], nc[1:3,], units::set_units(100000, m), sparse = FALSE)
all.equal(a, b)
x = st_is_within_distance(nc[1:3,], nc[1:5,], 100000)
y = st_is_within_distance(nc[1:3,], nc[1:5,], units::set_units(100, km))
all.equal(x, y)

nc_3857 = st_transform(nc, 3857)
a = st_is_within_distance(nc_3857[c(1:3,20),], nc_3857[1:3,], 100000, sparse = FALSE)
b = st_is_within_distance(nc_3857[c(1:3,20),], nc_3857[1:3,], units::set_units(100000, m), sparse = FALSE)
all.equal(a, b)
x = st_is_within_distance(nc_3857, nc_3857, 100000)
y = st_is_within_distance(nc_3857, nc_3857, units::set_units(100, km))
all.equal(x, y)

pe = st_sfc(st_point())
p = st_sfc(st_point(c(0,0)), st_point(c(0,1)), st_point(c(0,2)))
st_distance(p, p)
st_distance(p, pe)
st_distance(p, p, by_element = TRUE)
st_crs(p) = 4326
st_distance(p, p[c(2,3,1)], by_element = TRUE)
p = st_transform(p, 3587)
st_distance(p, p[c(2,3,1)], by_element = TRUE)

# from https://github.com/r-spatial/sf/issues/458 :
pts <- st_sfc(st_point(c(.5,.5)), st_point(c(1.5, 1.5)), st_point(c(2.5, 2.5)))
pol <- st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
pol_df <- data.frame(id = 1) 
st_geometry(pol_df) <- st_sfc(pol)
st_intersects(pts, pol_df[pol_df$id == 2,]) # with empty sf/sfc
st_intersects(pts, pol_df[pol_df$id == 2,], sparse = FALSE) # with empty sf/sfc

# st_node
l = st_linestring(rbind(c(0,0), c(1,1), c(0,1), c(1,0), c(0,0)))
st_node(l)
st_node(st_sfc(l))
st_node(st_sf(a = 1, st_sfc(l)))

# print.sgbp:
(lst = st_disjoint(nc, nc))
# dim.sgbp:
dim(lst)
# as.matrix.sgbp:
as.matrix(lst)[1:5, 1:5]
# negate:
!lst
# as.data.frame:
head(as.data.frame(lst), 10)

# snap:
nc1 = st_transform(nc, 32119)
g = st_make_grid(nc1, c(5000,5000), what = "centers")
s = st_snap(nc1[1:3,], g, 2501*sqrt(2))
sfg = st_snap(st_geometry(nc1)[[1]], g, 2501*sqrt(2))
sfg = st_snap(st_geometry(nc1)[[1]], st_combine(g), 2501*sqrt(2))

# Hausdorff distance: http://geos.refractions.net/ro/doxygen_docs/html/classgeos_1_1algorithm_1_1distance_1_1DiscreteHausdorffDistance.html
A = st_as_sfc("LINESTRING (0 0, 100 0, 10 100, 10 100)")
B = st_as_sfc("LINESTRING (0 100, 0 10, 80 10)")
st_distance(c(A,B))
st_distance(c(A,B), which = "Hausdorff")
st_distance(c(A,B), which = "Hausdorff", par = 0.001)
LE = st_as_sfc("LINESTRING EMPTY")
st_distance(c(A, LE), which = "Hausdorff", par = 0.001)

# one-argument st_intersection and st_difference:
set.seed(131)
m = rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))
p = st_polygon(list(m))
n = 100
l = vector("list", n)
for (i in 1:n)
   l[[i]] = p + 10 * runif(2)
s = st_sfc(l)
plot(s, col = sf.colors(categorical = TRUE, alpha = .5))
d = st_difference(s) # sequential differences: s1, s2-s1, s3-s2-s1, ...
plot(d, col = sf.colors(categorical = TRUE, alpha = .5))
i = st_intersection(s) # all intersections
plot(i, col = sf.colors(categorical = TRUE, alpha = .5))
summary(lengths(st_overlaps(s, s)))
summary(lengths(st_overlaps(d, d)))
summary(lengths(st_overlaps(i, i)))

sf = st_sf(s)
i = st_intersection(sf) # all intersections
plot(i["n.overlaps"])
summary(i$n.overlaps - lengths(i$origins))

# st_nearest_points:
pt1 = st_point(c(.1,.1))
pt2 = st_point(c(.9,.9))
b1 = st_buffer(pt1, 0.1)
b2 = st_buffer(pt2, 0.1)
plot(b1, xlim = c(0,1), ylim = c(0,1))
plot(b2, add = TRUE)
(ls0 = try(st_nearest_points(b1, b2))) # sfg
(ls = try(st_nearest_points(st_sfc(b1), st_sfc(b2)))) # sfc
(ls = try(st_nearest_points(st_sfc(b1), st_sfc(b2), pairwise = TRUE))) # sfc
identical(ls0, ls)
# plot(ls, add = TRUE, col = 'red')

nc = read_sf(system.file("gpkg/nc.gpkg", package="sf"))
plot(st_geometry(nc))
ls = try(st_nearest_points(nc[1,], nc))
# plot(ls, col = 'red', add = TRUE)
pts = st_cast(ls, "POINT") # gives all start & end points
# starting, "from" points, corresponding to x:
plot(pts[seq(1, 200, 2)], add = TRUE, col = 'blue')
# ending, "to" points, corresponding to y:
plot(pts[seq(2, 200, 2)], add = TRUE, col = 'red')

# points to nearest features
ls1 = st_linestring(rbind(c(0,0), c(1,0)))
ls2 = st_linestring(rbind(c(0,0.1), c(1,0.1)))
ls3 = st_linestring(rbind(c(0,1), c(1,1)))
(l = st_sfc(ls1, ls2, ls3))

p1 = st_point(c(0.1, -0.1))
p2 = st_point(c(0.1, 0.11))
p3 = st_point(c(0.1, 0.09))
p4 = st_point(c(0.1, 0.9))
p5 = st_point()

(p = st_sfc(p1, p2, p3, p4, p5))
#st_nearest_points(p, l)
n = try(st_nearest_feature(p,l))
if (!inherits(n, "try-error")) {
  print(st_nearest_points(p, l[n], pairwise = TRUE))
  print(st_nearest_feature(p, l))
  print(st_nearest_feature(p, st_sfc()))
  print(st_nearest_feature(st_sfc(), l))
}

# can do centroid of empty geom:
st_centroid(st_polygon())

#999:
pt = data.frame(x=1:2, y=1:2,a=letters[1:2])
pt = st_as_sf(pt, coords=c("x","y"))

bf =st_buffer(pt, dist=0.3)

st_within(pt,bf, sparse=FALSE)
st_within(pt[1,], bf[1,], sparse = FALSE)
st_relate(pt[1,], bf[1,], pattern = "T*F**F***", sparse = FALSE)

sf:::is_symmetric(pattern = "010121010")
sf:::is_symmetric(pattern = "010121021")

st_intersects(st_point(0:1), st_point(2:3)) # sfg method

if (isTRUE(try(compareVersion(sf_extSoftVersion()["GEOS"], "3.7.0") > -1, silent = TRUE))) {
	ls = st_linestring(rbind(c(1,1), c(2,2), c(3,3)))
	print(st_reverse(ls))
	print(st_reverse(st_sfc(ls)))
	print(st_reverse(st_sf(a = 2, geom = st_sfc(ls))))
}

p = st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
y = st_sfc(p)
x = st_sfc(p + 1.001)

x %>% st_set_precision(0) %>% st_intersects(y)
x %>% st_set_precision(10000) %>% st_intersects(y)
x %>% st_set_precision(1000) %>% st_intersects(y)
x %>% st_set_precision(501) %>% st_intersects(y) # no
x %>% st_set_precision(500) %>% st_intersects(y) # yes
x %>% st_set_precision(100) %>% st_intersects(y)
x %>% st_set_precision(10) %>% st_intersects(y)

p1 = st_point(0:1)
p2 = st_point(2:1)
p = st_sf(a = letters[1:8], geom = st_sfc(p1, p1, p2, p1, p1, p2, p2, p1))
st_equals(p)
st_equals(p, remove_self = TRUE)
(u = st_equals(p, retain_unique = TRUE))
# retain the records with unique geometries:
p[-unlist(u),]
