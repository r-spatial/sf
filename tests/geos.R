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

g = st_make_grid(nc)
x = st_intersection(nc, g)
x = st_intersection(g, nc)

ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
st_linestring(rbind(c(0,0),c(10,0))))

set.seed(13531) # make reproducible

st_line_sample(ls, density = 1, type = "random")

g = st_make_grid(nc, n = c(20,10))

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

mls = st_multilinestring(list(rbind(c(0,0), c(1,1)), rbind(c(2,0), c(1,1))))
st_line_merge(mls)

if (sf_extSoftVersion()["GEOS"] >= "3.5.0") {
 # voronoi:
 set.seed(1)
 x = st_multipoint(matrix(runif(10),,2))
 box = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
 v = st_sfc(st_voronoi(x, st_sfc(box)))
 plot(v, col = 0, border = 1, axes = TRUE)
 plot(box, add = TRUE, col = 0, border = 1) # a larger box is returned, as documented
 plot(x, add = TRUE, col = 'red', cex=2, pch=16)
 plot(st_intersection(st_cast(v), box)) # clip to smaller box
 plot(x, add = TRUE, col = 'red', cex=2, pch=16)

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
sfc = st_sfc(st_point(c(0,0)), st_point(c(3,3)))
grd = st_make_grid(sfc, n = c(3,3))
st_intersects(grd)
st_relate(grd, pattern = "****1****")
st_relate(grd, pattern = "****0****")
st_rook = function(a, b = a, ...) st_relate(a, b, pattern = "F***1****", ...)
st_rook(grd, sparse = FALSE)

try(st_relate(st_point(), st_point(), pattern = "FF*FF****")) # error: use st_disjoint

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

p = st_sfc(st_point(c(0,0)), st_point(c(0,1)), st_point(c(0,2)))
st_distance(p, p)
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

