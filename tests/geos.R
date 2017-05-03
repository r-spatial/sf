suppressPackageStartupMessages(library(sf))
# nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc_checked = st_transform(nc, 32119, check = TRUE)
ncm = st_transform(nc, 32119)

x = st_transform(nc[1:10,], 32119)
st_distance(x)

st_is_valid(nc)

ops = c("intersects", "disjoint", "touches", "crosses", "within", "contains", "overlaps", "equals", 
"covers", "covered_by", "equals_exact")
for (op in ops) {
	x = sf:::st_geos_binop(op, ncm[1:50,], ncm[51:100,], 0, NA_character_, FALSE)
	x = sf:::st_geos_binop(op, ncm[1:50,], ncm[51:100,], 0, NA_character_, TRUE)
}

ops = c("intersects", "disjoint", "touches", "crosses", "within", "contains", "overlaps",
"covers", "covered_by")
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

i = st_intersects(ncm, ncm)
j = sf:::CPL_invert_sparse_incidence(i, 100)
all.equal(i, sf:::CPL_invert_sparse_incidence(j, 100))

# check use of pattern in st_relate:
sfc = st_sfc(st_point(c(0,0)), st_point(c(3,3)))
grd = st_make_grid(sfc, n = c(3,3))
st_intersects(grd)
st_relate(grd, pattern = "****1****")
st_relate(grd, pattern = "****0****")
st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")
st_rook(grd)
