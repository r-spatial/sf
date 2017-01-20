suppressPackageStartupMessages(library(sf))

nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267,
	agr = c(AREA = "aggregate", PERIMETER = "aggregate", CNTY_ = "identity",
		CNTY_ID = "identity", NAME = "identity", FIPS = "identity", FIPSNO = "identity",
		CRESS_ID = "identity", BIR74 = "aggregate", SID74 = "aggregate", NWBIR74 = "aggregate",
		BIR79 = "aggregate", SID79 = "aggregate", NWBIR79  = "aggregate"), quiet = TRUE)

st_is_valid(nc)

st_is_simple(nc)

nc_tr = st_transform(nc, 3857)

x = st_buffer(nc_tr, 1000)

x = st_boundary(nc)

x = st_convex_hull(nc)

x = st_simplify(nc_tr, 1e4)

if (sf:::CPL_geos_version() >= "3.4.0")
	x = st_triangulate(nc_tr)

mls = st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0),,2,byrow=TRUE)))
x = st_polygonize(mls)

x = st_segmentize(nc_tr, 5e4)

try(x <- st_segmentize(nc_tr, -0.1))

x = st_centroid(nc_tr)

a = nc[1:5,]
b = nc[4:10,]

x <- st_intersection(a[1,] ,b)

u = st_union(b) 

x <- st_intersection(st_geometry(a), st_geometry(u))

x = st_union(a[1,], b)

x = st_union(a, st_union(b))

x = st_difference(a[1,], b)

x = st_difference(a, st_union(b))

x = st_sym_difference(a[1,], b)

x = st_sym_difference(a, st_union(b))

x = st_drivers()
cat(paste("GDAL has", nrow(x), "drivers\n"))

# GEOS ops:

st_relate(a, b)

st_disjoint(a, b)

st_touches(a, b)

st_crosses(a, b)

st_within(a, b)

st_contains(a, b)

st_overlaps(a, b)

st_equals(a, b)

st_covers(a, b)

st_covered_by(a, b)

st_equals_exact(a, b, 0.01)

# st_is_within_distance(a, b, 2)

st_geometry_type(st_sfc(st_point(1:2), st_linestring(matrix(1:4,2,2))))

st_zm(list(st_point(1:3), st_linestring(matrix(1:6,2,3))))

st_zm(list(st_point(1:2), st_linestring(matrix(1:6,3,2))), add = TRUE, "Z")

st_transform(st_sfc(st_point(c(0,0)), crs=4326), st_crs("+proj=geocent"))

cbind(st_area(nc_tr[1:5,]), a$AREA)

st_area(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))

st_length(st_linestring(rbind(c(0,0),c(0,1))))

st_length(st_multilinestring(list(rbind(c(0,0),c(0,1)))))

try(st_length(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))))

st_area(st_multilinestring(list(rbind(c(0,0),c(0,1)))))

# adds the (0.5 0.5) node:
st_union(st_multilinestring(list(rbind(c(0,0),c(1,1)), rbind(c(0,1), c(1,0)))))

