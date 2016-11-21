options(warn = 2)
library(sf)

nc = st_read(system.file("shape/nc.shp", package="sf"), "nc", crs = 4267,
	relation_to_geometry = c(AREA = "lattice", PERIMETER = "lattice", CNTY_ = "entity",
		CNTY_ID = "entity", NAME = "entity", FIPS = "entity", FIPSNO = "entity",
		CRESS_ID = "entity", BIR74 = "lattice", SID74 = "lattice", NWBIR74 = "lattice",
		BIR79 = "lattice", SID79 = "lattice", NWBIR79  = "lattice"))

st_is_valid(nc)

st_is_simple(nc)

x = st_buffer(nc, 1)

x = st_boundary(nc)

x = st_convex_hull(nc)

x = st_union_cascaded(nc)

x = st_simplify(nc, 0.1)

if (sf:::CPL_geos_version() >= "3.4.0" && sf:::CPL_gdal_version() >= "2.1.0") {
	x = st_triangulate(nc)
}

mls = st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0),,2,byrow=TRUE)))
x = st_polygonize(mls)

x = st_segmentize(nc, 0.1)

try(x <- st_segmentize(nc, -0.1))

x = st_centroid(nc)

a = nc[1:5,]
b = nc[4:10,]

x = st_intersection(a[1,] ,b)

x <- st_intersection(a, st_union(b)) # FIXME

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

st_drop_zm(list(st_point(1:3), st_linestring(matrix(1:6,2,3))))

cbind(suppressWarnings(st_area(a)), a$AREA)

st_area(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))

st_length(st_linestring(rbind(c(0,0),c(0,1))))

st_length(st_multilinestring(list(rbind(c(0,0),c(0,1)))))

st_length(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))

st_area(st_multilinestring(list(rbind(c(0,0),c(0,1)))))

# adds the (0.5 0.5) node:
st_union(st_multilinestring(list(rbind(c(0,0),c(1,1)), rbind(c(0,1), c(1,0)))))

