library(sf)
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg", crs = 4267,
	relation_to_geometry = c(AREA = "lattice", PERIMETER = "lattice", CNTY_ = "entity",
		CNTY_ID = "entity", NAME = "entity", FIPS = "entity", FIPSNO = "entity",
		CRESS_ID = "entity", BIR74 = "lattice", SID74 = "lattice", NWBIR74 = "lattice",
		BIR79 = "lattice", SID79 = "lattice", NWBIR79  = "lattice"))

st_is_valid(nc)

st_is_simple(nc)

x = st_buffer(nc, 1)

x = st_boundary(nc)

x = st_convexhull(nc)

x = st_unioncascaded(nc)

x = st_simplify(nc, 0.1)

# x = st_triangulate(nc)

mls = st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0),,2,byrow=TRUE)))
x = st_polygonize(mls)

x = st_segmentize(nc, 0.1)

try(x <- st_segmentize(nc, -0.1))

x = st_centroid(nc)

a = nc[1:5,]
b = nc[4:10,]

x = st_intersection(a[1,] ,b)

try(x <- st_intersection(a ,b)) # FIXME

x = st_union(a[1,], b)

x = st_difference(a[1,], b)

x = st_sym_difference(a[1,], b)

x = st_drivers()
cat(paste("GDAL has", nrow(x), "drivers\n"))
