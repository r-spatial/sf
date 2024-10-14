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

x = st_simplify(nc_tr, dTolerance = 1e4)

x = st_simplify(nc_tr, preserveTopology = TRUE)

if (sf:::CPL_geos_version() >= "3.4.0")
	x = st_triangulate(nc_tr)

mls = st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0),,2,byrow=TRUE)))
x = st_polygonize(mls)

x = st_segmentize(nc_tr, 5e4)

try(x <- st_segmentize(nc_tr, -0.1))

x = st_centroid(nc_tr)
x = st_point_on_surface(nc_tr)

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
#cat(paste("GDAL has", nrow(x), "drivers\n"))

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

st_geometry_type(st_sfc(st_point(1:2), st_linestring(matrix(1:4,2,2))), by_geometry = FALSE)

st_zm(list(st_point(1:3), st_linestring(matrix(1:6,2,3))))

st_zm(list(st_point(1:2), st_linestring(matrix(1:6,3,2))), add = TRUE, "Z")

st_transform(st_sfc(st_point(c(0,0)), crs=4326), st_crs("+proj=geocent"))[[1]]

cbind(st_area(nc_tr[1:5,]), a$AREA)

st_area(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))

st_length(st_linestring(rbind(c(0,0),c(0,1))))

st_length(st_multilinestring(list(rbind(c(0,0),c(0,1)))))

try(st_length(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))))

st_area(st_multilinestring(list(rbind(c(0,0),c(0,1)))))

# adds the (0.5 0.5) node:
st_union(st_multilinestring(list(rbind(c(0,0),c(1,1)), rbind(c(0,1), c(1,0)))))

p1 = st_point(c(7,52))
p2 = st_point(c(-30,20))
sfc = st_sfc(p1, p2)
try(st_buffer(sfc, units::set_units(1000, km))) # error: no crs
sfc = st_sfc(p1, p2, crs = 4326)
try(zzz <- st_buffer(sfc, units::set_units(1000, km))) # error: wrong units
if (version$os == "linux-gnu") { # FIXME: why does this break on windows - degree symbol?
  x = st_buffer(sfc, units::set_units(0.1, rad))      # OK: will convert to arc_degrees
}
x = st_transform(sfc, 3857)
x = st_buffer(x, units::set_units(1000, km)) # success

cr = st_as_sfc("CIRCULARSTRING(0 0,1 0,1 1)")
cr1 = st_sf(a = 1, geometry = cr)
plot(cr)
st_as_grob(cr[[1]])

x = st_as_sfc("MULTISURFACE(CURVEPOLYGON(COMPOUNDCURVE(LINESTRING(-159.399779123 22.226016471, -159.399699153 22.226276431, -159.398736217 22.226118372, -159.398260872 22.226095318, -159.398140792 22.2260564590001, -159.398163058 22.2257268010001, -159.397882642 22.225394244, -159.397397157 22.225057335, -159.397318825 22.2251780230001, -159.396993115 22.225177984, -159.396748242 22.2248808800001, -159.396901679 22.224770398, -159.396876329 22.224673093, -159.399167008 22.224731392, -159.399502204 22.225551382), CIRCULARSTRING(-159.399502204 22.225551382, -159.399622762077 22.2257930044972, -159.399779123 22.226016471))))")
mp <- x[[1]] %>% st_cast("MULTIPOLYGON")

x = st_as_sfc("COMPOUNDCURVE(CIRCULARSTRING(0 0, 1 1, 1 0),(1 0, 0 1))")
ls <- x[[1]] %>% st_cast()
class(ls)

is.na(st_bbox(ls))

mp = st_combine(st_buffer(st_sfc(lapply(1:3, function(x) st_point(c(x,x)))), 0.2 * 1:3))
plot(st_centroid(mp), add = TRUE, col = 'red') # centroid of combined geometry
plot(st_centroid(mp, of_largest_polygon = TRUE), add = TRUE, col = 'blue', pch = 3) # center of largest sub-polygon

x = st_sfc(st_polygon(list(rbind(c(0,0),c(0.5,0),c(0.5,0.5),c(0.5,0),c(1,0),c(1,1),c(0,1),c(0,0)))))
suppressWarnings(st_is_valid(x))
y = st_make_valid(x)
y = st_make_valid(x[[1]])
y = st_make_valid(st_sf(a = 1, geom = x))
st_is_valid(y)
