suppressPackageStartupMessages(library(sf))

# create full polygon:
(f = st_as_sfc("POLYGON FULL"))
g = st_sfc(st_polygon(list(matrix(c(0,-90,0,-90), 2, byrow = TRUE))))
identical(f, g)
old = sf_use_s2(FALSE)
try(st_as_sfc("POLYGON FULL")) # errors
sf_use_s2(old)
(f = st_as_sfc(c("POLYGON FULL", "POLYGON((0 0,1 0,1 1,0 1,0 0))")))
st_is_full(f)
st_bbox(f[1])
st_bbox(f[2])
st_is_valid(f) # full polygon NA: right, we don't know the CRS
st_crs(f) = 'OGC:CRS84' # geodetic:
st_is_valid(f)
st_crs(f) = NA
try(st_make_valid(f))
st_crs(f) = 'OGC:CRS84' # geodetic:
st_make_valid(f)
# mixed geometries:
(f = st_as_sfc(c("POLYGON FULL", "POLYGON((0 0,1 0,1 1,0 1,0 0))", "POINT(3 1)"), crs = 'OGC:CRS84'))
st_bbox(f[1])
st_bbox(f[3])
st_is_valid(f)
st_make_valid(f)
st_make_valid(f[2:3])

# roundtrip:
sf = st_as_sf(data.frame(attr = 1:3, geom = f[1:3]))
write_sf(sf, fn <- tempfile(fileext=".gpkg"))
g = read_sf(fn)
g

st_is_empty(g)
st_is_full(g)
st_is_valid(g)
st_is_simple(g)
st_dimension(g)
st_area(g)
st_length(g)
st_distance(g)
