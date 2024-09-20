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
(f = st_as_sfc(c("POLYGON FULL", "POLYGON((0 0,1 0,1 1,0 1,0 0))", "POINT(0 1)"), crs = 'OGC:CRS84'))
st_bbox(f[1])
st_bbox(f[3])
st_is_valid(f)
st_make_valid(f)
st_make_valid(f[2:3])
