suppressPackageStartupMessages(library(sf))

# create empty geometries:
st_point(rep(NA_real_,2))
st_point(rep(NA_real_,3), dim = "XYZ")
st_point(rep(NA_real_,3), dim = "XYM")
st_point(rep(NA_real_,4), dim = "XYZM")

st_multipoint()
st_multipoint(matrix(numeric(0), 0, 3), dim = "XYZ")
st_multipoint(matrix(numeric(0), 0, 3), dim = "XYM")
st_multipoint(matrix(numeric(0), 0, 4), dim = "XYZM")

st_linestring(matrix(numeric(0), 0, 2), "XY")
st_linestring(matrix(numeric(0), 0, 3), "XYZ")
st_linestring(matrix(numeric(0), 0, 3), "XYM")
st_linestring(matrix(numeric(0), 0, 4), "XYZM")

st_multilinestring(list(), "XY")
st_multilinestring(list(), "XYZ")
st_multilinestring(list(), "XYM")
st_multilinestring(list(), "XYZM")

st_polygon(list(), "XY")
st_polygon(list(), "XYZ")
st_polygon(list(), "XYM")
st_polygon(list(), "XYZM")

st_multipolygon(list(), "XY")
st_multipolygon(list(), "XYZ")
st_multipolygon(list(), "XYM")
st_multipolygon(list(), "XYZM")

st_geometrycollection()
st_geometrycollection(dim = "XYZ")
st_geometrycollection(dim = "XYM")
st_geometrycollection(dim = "XYZM")

st_point(rep(NA_real_,2))
st_multipoint()
st_linestring(matrix(numeric(0), 0, 2))
st_multilinestring(list(), "XY")
st_polygon(list(), "XY")
st_multipolygon(list(), "XY")
st_geometrycollection(, "XY")

(e1 = st_sfc(
st_point(rep(NA_real_,2)),
st_multipoint(),
st_linestring(matrix(numeric(0), 0, 2)),
st_multilinestring(list(), "XY"),
st_polygon(list(), "XY"),
st_multipolygon(list(), "XY"),
st_geometrycollection(, "XY")))

(e2 = st_sfc(st_point(rep(NA_real_,3), "XYZ"),
st_multipoint(matrix(numeric(0),0,3), dim = "XYZ"),
st_linestring(matrix(numeric(0), 0, 3)),
st_multilinestring(list(), "XYZ"),
st_polygon(list(), "XYZ"),
st_multipolygon(list(), "XYZ"),
st_geometrycollection(dim = "XYZ")))

(e3 = st_sfc(st_point(rep(NA_real_,3), "XYM"),
st_multipoint(matrix(numeric(0),0,3), dim = "XYM"),
st_linestring(matrix(numeric(0), 0, 3), "XYM"),
st_multilinestring(list(), "XYM"),
st_polygon(list(), "XYM"),
st_multipolygon(list(), "XYM"),
st_geometrycollection(dim = "XYM")))

(e4 = st_sfc(st_point(rep(NA_real_,4)),
st_multipoint(matrix(numeric(0),0,4), dim = "XYZM"),
st_linestring(matrix(numeric(0), 0, 4)),
st_multilinestring(list(), "XYZM"),
st_polygon(list(), "XYZM"),
st_multipolygon(list(), "XYZM"),
st_geometrycollection(dim = "XYZM")))

st_as_sfc(st_as_binary(e1, pureR = TRUE))
st_as_sfc(st_as_binary(e1, pureR = FALSE))
st_as_sfc(st_as_binary(e2, pureR = FALSE))
st_as_sfc(st_as_binary(e3, pureR = FALSE))
st_as_sfc(st_as_binary(e4, pureR = FALSE))
st_as_sfc(st_as_binary(e1, pureR = FALSE))

# sfc_GEOMETRY:
x = st_sfc(st_point(0:1), st_linestring(matrix(1:4,2,2)))
st_intersects(x, x, sparse = FALSE)

# two empty geoms:
x = st_sfc(st_multipoint(), st_linestring())
st_intersects(x, x, sparse = FALSE)
