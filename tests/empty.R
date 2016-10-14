library(sf)

st_point(rep(NA_real_,2))
st_point(rep(NA_real_,3), "XYZ")
st_point(rep(NA_real_,3), "XYM")
st_point(rep(NA_real_,4))

st_multipoint()
st_multipoint(dim = "XYZ")
st_multipoint(dim = "XYM")
st_multipoint(matrix(numeric(0), 0, 4))
st_multipoint(dim = "XYZM")

st_linestring(matrix(numeric(0), 0, 2))
st_linestring(matrix(numeric(0), 0, 3))
st_linestring(matrix(numeric(0), 0, 3), "XYM")
st_linestring(matrix(numeric(0), 0, 4))

st_multilinestring(list())
st_multilinestring(list(), "XYZ")
st_multilinestring(list(), "XYM")
st_multilinestring(list(), "XYZM")

st_polygon(list())
st_polygon(list(), "XYZ")
st_polygon(list(), "XYM")
st_polygon(list(), "XYZM")

st_multipolygon(list())
st_multipolygon(list(), "XYZ")
st_multipolygon(list(), "XYM")
st_multipolygon(list(), "XYZM")

st_geometrycollection()
st_geometrycollection(dim = "XYZ")
st_geometrycollection(dim = "XYM")
st_geometrycollection(dim = "XYZM")
