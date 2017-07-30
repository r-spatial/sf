suppressPackageStartupMessages(library(sf))
library(testthat)
# "vertical" conversions:
# column 1:
mp = st_sfc(st_multipoint(matrix(0:3,,2)), st_multipoint(matrix(10:15,,2)))
(ls = st_cast(mp, "LINESTRING"))
st_cast(ls, "MULTIPOINT")

# column 2:
mls = st_sfc(st_multilinestring(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10)), 
	rbind(c(5,5),c(5,6), c(6,6), c(6,5)))), 
	st_multilinestring(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1)))))
(pol = st_cast(mls, "POLYGON"))
st_cast(pol, "MULTILINESTRING")

# "horizontal" conversions:

(pt = st_cast(mp, "POINT"))
(i = attr(pt, "ids"))
(xx = st_cast(pt, "MULTIPOINT", rep(seq_along(i), i)))
(yy = st_cast(pt, "LINESTRING", rep(seq_along(i), i)))

(zz = st_cast(yy, "MULTILINESTRING"))
(zz = st_cast(yy, "POLYGON"))

st_cast(mls, "LINESTRING")

(g = st_sfc(c(mls, ls)))
st_cast(g, "MULTILINESTRING")
expect_warning(st_cast(g, "LINESTRING"))
st_cast(st_cast(g, "MULTILINESTRING"), "LINESTRING") # will not loose

gc = st_sfc(st_geometrycollection(
  list(
    st_multilinestring(list(rbind(c(0,0), c(10,0), c(10,10), c(0,10)), 
	rbind(c(5,5),c(5,6), c(6,6), c(6,5)))), 
	st_multilinestring(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1)))),
	st_point(0:1)
  )))
try(st_cast(mls, "POINT"))
try(st_cast(mls, "MULTIPOINT"))

outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pol1 = list(outer, hole1, hole2)
pol2 = list(outer + 12, hole1 + 12)
pol3 = list(outer + 24)
mp = list(pol1,pol2,pol3)
mp1 = st_multipolygon(mp)
s = st_sfc(mp1)
x = st_cast(s, "MULTIPOINT")
x = st_cast(s, "POINT")
expect_warning(st_cast(mp1, "LINESTRING"))
expect_warning(st_cast(mp1, "POINT"))
mls = mls[[1]]
class(mls)
#expect_error(st_cast(mls, "POLYGON"))
st_cast(mls, "POLYGON")

expect_warning(st_cast(mls, "POINT"))
p1 = st_polygon(pol1)
expect_warning(st_cast(p1, "POINT"))
ls = ls[[1]]
class(ls)
expect_warning(st_cast(ls, "POINT"))

mls = st_cast(p1, "MULTILINESTRING")
p2 = st_cast(mls, "POLYGON")

# st_is:
st_is(st_point(0:1), "POINT")
sfc = st_sfc(st_point(0:1), st_linestring(matrix(1:6,,2)))
st_is(sfc, "POINT")
st_is(sfc, "POLYGON")
st_is(sfc, "LINESTRING")
st_is(st_sf(a = 1:2, sfc), "LINESTRING")
st_is(sfc, c("POINT", "LINESTRING"))
