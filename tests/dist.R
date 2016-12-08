library(sf)
library(sp)
library(units)

x = st_sfc(
st_point(c(0,0)),
st_point(c(1,0)),
st_point(c(2,0)),
st_point(c(3,0)),
crs = 4326
)

y = st_sfc(
st_point(c(0,10)),
st_point(c(1,0)),
st_point(c(2,0)),
st_point(c(3,0)),
st_point(c(4,0)),
crs = 4326
)

st_distance(x, y)

d.sf = st_distance(x, y, geosphere::distMeeus)
d.sp = spDists(as(x, "Spatial"), as(y, "Spatial"))
units(d.sp) = make_unit("km")
d.sf - d.sp

#summary(unclass(d.sf) - d.sp)

st_crs(x) = st_crs(y) = NA
d.sf = st_distance(x, y)
d.sp = spDists(as(x, "Spatial"), as(y, "Spatial"))
d.sf - d.sp
