suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(units))

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

if (suppressPackageStartupMessages(require(sp, quietly = TRUE))) {
 d.sf = st_distance(x, y)
 d.sp = spDists(as(x, "Spatial"), as(y, "Spatial"))
 units(d.sp) = as_units("km")
 print(round(d.sf - d.sp, 7))

#summary(unclass(d.sf) - d.sp)

 st_crs(x) = st_crs(y) = NA
 d.sf = st_distance(x, y)
 d.sp = spDists(as(x, "Spatial"), as(y, "Spatial"))
 print(round(d.sf - d.sp, 7))
}

# st_length:
st_crs(y) = 4326
(z = st_sfc(st_linestring(rbind(c(0,10), c(1,0), c(2,0), c(3,0), c(4,0))), crs = 4326))
d = st_distance(y, y)
round(d, 7)
st_length(z)
round(st_length(z) - sum(d[1,2], d[2,3], d[3,4], d[4,5]), 7)

# st_line_sample:
ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
	st_linestring(rbind(c(0,0),c(10,0))))
# set.seed(135)
st_line_sample(ls, density = 1)

ls = st_sfc(st_linestring(rbind(c(0,0),c(0,1))),
	st_linestring(rbind(c(0,0),c(.1,0))), crs = 4326)

st_length(ls)
try(st_line_sample(ls, density = 1/1000))
x = st_line_sample(st_transform(ls, 3857), density = 1/1000) # one per km
