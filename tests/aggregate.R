library(sf)
# aggregate
pl1 = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,0))))
pl2 = st_polygon(list(rbind(c(0,0),c(1,1),c(0,1),c(0,0))))
s = st_sf(a = 1:2, geom = st_sfc(pl1, pl2))
(a = aggregate(s, list(c(1,1)), mean, do_union = FALSE))
(a = aggregate(s, list(c(1,1)), mean, do_union = TRUE))
# expect_warning(st_cast(a, "POINT"))
if (suppressPackageStartupMessages(require(sp, quietly = TRUE))) {
 demo(meuse_sf, echo = FALSE, ask = FALSE)
 a = aggregate(meuse_sf, list(meuse_sf$soil), mean)
 print(attributes(a)$agr)
 a = aggregate(meuse_sf, list(soil = meuse_sf$soil), mean)
 print(attributes(a)$agr)
 a = aggregate(meuse_sf, list(meuse_sf$soil, meuse_sf$ffreq), mean)
 print(attributes(a)$agr)
 a = aggregate(meuse_sf, list(soil = meuse_sf$soil, ff = meuse_sf$ffreq), mean)
 print(attributes(a)$agr)
}

# aggregate by sf/sfc
a = st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))) * 2
b = a + 1
p = st_sfc(st_point(c(0.1,0.1)), st_point(c(1.5,1.5)), st_point(c(2.9,2.9)))
x = st_sf(count = 1:3, geom = p)
aggregate(x, st_sfc(a,b), mean)
aggregate(x, st_sf(st_sfc(a,b)), mean)
aggregate(x, st_sf(st_sfc(a,b,b+10)), mean)
