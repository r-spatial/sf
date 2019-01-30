suppressPackageStartupMessages(library(sf))
library(dplyr)

# plot linestrings:
l1 = st_linestring(matrix(runif(6)-0.5,,2))
l2 = st_linestring(matrix(runif(6)-0.5,,2))
l3 = st_linestring(matrix(runif(6)-0.5,,2))
s = st_sf(a=2:4, b=st_sfc(l1,l2,l3))
plot(s, col = s$a, axes = FALSE)
plot(s, col = s$a)
attr(s$b, "proj4string") = sp::CRS("+init=epsg:4326")@projargs
plot(s, col = s$a, axes = TRUE)
plot(s, col = s$a, lty = s$a, lwd = s$a, pch = s$a, type = 'b')
l4 = st_linestring(matrix(runif(6),,2))
plot(st_sf(a=1,b=st_sfc(l4)), add = TRUE)
# plot multilinestrings:
ml1 = st_multilinestring(list(l1, l2))
ml2 = st_multilinestring(list(l3, l4))
ml = st_sf(a = 2:3, b = st_sfc(ml1, ml2))
plot(ml, col = ml$a, lty = ml$a, lwd = ml$a, pch = ml$a, type = 'b')
# plot points:
p1 = st_point(c(1,2))
p2 = st_point(c(3,3))
p3 = st_point(c(3,0))
p = st_sf(a=2:4, b=st_sfc(p1,p2,p3))
plot(p, col = s$a, axes = TRUE)
plot(p, col = s$a)
plot(p, col = p$a, pch = p$a, cex = p$a, bg = s$a, lwd = 2, lty = 2, type = 'b')
p4 = st_point(c(2,2))
plot(st_sf(a=1, st_sfc(p4)), add = TRUE)
# multipoints:
mp1 = st_multipoint(matrix(1:4,2))
mp2 = st_multipoint(matrix(5:8,2))
mp = st_sf(a = 2:3, b = st_sfc(mp1, mp2))
plot(mp)
plot(mp, col = mp$a, pch = mp$a, cex = mp$a, bg = mp$a, lwd = mp$a, lty = mp$a, type = 'b')
# polygon:
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pl1 = st_polygon(list(outer, hole1, hole2))
pl2 = st_polygon(list(outer+10, hole1+10, hole2+10))
po = st_sf(a = 2:3, st_sfc(pl1,pl2))
plot(po, col = po$a, border = rev(po$a), lwd=3)
# multipolygon
r10 = matrix(rep(c(0,10),each=5),5)
pl1 = list(outer, hole1, hole2)
pl2 = list(outer+10, hole1+10, hole2+10)
pl3 = list(outer+r10, hole1+r10, hole2+r10)
mpo1 = st_multipolygon(list(pl1,pl2))
mpo2 = st_multipolygon(list(pl3))
mpo = st_sf(a=2:3, b=st_sfc(mpo1,mpo2))
plot(mpo, col = mpo$a, border = rev(mpo$a), lwd = 2)
# geometrycollection:
gc1 = st_geometrycollection(list(mpo1, st_point(c(21,21)), l1 * 2 + 21))
gc2 = st_geometrycollection(list(mpo2, l2 - 2, l3 - 2, st_point(c(-1,-1))))
gc = st_sf(a=2:3, b = st_sfc(gc1,gc2))
plot(gc, cex = gc$a, col = gc$a, border = rev(gc$a) + 2, lwd = 2)

plot(gc1)

plot(st_sfc(mp1, mpo1))

# color ramp
nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
plot(nc)
plot(nc, axes = TRUE)
plot(nc, col="lightgrey") 
plot(st_centroid(nc), add = TRUE, col = 1)
nc %>% 
  select(geometry) %>% 
  plot()

nc$f = cut(nc[[1]], 5)
plot(nc["f"], key.pos = 1)
plot(nc[1],   key.pos = 1)

# test background map plotting:
load("bgmap.rda")
merc = st_crs(3857)
WGS84 = st_crs(4326)
nc = st_transform(nc, WGS84)
## ggmap:
#library(ggmap)
#bgMap = get_map(unname(st_bbox(nc)), source = "google", zoom = 8) 
plot(st_transform(nc[1], merc), bgMap = bgMap)

# RgoogleMaps:
#library(RgoogleMaps)
#center = c(mean(st_bbox(nc)[c(2,4)]), mean(st_bbox(nc)[c(1,3)]))
#g = GetMap(center=center, zoom=6) # google
par(mar = c(0,0,1,0))
plot(st_transform(nc, merc), bgMap = g)

m = st_make_grid()
st_crs(m) = NA_crs_
m = st_segmentize(m, 2)
st_crs(m) = 4326
plot(m, axes = TRUE)
g = st_transform(m, st_crs("+proj=ortho +lat_0=30 +lon_0=45"), check = TRUE)
plot(g, axes = TRUE)

nc[[1]] = NA
nc[[10]] = 1
plot(nc, pal = rainbow, nbreaks = 3)
plot(nc, pal = rainbow, breaks = "jenks", nbreaks = 3)
plot(nc, pal = rainbow, breaks = (0:10)/3)

# logz:
nc$e = 10^(nc$SID74)
plot(nc["e"], logz = TRUE)

# shared key:
plot(nc[c("SID74", "SID79")], key.pos = -1)
plot(nc[c("BIR74", "BIR79")], key.pos = 1, logz=TRUE)
