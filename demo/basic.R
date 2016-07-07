## ----error=TRUE----------------------------------------------------------
library(sf)
(p1 = POINT(c(1,2)))
bbox(p1)
(p2 = POINT(c(1,2,3)))
(p3 = POINT(c(1,2,3), "M"))
(p4 = POINT(c(1,2,3,4)))
try(POINT(1)) # Error:
try(POINT(1:5)) # Error:

## ----error=TRUE----------------------------------------------------------
pts = matrix(1:10, , 2)
(mp1 = MULTIPOINT(pts))
pts = matrix(1:15, , 3)
(mp2 = MULTIPOINT(pts))
(mp3 = MULTIPOINT(pts, "M"))
pts = matrix(1:20, , 4)
(mp4 = MULTIPOINT(pts))
try(MULTIPOINT(1)) # Error:
try(MULTIPOINT(1:5)) # Error:
bbox(mp1)

## ----error=TRUE----------------------------------------------------------
pts = matrix(1:10, , 2)
(ls1 = LINESTRING(pts))
pts = matrix(1:15, , 3)
(ls2 = LINESTRING(pts))
(ls3 = LINESTRING(pts, "M"))
pts = matrix(1:20, , 4)
(ls4 = LINESTRING(pts))
try(LINESTRING(pts[1,])) # Error:
try(LINESTRING(matrix(1:10, 2))) # Error:
bbox(ls1)

## ----error=TRUE----------------------------------------------------------
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
outer
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
(ml1 = MULTILINESTRING(pts))
pts3 = lapply(pts, function(x) cbind(x, 0))
(ml2 = MULTILINESTRING(pts3))
(ml3 = MULTILINESTRING(pts3, "M"))
pts4 = lapply(pts3, function(x) cbind(x, 0))
(ml4 = MULTILINESTRING(pts4))
bbox(ml1)

## ----error=TRUE----------------------------------------------------------
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
outer
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
(pl1 = POLYGON(pts))
pts3 = lapply(pts, function(x) cbind(x, 0))
(pl2 = POLYGON(pts3))
(pl3 = POLYGON(pts3, "M"))
pts4 = lapply(pts3, function(x) cbind(x, 0))
(pl4 = POLYGON(pts4))
bbox(pl1)

## ----error=TRUE----------------------------------------------------------
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
outer
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pol1 = list(outer, hole1, hole2)
pol2 = list(outer + 12, hole1 + 12)
pol3 = list(outer + 24)
mp = list(pol1,pol2,pol3)
(mp1 = MULTIPOLYGON(mp))
pts3 = lapply(mp, function(x) lapply(x, function(y) cbind(y, 0)))
(mp2 = MULTIPOLYGON(pts3))
(mp3 = MULTIPOLYGON(pts3, "M"))
pts4 = lapply(mp2, function(x) lapply(x, function(y) cbind(y, 0)))
(mp4 = MULTIPOLYGON(pts4))
bbox(mp1)

## ----error=TRUE----------------------------------------------------------
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
outer
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pol1 = list(outer, hole1, hole2)
pol2 = list(outer + 12, hole1 + 12)
pol3 = list(outer + 24)
mp = list(pol1,pol2,pol3)
mp1 = MULTIPOLYGON(mp)
(gc = GEOMETRYCOLLECTION(list(p1, ls1, pl1, mp1)))
(gc3 = GEOMETRYCOLLECTION(list(mp3, pl1)))
bbox(gc)

