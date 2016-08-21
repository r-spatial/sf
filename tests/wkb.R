library(sf)
round_trip = function(x, EWKB = FALSE) {
	if (inherits(x, "sfi"))
		x = ST_sfc(list(x))
	wkb = ST_as.WKB(x, EWKB = EWKB)
	class(wkb) = "WKB"
	y = ST_as.sfc(wkb)
	a = all.equal(x, y)
	if (length(a) == 1 && is.logical(a))
		TRUE
	else {
		print(x)
		print(wkb)
		print(y)
		FALSE
	}
}

p3 = ST_Point(c(0,0,0))
p3m = ST_Point(c(0,0,0), "XYM")
p4 = ST_Point(c(0,0,0,0))
p2 = ST_Point(c(0,0))
ls = ST_LineString(matrix(1:6,3))
mp = ST_MultiPoint(matrix(1:6,3))

outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
pl1 = ST_Polygon(pts)

pol1 = list(outer, hole1, hole2)
pol2 = list(outer + 12, hole1 + 12)
pol3 = list(outer + 24)
mp1 = ST_MultiPolygon(list(pol1,pol2,pol3))

ml1 = ST_MultiLineString(list(outer, hole1, hole2))
gc = ST_GeometryCollection(list(p2, ls, pl1, mp1))

sapply(list(p3, p3m, p4, p2, ls, mp, pl1, mp1, ml1, gc), round_trip, EWKB = TRUE)
sapply(list(p3, p3m, p4, p2, ls, mp, pl1, mp1, ml1, gc), round_trip, EWKB = FALSE)
