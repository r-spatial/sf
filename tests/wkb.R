library(sf)
round_trip = function(x, EWKB = FALSE, pureR = FALSE) {
	if (inherits(x, "sfg"))
		x = st_sfc(x)
	wkb = st_as_binary(x, EWKB = EWKB, pureR = pureR)
	class(wkb) = "WKB"
	# print(wkb)
	y = st_as_sfc(wkb, EWKB = EWKB, pureR = pureR)
	a = all.equal(x, y)
	if (length(a) == 1 && is.logical(a) && a)
		TRUE
	else {
		print(x)
		print(wkb)
		print(y)
		FALSE
	}
}

p3 = st_point(c(0,0,0))
p3m = st_point(c(0,0,0), "XYM")
p4 = st_point(c(0,0,0,0))
p2 = st_point(c(0,0))
ls = st_linestring(matrix(1:6,3))
mp = st_multipoint(matrix(1:6,3))

outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1, hole2)
pl1 = st_polygon(pts)

pol1 = list(outer, hole1, hole2)
pol2 = list(outer + 12, hole1 + 12)
pol3 = list(outer + 24)
mp1 = st_multipolygon(list(pol1,pol2,pol3))

ml1 = st_multilinestring(list(outer, hole1, hole2))
gc = st_geometrycollection(list(p2, ls, pl1, mp1))

sapply(list(p3, p3m, p4, p2, ls, mp, pl1, mp1, ml1, gc), round_trip, EWKB = FALSE)
sapply(list(p3, p3m, p4, p2, ls, mp, pl1, mp1, ml1, gc), round_trip, EWKB = FALSE, pureR = TRUE)
sapply(list(p3, p3m, p4, p2, ls, mp, pl1, mp1, ml1, gc), round_trip, EWKB = TRUE)
sapply(list(p3, p3m, p4, p2, ls, mp, pl1, mp1, ml1, gc), round_trip, EWKB = TRUE, pureR = TRUE)

rawToHex(st_as_binary(st_multipoint(matrix(1:6,3))))
rawToHex(st_as_binary(st_sfc(st_point(c(0,1)), st_multipoint(matrix(1:6,3)))))
try(rawToHex("error"))
