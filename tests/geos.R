library(sf)
s = st_read(system.file("shapes/", package="maptools"), "sids")

st_distance(s[1:10,], s[1:10,])

st_is_valid(s)

ops = c("intersects", "disjoint", "touches", "crosses", "within", "contains", "overlaps", "equals", 
"covers", "coveredBy", "equalsExact", "isWithinDistance")
for (op in ops) {
	x = st_geos_binop(op, s[1:50,], s[51:100,], 0, FALSE)
	x = st_geos_binop(op, s[1:50,], s[51:100,], 0, TRUE)
}

try(x <- st_geos_binop("ErrorOperation", s[1:50,], s[51:100,], 0, TRUE))
