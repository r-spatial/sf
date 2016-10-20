library(sf)
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"), "nc.gpkg", crs = 4267)

st_distance(nc[1:10,], nc[1:10,])

st_is_valid(nc)

ops = c("intersects", "disjoint", "touches", "crosses", "within", "contains", "overlaps", "equals", 
"covers", "covered_by", "equals_exact", "is_within_distance")
for (op in ops) {
	x = sf:::st_geos_binop(op, nc[1:50,], nc[51:100,], 0, FALSE)
	x = sf:::st_geos_binop(op, nc[1:50,], nc[51:100,], 0, TRUE)
}

try(x <- sf:::st_geos_binop("ErrorOperation", s[1:50,], s[51:100,], 0, TRUE))
