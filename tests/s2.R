library(sf)
d = data.frame(z = 1:100, x = runif(100), y = runif(100))
n0 = st_as_sf(d, coords = c("x", "y"), crs = 4326)
n1 = st_transform(n0, 3857)

# st_nearest_points
cp1 = st_nearest_points(n0[1:50,], n0[51:100,])
cp2 = st_transform(st_nearest_points(n1[1:50,], n1[51:100,]), 4326)
length(cp1)
all.equal(cp1, cp2)

# st_nearest_points, pairwise
cp1 = st_nearest_points(n0[1:50,], n0[51:100,], pairwise = TRUE)
cp2 = st_transform(st_nearest_points(n1[1:50,], n1[51:100,], pairwise = TRUE), 4326)
length(cp1)
all.equal(cp1, cp2)

if (sf_extSoftVersion()["GEOS"] >= "3.6.1") {
# st_nearest_feature
  nf1 = st_nearest_feature(n0[1:50,], n0[51:100,])
  nf2 = st_nearest_feature(n1[1:50,], n1[51:100,])
  print(all.equal(nf1, nf2))
}
