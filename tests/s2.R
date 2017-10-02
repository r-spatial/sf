
library(s2)
library(sf)

p0 = rbind(c(0,80), c(120,80), c(240,80), c(0,80))
p1 = rbind(c(0,85), c(120,85), c(240,85), c(0,85))

pol0 = st_sfc(st_polygon(list(p0)), crs = 4326)
pol1 = st_sfc(st_polygon(list(p1)), crs = 4326)

# 1:
sf:::s2_intersection(pol0, pol1)
# 0:
st_intersection(pol0, pol1)


pol0 = st_sfc(st_polygon(list(p0, p1)), crs = 4326)
# empty:
sf:::s2_intersection(pol0, pol1)

# not empty, although reversed ring direction:
pol0 = st_sfc(st_polygon(list(p0[4:1,])), crs = 4326)
sf:::s2_intersection(pol0, pol1)

# equator: ring direction counts; LHS=inside
eq = rbind(c(0,0), c(120,0), c(240,0), c(0,0))
pol0 = st_sfc(st_polygon(list(eq)), crs = 4326)
sf:::s2_intersection(pol0, pol1)

# equator: ring direction counts; RHS=outside
pol0 = st_sfc(st_polygon(list(eq[4:1,])), crs = 4326)
sf:::s2_intersection(pol0, pol1)

# no longer equator: ring direction ignored
eq = rbind(c(0,1), c(120,1), c(240,1), c(0,1))
pol0 = st_sfc(st_polygon(list(eq)), crs = 4326)
sf:::s2_intersection(pol0, pol1)
pol0 = st_sfc(st_polygon(list(eq[4:1,])), crs = 4326)
sf:::s2_intersection(pol0, pol1)

# intersection has hole:
p0 = rbind(c(0,80), c(120,80), c(240,80), c(0,80))
phole = rbind(c(0,89), c(120,89), c(240,89), c(0,89))
p1 = rbind(c(0,85), c(120,85), c(240,85), c(0,85))

pol0 = st_sfc(st_polygon(list(p0,phole)), crs = 4326)
pol1 = st_sfc(st_polygon(list(p1)), crs = 4326)
sf:::s2_intersection(pol0, pol1)


p0 = rbind(c(2,0),c(4,1),c(2,2),c(3,1),c(2,0))
p1 = rbind(c(3,0),c(2,1),c(3,2),c(0,1),c(3,0))

h0 = st_coordinates(st_buffer(st_point(c(2.4588,0.345)), .05))
h1 = st_coordinates(st_buffer(st_point(c(2.4794,1.66696)), .05))

pol0 = st_sfc(st_polygon(list(p0,h0[,1:2],h1[,1:2])), crs = 4326)
pol1 = st_sfc(st_polygon(list(p1)), crs = 4326)

sf:::s2_intersection(pol0, pol1)


p0 = rbind(c(0,80), c(120,80), c(240,80), c(0,80))
p1 = rbind(c(0,85), c(120,85), c(240,85), c(0,85))

pol0 = st_sfc(st_polygon(list(p0)), crs = 4326)
pol1 = st_sfc(st_polygon(list(p1)), crs = 4326)

sf:::s2_centroid(c(pol0, pol1))

sf:::s2_centroid(pol0)

sf:::s2_area(pol0)

st_intersects(pol0, pol1)
sf:::s2_intersects(pol0, pol1)

demo(nc, echo = FALSE, ask = FALSE)
all.equal(
  st_relate(nc, nc, pattern = "2********"),
  sf:::s2_intersects(nc, nc)
)
