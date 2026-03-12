test_that("st_interpolate_aw", {
sq = function(x, y, s=1) st_polygon(list(rbind(c(x,y),c(x+s,y),c(x+s,y+s),c(x,y+s),c(x,y))))
sq2 = function(x, y) st_polygon(list(rbind(c(x,y),c(x+1,y),c(x+1,y+2),c(x,y+2),c(x,y))))
x = st_sf(a = 1:2, b = c(10,1), geom = st_sfc(sq(2,3), sq(2,4)))
# plot(st_geometry(x))
y1 = st_sfc(sq2(2, 3))
# plot(y, add = TRUE, lty = 2, lwd = 3)
st_agr(x) = "constant"
expect_equal(st_interpolate_aw(x, y1, extensive = TRUE)$a,  3)
expect_equal(st_interpolate_aw(x, y1, extensive = FALSE)$a, 1.5)
y2 = st_sf(a = 5, st_sfc(sq2(1.5, 2.5)))
st_agr(y2) = "constant"
# plot(y, add = TRUE, lty = 3, lwd = 3)
st_interpolate_aw(x, y2, extensive = TRUE)
expect_equal(st_interpolate_aw(x, y2, extensive = TRUE)$a, 1)
expect_equal(st_interpolate_aw(x, y2, extensive = FALSE)$a, 1 + 1/3)
expect_equal(st_interpolate_aw(x, y2, extensive = FALSE, include_non_intersected = TRUE)$a, 0.5)
expect_equal(st_interpolate_aw(x, y2, extensive = TRUE, weights = "a")$b, 11)
expect_error(st_interpolate_aw(x, y2, extensive = FALSE, weights = "a"))
expect_error(st_interpolate_aw(x, y2, extensive = TRUE, include_non_intersected = TRUE, weights = "a"), "include")

t1 = sq(0, 1)
t2 = sq(0, 0, 2)
t = st_difference(t2, t1)
to = st_sf(a = c(9,1), geom = st_sfc(t1,t))
x = st_sf(s = 20, geom = st_sfc(sq(0, 0, 2)))
st_agr(to) = "constant"
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(18,2))

x = st_sf(s = 20, geom = st_sfc(sq(0, .5, 1)))
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(4.5/(4.5+1/6)*20, (1/6)/(4.5+1/6)*20))


x = st_sf(s = 20, geom = st_sfc(sq(-.5, .5, 1)))
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(4.5/(4.5+1/6)*20, (1/6)/(4.5+1/6)*20))

x = st_sf(s = 20, geom = st_sfc(sq(-1, .5, 1)))
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(NA_real_,NA_real_))

geom = st_sfc(
  sq(0, 0, 1),
  sq(1, 0, 1),
  sq(0, 1, 1),
  sq(1, 1, 1))
x = st_sf(s = 1:4, geom = geom)
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(3,7))
})
