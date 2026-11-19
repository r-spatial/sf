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
st_agr(x) = "constant"
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(18,2))

x = st_sf(s = 20, geom = st_sfc(sq(0, .5, 1)))
st_agr(x) = "constant"
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(4.5/(4.5+1/6)*20, (1/6)/(4.5+1/6)*20))


x = st_sf(s = 20, geom = st_sfc(sq(-.5, .5, 1)))
st_agr(x) = "constant"
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(4.5/(4.5+1/6)*20, (1/6)/(4.5+1/6)*20))

x = st_sf(s = 20, geom = st_sfc(sq(-1, .5, 1)))
st_agr(x) = "constant"

# FIXME:
# expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a", keep_NA = TRUE)$s, c(NA_real_,NA_real_))

geom = st_sfc(
  sq(0, 0, 1),
  sq(1, 0, 1),
  sq(0, 1, 1),
  sq(1, 1, 1))
x = st_sf(s = 1:4, geom = geom)
st_agr(x) = "constant"
expect_equal(st_interpolate_aw(x, to, extensive = TRUE, weights = "a")$s, c(3,7))
})

test_that("st_interpolate_aw: raster-raster, vector-raster, raster-vector and vector-vector agree", {
  skip_if_not_installed("stars")
  # the two mixed raster/vector paths use GEOSGridIntersectionFractions()
  skip_if(utils::compareVersion(
    gsub("[a-zA-Z].+$", "", sf_extSoftVersion()[["GEOS"]]), "3.14.0") < 0)
  suppressPackageStartupMessages(library(stars))

  # Two partially overlapping regular rasters. Their cell edges deliberately
  # do not coincide, so only cells with a positive-area overlap take part and
  # the vector-vector result does not pick up zero-area (edge touching) cells.
  mk_raster = function(values, xs, ys)
    st_as_stars(matrix(values, 4, 4, byrow = TRUE),
      dimensions = st_dimensions(x = xs, y = ys, .raster = c("x", "y")))
  a = mk_raster(1:16, 0.5 + 0:3, 3.5 - 0:3)          # bbox x [0.5,4.5],  y [-0.5,3.5]
  b = mk_raster(101:116, 2.87 + 0:3, 3.73 - 0:3)      # bbox x [2.87,6.87], y [-0.27,3.73]

  av = st_as_sf(a, as_points = FALSE)
  bv = st_as_sf(b, as_points = FALSE)
  names(av) = names(bv) = c("v", "geometry")
  st_agr(av) = st_agr(bv) = "constant"

  # Align all four results by the centroid of the target (b) cells.
  key = function(g) {
    xy = st_coordinates(st_centroid(g))
    paste(round(xy[, 1], 5), round(xy[, 2], 5), sep = ",")
  }
  keys = key(st_geometry(bv))
  # stars results are in target cell order; the sf result contains only the
  # intersecting cells, so index it by geometry (NA elsewhere).
  values = function(o) {
    if (inherits(o, "sf")) {
      out = setNames(rep(NA_real_, length(keys)), keys)
      out[key(st_geometry(o))] = as.numeric(st_drop_geometry(o)[[1]])
      out
    } else
      setNames(as.vector(o[[1]]), keys)
  }

  for (extensive in c(TRUE, FALSE)) {
    rr_obj = st_interpolate_aw(a,  b,  extensive = extensive)     # raster -> raster
    rv_obj = st_interpolate_aw(a,  bv, extensive = extensive)     # raster -> vector
    vr_obj = st_interpolate_aw(av, b,  extensive = extensive)     # vector -> raster
    vv_obj = st_interpolate_aw(av, bv, extensive = extensive)     # vector -> vector
    rr = values(rr_obj); rv = values(rv_obj); vr = values(vr_obj); vv = values(vv_obj)

    expect_equal(rr, rv)
    expect_equal(rr, vr)
    expect_equal(rr, vv)
    expect_equal(sum(!is.na(rr)), 8L) # only 8 cells have a positive overlap

    expect_s3_class(st_as_sf(rv_obj), "sf")
  }
})

test_that("st_interpolate_aw: coinciding cell edges leave zero-area cells in the vector-vector result", {
  skip_if_not_installed("stars")
  skip_if(utils::compareVersion(
    gsub("[a-zA-Z].+$", "", sf_extSoftVersion()[["GEOS"]]), "3.14.0") < 0)
  suppressPackageStartupMessages(library(stars))

  # Here the grids are aligned: b shares cell edges with a. Besides the 8
  # genuinely overlapping cells, 4 cells of b merely touch a along an edge.
  mk_raster = function(values, xs, ys)
    st_as_stars(matrix(values, 4, 4, byrow = TRUE),
      dimensions = st_dimensions(x = xs, y = ys, .raster = c("x", "y")))

  test_ab = function(a, b) {
    av = st_as_sf(a, as_points = FALSE)
    bv = st_as_sf(b, as_points = FALSE)
    names(av) = names(bv) = c("v", "geometry")
    st_agr(av) = st_agr(bv) = "constant"
    for (extensive in c(TRUE, FALSE)) {
      rr = st_interpolate_aw(a,  b,  extensive = extensive)
      rv = st_interpolate_aw(a,  bv, extensive = extensive)
      vr = st_interpolate_aw(av, b,  extensive = extensive)
      vv = st_interpolate_aw(av, bv, extensive = extensive, keep_NA = TRUE)
  
      expect_equal(as.vector(rr[[1]]), rv[[1]], tolerance = 1e-6)
      expect_equal(rr[[1]], vr[[1]], tolerance = 1e-6)
      expect_equal(as.vector(rr[[1]]), vv[[1]], tolerance = 1e-6)
    }
  }
  a = mk_raster(1:16, 0.5 + 0:3, 3.5 - 0:3)
  b = mk_raster(101:116, 2.4 + 0:3, 3.3 - 0:3) # coinciding cell edges
  test_ab(a, b)
  # aligned cells:
  a = mk_raster(1:16, 0.5 + 0:3, 3.5 - 0:3)
  b = mk_raster(101:116, 2.5 + 0:3, 3.5 - 0:3) # coinciding cell edges
  test_ab(a, b)
})
