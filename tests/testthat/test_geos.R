context("sf: geos tests")

test_that("CPL_goes_relate works", {
  r1 = sf:::CPL_geos_relate(st_sfc(st_point(c(0,0))), st_sfc(st_linestring(rbind(c(0,0),c(1,1)))))
  library(sp)
  p = SpatialPoints(matrix(0,1,2))
  l = Lines(list(Line(rbind(c(0,0),c(1,1)))), "ID")
  sl = SpatialLines(list(l))
  library(rgeos)
  r2 = gRelate(p, sl)
  expect_true(all(r1 == r2))
})

test_that("CPL_geos_is_valid works", {
  expect_true( sf:::CPL_geos_is_valid(
  	st_sfc(st_polygon(list(cbind(c(0,1,1,0,0), c(0,0,1, 1,0)))))))
  expect_warning(
    expect_false(sf:::CPL_geos_is_valid(
  	st_sfc(st_polygon(list(cbind(c(0,1,1,.5,0),c(0,0,1,-1,0))))), FALSE))
    )
  expect_false(st_is_valid(st_sfc(st_polygon(list(cbind(c(0,1,1,.5,0),c(0,0,1,-1,0)))))))
  p0 = st_as_sfc(factor("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))"))
  p1 = st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))")
  expect_equal(p0, p1)
  expect_false(st_is_valid(p1))
  expect_equal(st_is_valid(p1, reason = TRUE), "Self-intersection[5 5]")
})

test_that("geos ops give warnings and errors on longlat", {
	nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
	x = nc[1:2,]
	y = nc[2:3,]
	expect_silent(st_equals(x, y))
	expect_silent(st_equals_exact(x, y, 0.01))
	l = st_sfc(st_linestring(matrix(1:10, ncol=2)), crs = st_crs(nc))
	expect_silent(st_polygonize(l))

	expect_message(st_intersects(x,y))
	expect_message(st_disjoint(x,y))
	expect_message(st_touches(x,y))
	expect_message(st_crosses(x,y))
	expect_message(st_within(x,y))
	expect_message(st_contains(x,y))
	expect_message(st_overlaps (x,y))
	expect_message(st_covers(x,y))
	expect_message(st_covered_by(x,y))

	expect_warning(st_buffer(x, .1))
	expect_warning(st_simplify(x, .1))
	expect_warning(st_centroid(x))
	expect_silent(st_segmentize(l, 1e5))
	expect_silent(st_segmentize(l, 1e5))

	expect_silent(out <- st_segmentize(l, units::set_units(0.001, rad)))
	expect_silent(out <- st_segmentize(l, units::set_units(100, km)))

	if (CPL_geos_version() >= "3.4.0")
		expect_warning(st_triangulate(x))
	else
		expect_error(st_triangulate(x))

	expect_silent(st_area(x))
	expect_silent(st_length(l))

	# errors:
	expect_error(st_distance(x, y))
})

test_that("st_area() works on GEOMETRY in longlat (#131)", {
  single <- list(rbind(c(0,0), c(1,0), c(1, 1), c(0,1), c(0,0))) %>% st_polygon()
  multi <- list(single + 2, single + 4) %>% st_multipolygon()
  
  w <- st_sfc(single, multi)
  expect_equal(st_area(w), 1:2)
  expect_equal(st_area(st_set_crs(w, 4326)) %>% as.numeric(), 
               c(12308778361, 24570125261))
})




nc = st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
pnc <- st_transform(nc[4:6, ], "+proj=laea +lon_0=-90")
gpnc <- st_geometry(pnc)

suppressWarnings(lnc <- st_cast(pnc, "MULTILINESTRING"))
glnc <- st_geometry(lnc)

test_that("geom operations work on sfg or sfc or sf", {
  expect_silent(st_buffer(pnc, 1000))
  expect_silent(st_buffer(gpnc, 1000))
  expect_silent(st_buffer(gpnc[[1L]], 1000))
  
  expect_silent(st_boundary(pnc))
  expect_that(st_boundary(gpnc), is_a("sfc_MULTILINESTRING"))
  expect_that(st_boundary(gpnc[[1L]]), is_a("MULTILINESTRING"))
  
  expect_true(inherits(st_convex_hull(pnc)$geometry, "sfc_POLYGON"))
  expect_true(inherits(st_convex_hull(gpnc), "sfc_POLYGON"))
  expect_true(inherits(st_convex_hull(gpnc[[1L]]), "POLYGON"))
  
  expect_silent(st_simplify(pnc, FALSE, 1e4))
  expect_silent(st_simplify(gpnc, FALSE, 1e4))
  expect_silent(st_simplify(gpnc[[1L]], FALSE, 1e4))

  if (sf:::CPL_geos_version() >= "3.4.0") {  
   expect_silent(st_triangulate(pnc))
   expect_that(st_triangulate(gpnc), is_a("sfc_GEOMETRYCOLLECTION"))
   expect_that(st_triangulate(gpnc[[1]]), is_a("GEOMETRYCOLLECTION"))
  }
  
  expect_silent(st_polygonize(lnc))
  expect_silent(st_polygonize(glnc)) 
  expect_silent(st_polygonize(glnc[[1]])) 
  
  expect_that(st_line_merge(lnc), is_a("sf"))
  expect_that(st_line_merge(glnc), is_a("sfc"))
  expect_that(st_line_merge(glnc[[4]]), is_a("sfg"))
  
  expect_silent(st_centroid(lnc))
  expect_that(st_centroid(glnc),  is_a("sfc_POINT"))
  expect_that(st_centroid(glnc[[1]]),  is_a("POINT"))
  
  expect_silent(st_segmentize(lnc, 10000))
  expect_silent(st_segmentize(glnc, 10000))
  expect_silent(st_segmentize(glnc[[1]], 10000))
})

test_that("st_union/difference/sym_difference/intersection work, for all types", {
  p = st_point(0:1)
  l = st_linestring(matrix(1:10,,2))
  pl = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
  x = list(
  	pl,
	st_sfc(pl,l,pl),
	st_sf(a=5:7, st_sfc(pl,l,pl), agr = "constant")
  )
  y = x
  for (f in list(st_union, st_difference, st_sym_difference, st_intersection)) {
  	for (xx in x)
	  for (yy in y)
	  	expect_silent(f(xx,yy))
  }
  for (f in list(st_difference, st_sym_difference, st_intersection)) {
  	for (xx in x)
	  for (yy in y)
	  	expect_equal(tail(class(f(xx,yy)),1), tail(class(xx),1))
  }
})


test_that("st_union works with by_feature", {
  p = st_point(0:1)
  l = st_linestring(matrix(1:10,,2))
  pl = st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
  x = list(
  	pl,
	st_sfc(pl,l,pl),
	st_sf(a=5:7, st_sfc(pl,l,pl), agr = "constant")
  )
  expect_silent(z <- st_union(x[[1]], by_feature = TRUE))
  expect_silent(z <- st_union(x[[2]], by_feature = TRUE))
  expect_silent(z <- st_union(x[[3]], by_feature = TRUE))
})
