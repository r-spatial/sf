context("sf: geos tests")

test_that("st_relate works", {
  r1 = st_relate(st_sfc(st_point(c(0,0))), st_sfc(st_linestring(rbind(c(0,0),c(1,1)))))
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
	expect_warning(st_buffer(x, .1, joinStyle = "BEVEL"))
	expect_warning(st_simplify(x, dTolerance = .1))
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

	# distance on long/lat:
	if (utils::packageVersion("lwgeom") <= "0.1-0")
		expect_error(st_distance(x, y))
	else
		expect_silent(st_distance(x, y))
})

test_that("st_area() works on GEOMETRY in longlat (#131)", {
  single <- list(rbind(c(0,0), c(1,0), c(1, 1), c(0,1), c(0,0))) %>% st_polygon()
  multi <- list(single + 2, single + 4) %>% st_multipolygon()

  w <- st_sfc(single + 0.1, multi)
  expect_equal(st_area(w), 1:2)
  expect_silent(st_area(st_set_crs(w, 4326))) # outcome might depend on backend used: lwgeom if proj.4 < 490, else proj.4
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

	expect_silent(st_buffer(pnc, 1000, endCapStyle = "SQUARE"))
	expect_silent(st_buffer(gpnc, 1000, joinStyle = "BEVEL"))
	expect_silent(st_buffer(gpnc[[1L]], 1000, joinStyle = "MITRE", mitreLimit = 0.2))

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
	expect_that(st_line_merge(glnc[[3]]), is_a("sfg"))

	expect_warning(st_centroid(lnc)) # was: silent
	expect_that(st_centroid(glnc),  is_a("sfc_POINT"))
	expect_that(st_centroid(glnc[[1]]),  is_a("POINT"))

	expect_warning(st_point_on_surface(lnc)) # was: silent
	expect_that(st_point_on_surface(glnc),  is_a("sfc_POINT"))
	expect_that(st_point_on_surface(glnc[[1]]),  is_a("POINT"))

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

test_that("st_difference works with partially overlapping geometries", {
	# create input testing data
	pl1 = st_polygon(list(matrix(c(0, 0, 2, 0, 1, 1, 0 ,0), byrow = TRUE, ncol=2)))
	pl2 = st_polygon(list(matrix(c(0, 0.5, 2, 0.5, 1, 1.5, 0, 0.5), byrow = TRUE, ncol = 2)))
	pl3 = st_polygon(list(matrix(c(0, 1.25, 2, 1.25, 1, 2.5, 0, 1.25), byrow = TRUE, ncol = 2)))
	in1 = st_sfc(list(pl1, pl2, pl3))
	in2 = st_sf(order = c("A", "B", "C"), geometry = st_sfc(list(pl1, pl2, pl3), crs = 4326), agr = "constant")
	correct_geom = st_sfc(list(
		st_polygon(list(matrix(c(0, 2, 1, 0, 0, 0, 1, 0), ncol = 2))),
		st_polygon(list(matrix(c(0.5, 0, 1, 2, 1.5, 1, 0.5, 0.5, 0.5, 1.5, 0.5, 0.5, 1, 0.5), ncol = 2))),
		st_polygon(list(matrix(c(0.75, 0, 1, 2, 1.25, 1, 0.75, 1.25, 1.25, 2.5, 1.25, 1.25, 1.5, 1.25), ncol = 2)))))
	# erase overlaps
	out1 = st_difference(in1)
	out2 = st_difference(in2)
	# check that output class is correct
	expect_is(out1, "sfc")
	expect_is(out2, "sf")
	# check that output geometries are valid
	expect_true(all(sf::st_is_valid(out1)))
	expect_true(all(sf::st_is_valid(out2)))
	# check that output geometries have correct attributes
	expect_equal(attr(out1, "idx"), seq_len(3))
	#expect_equal(attr(out2, "idx"), seq_len(3))
	expect_equal(attr(out1, "crs"), attr(in1, "crs"))
	expect_equal(st_crs(out2), st_crs(in2))
	# check that output geometries are actually correct
	expect_equal(length(out1), 3)
	expect_equal(nrow(out2), 3)
	expect_equal(out1[[1]][[1]], correct_geom[[1]][[1]])
	expect_equal(out1[[2]][[1]], correct_geom[[2]][[1]])
	expect_equal(out1[[3]][[1]], correct_geom[[3]][[1]])
	#expect_equal(out2[[1]][[1]], correct_geom[[1]][[1]])
	#expect_equal(out2[[2]][[1]], correct_geom[[2]][[1]])
	#expect_equal(out2[[3]][[1]], correct_geom[[3]][[1]])
})

test_that("st_difference works with fully contained geometries", {
	# create input testing data
	pl1 = st_polygon(list(matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), byrow = TRUE, ncol=2)))
	pl2 = st_polygon(list(matrix(c(0.5, 0.5, 1.5, 0.5, 1.5, 1.5, 0.5, 1.5, 0.5, 0.5), byrow = TRUE, ncol = 2)))
	pl3 = st_polygon(list(matrix(c(5, 5, 7, 5, 7, 7, 5, 7, 5, 5), byrow = TRUE, ncol = 2)))
	in1 = st_sfc(list(pl1, pl2, pl3))
	in2 = st_sf(order = c("A", "B", "C"), geometry = st_sfc(list(pl1, pl2, pl3), crs = 4326), agr = "constant")
	correct_geom = st_sfc(list(pl1, pl3))
	# erase overlaps
	out1 = st_difference(in1)
	out2 = st_difference(in2)
	# check that output class is correct
	expect_is(out1, "sfc")
	expect_is(out2, "sf")
	# check that output geometries are valid
	expect_true(all(sf::st_is_valid(out1)))
	expect_true(all(sf::st_is_valid(out2)))
	# check that output geometries have correct attributes
	expect_equal(attr(out1, "idx"), c(1L, 3L))
	#expect_equal(attr(out2, "idx"), c(1L, 3L))
	expect_equal(attr(out1, "crs"), attr(in1, "crs"))
	expect_equal(st_crs(out2), st_crs(in2))
	# check that output geometries are actually correct
	expect_equal(length(out1), 2)
	expect_equal(length(out2), 2)
	expect_equal(out1[[1]][[1]], correct_geom[[1]][[1]])
	#expect_equal(out1[[2]][[1]], correct_geom[[2]][[1]])
	#expect_equal(out2[[1]][[1]], correct_geom[[1]][[1]])
	#expect_equal(out2[[2]][[1]], correct_geom[[2]][[1]])
	# check change in order
	in3 = st_sfc(list(pl2, pl1))
	correct_geom = list(pl2, st_difference(pl1, pl2))
	out3 = st_difference(in3)
	expect_equal(correct_geom[[1]], out3[[1]])
	expect_equal(correct_geom[[2]], out3[[2]])
})

test_that("binary operations work on sf objects with common column names", {
	pol1 <- st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0)))))
	pol2 <- pol1 + 1
	sf1 <- st_sf(id = 1, pol1)
	sf2 <- st_sf(id = 2, pol2)
	# Test as regular data.frames
	expect_is(st_intersection(sf1, sf2), "sf")
	# Convert to tibbles
	sf1 <- st_as_sf(tibble::as_tibble(sf1))
	sf2 <- st_as_sf(tibble::as_tibble(sf2))
	expect_is(st_intersection(sf1, sf2), c("sf", "tbl_df"))
})

test_that("binary operations on empty sfg objects return NA", {
  x = st_point() == st_linestring()
  expect_true(is.na(x))
})
