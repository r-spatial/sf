context("sf: wkt")

test_that("well-known text", {
  gcol <- st_geometrycollection(list(st_point(1:2), st_linestring(matrix(1:4,2))))
  expect_message(x <- print(gcol), 
                 "GEOMETRYCOLLECTION \\(POINT \\(1 2\\), LINESTRING \\(1 3, 2 4\\)\\)", all = TRUE)
  expect_equal(x, "GEOMETRYCOLLECTION (POINT (1 2), LINESTRING (1 3, 2 4))")
  
  p1 = st_point(1:3)
  p2 = st_point(5:7)
  sfc = st_sfc(p1,p2)
  expect_identical(st_as_text(sfc), c("POINT Z (1 2 3)", "POINT Z (5 6 7)"))
  expect_equal(st_sfc(gcol), st_as_sfc(list("GEOMETRYCOLLECTION (POINT (1 2), LINESTRING (1 3, 2 4))")))
})

test_that("detect ewkt", {
	expect_equal(is_ewkt(c("LINESTRING(1663106 -105415,1664320 -104617)",
			  "SRID=4326;POLYGON(1.0 -2.5,3.2 -5.70000)")),
				 c(FALSE, TRUE))
})

test_that("can parse ewkt", {
	expect_equal(get_crs_ewkt("SRID=4326;POINT(1.0 -2.5,3.2 -5.7)"), 4326)
	expect_equal(ewkt_to_wkt("SRID=4326;POINT(1.0 -2.5, 3.2 -5.7)"),
				 "POINT(1.0 -2.5, 3.2 -5.7)")
	expect_equal(ewkt_to_wkt("POINT(1.0 -2.5, 3.2 -5.7)"),
				 "POINT(1.0 -2.5, 3.2 -5.7)")
})

test_that("can read ewkt", {
	expect_equal(st_as_sfc("SRID=3879;LINESTRING(1663106 -105415,1664320 -104617)"),
				 st_as_sfc("LINESTRING(1663106 -105415,1664320 -104617)", 3879))
	expect_equal(st_as_sfc(c("SRID=3879;LINESTRING(1663106 -105415,1664320 -104617)",
							 "SRID=3879;LINESTRING(0 0,1 1)")),
				 st_as_sfc(c("LINESTRING(1663106 -105415,1664320 -104617)",
				 			"LINESTRING(0 0,1 1)"), 3879)
				 )
	expect_equal(st_crs(st_as_sfc(c("SRID=3879;LINESTRING(1663106 -105415,1664320 -104617)",
							 "SRID=3879;LINESTRING(0 0,1 1)"))), st_crs(3879))
	expect_error(st_as_sfc(c("SRID=3879;LINESTRING(1663106 -105415,1664320 -104617)",
									"SRID=4326;LINESTRING(0 0,1 1)")), "3879, 4326")
})
