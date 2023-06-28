test_that("point grob constructors work", {
	p1 <- st_point(c(0, 1))
	p2 <- st_point(c(2, 3))
	p3 <- st_multipoint(matrix(11:20, ncol = 2))
	p4 <- st_sfc(list(p1, p2, p3))
	g1 <- st_as_grob(p1)
	expect_s3_class(g1, c('points', 'grob'))
	g2 <- st_as_grob(p4, gp = grid::gpar(col = c('red', 'green', 'blue')))
	expect_s3_class(g2, c('points', 'grob'))
	expect_equal(as.numeric(g2$x), unname(rbind(p1, p2, p3)[,1]))
	expect_equal(g2$gp$col, rep(c('red', 'green', 'blue'), c(1, 1, 5)))
})

test_that("linestring grob construction work", {
	lines <- list(
		matrix(11:20, ncol = 2),
		matrix(21:30, ncol = 2),
		matrix(31:50, ncol = 2)
	)
	l1 <- st_linestring(lines[[1]])
	l2 <- st_multilinestring(lines[2:3])
	l3 <- st_sfc(list(l1, l2))
	g1 <- st_as_grob(l1)
	expect_s3_class(g1, c('lines', 'grob'))
	g2 <- st_as_grob(l3, gp = grid::gpar(lwd = c(2, 4)))
	expect_s3_class(g2, c('lines', 'grob'))
	expect_equal(as.numeric(g2$x), do.call(rbind, lines)[, 1])
	expect_equal(g2$gp$lwd, rep(c(2, 4), c(1, 2)))
	expect_equal(g2$id.lengths, c(5, 5, 10))
})

holed_rect <- function(x0, y0, width, height, hole) {
	outer <- cbind(
		c(x0 - width/2, x0 + width/2, x0 + width/2, x0 - width/2, x0 - width/2),
		c(y0 - height/2, y0 - height/2, y0 + height/2, y0 + height/2, y0 - height/2)
	)
	inner <- outer
	inner[,1] <- (inner[,1] - x0) * hole + x0
	inner[,2] <- (inner[,2] - y0) * hole + y0
	list(outer, inner)
}
test_that("polygon grob construction work", {
	polys <- list(
		holed_rect(0, 0, 1, 1, 0.5),
		holed_rect(10, 5, 5, 1, 0.25),
		holed_rect(-3, -10, 4, 10, 0.7)
	)
	p1 <- st_polygon(polys[[1]])
	p2 <- st_multipolygon(polys[2:3])
	p3 <- st_sfc(list(p1, p2))
	g1 <- st_as_grob(p1)
	expect_s3_class(g1, c('pathgrob', 'grob'))
	g2 <- st_as_grob(p3, gp = grid::gpar(fill = c('red', 'blue')))
	if (getRversion() <  as.numeric_version("3.6")) {
		expect_s3_class(g2, 'gList')
		expect_equal(g2[[1]]$gp$fill, 'red')
		expect_equal(g2[[2]]$gp$fill, 'blue')
	} else {
		expect_s3_class(g2, c('pathgrob', 'grob'))
		coords <- do.call(rbind, unlist(polys, recursive = FALSE))
		expect_equal(as.numeric(g2$x), coords[, 1])
		expect_equal(g2$id.lengths, rep(5, 6))
		expect_equal(g2$pathId.lengths, rep(10, 3))
		expect_equal(g2$gp$fill, c('red', 'blue', 'blue'))
	}
})

test_that("mixed sfc grob construction works", {
	p1 <- st_point(c(0, 1))
	p2 <- st_multipoint(matrix(11:20, ncol = 2))
	l1 <- st_linestring(matrix(21:30, ncol = 2))
	p3 <- st_polygon(holed_rect(0, 0, 1, 1, 0.5))
	sfc <- st_sfc(list(p1, p2, l1, p3))
	g1 <- st_as_grob(sfc, pch = 1:4, gp = grid::gpar(col = 'blue', fill = c('red', 'red', 'blue', 'green')))
	expect_s3_class(g1[[1]], c('points', 'grob'))
	expect_s3_class(g1[[2]], c('points', 'grob'))
	expect_s3_class(g1[[3]], c('lines', 'grob'))
	expect_s3_class(g1[[4]], c('pathgrob', 'grob'))
	expect_equal(g1[[1]]$pch, 1)
	expect_equal(g1[[2]]$pch, 2)
	expect_null(g1[[3]]$pch)
	expect_null(g1[[4]]$pch)
	expect_equal(g1[[1]]$gp$col, 'blue')
	expect_equal(g1[[2]]$gp$col, 'blue')
	expect_equal(g1[[3]]$gp$col, 'blue')
	expect_equal(g1[[4]]$gp$col, 'blue')
	expect_equal(g1[[1]]$gp$fill, 'red')
	expect_equal(g1[[2]]$gp$fill, 'red')
	expect_equal(g1[[3]]$gp$fill, 'blue')
	expect_equal(g1[[4]]$gp$fill, 'green')
})
