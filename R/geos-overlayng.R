# nocov start
is_overlayng <- function() {
	pl1 = st_polygon(list(matrix(c(0, 0, 2, 0, 1, 1, 0 ,0), byrow = TRUE, ncol=2)))
	pl2 = st_polygon(list(matrix(c(0, 0.5, 2, 0.5, 1, 1.5, 0, 0.5), byrow = TRUE, ncol = 2)))
	pl3 = st_polygon(list(matrix(c(0, 1.25, 2, 1.25, 1, 2.5, 0, 1.25), byrow = TRUE, ncol = 2)))
	in1 = st_sfc(list(pl1, pl2, pl3))
	correct_geom = st_sfc(list(
		st_polygon(list(matrix(c(0, 2, 1, 0, 0, 0, 1, 0), ncol = 2))),
		st_polygon(list(matrix(c(0, 1, 2, 1.5, 1, 0.5, 0, 0.5, 1.5, 0.5, 0.5, 1, 0.5, 0.5), ncol = 2))),
		st_polygon(list(matrix(c(0, 1, 2, 1.25, 1, 0.75, 0, 1.25, 2.5, 1.25, 1.25, 1.5, 1.25, 1.25), ncol = 2)))))
	out1 = st_difference(in1)
	isTRUE(all.equal(out1[[2]][[1]], correct_geom[[2]][[1]]))
}
# nocov end
