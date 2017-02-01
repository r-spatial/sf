check_join = function(x, y) {
	if (inherits(y, "sf"))
		stop("y should be a data.frame; no spatial joins supported yet", .call = FALSE)
}

sf_join = function(g, sf_column) {
	g[[ sf_column ]] = fix_NULL_values(g[[ sf_column ]])
	st_sf(g)
}

#' @name dplyr
#' @export
inner_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	sf_join(NextMethod(), attr(x, "sf_column"))
}

#' @name dplyr
#' @param x see \link[dplyr]{left_join}
#' @param y see \link[dplyr]{left_join}
#' @param by see \link[dplyr]{left_join}
#' @param copy see \link[dplyr]{left_join}
#' @param suffix see \link[dplyr]{left_join}
#' @export
left_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	sf_join(NextMethod(), attr(x, "sf_column"))
}
     
#' @name dplyr
#' @export
right_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	sf_join(NextMethod(), attr(x, "sf_column"))
}
     
#' @name dplyr
#' @export
full_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	sf_join(NextMethod(), attr(x, "sf_column"))
}
     
#' @name dplyr
#' @export
semi_join.sf = function(x, y, by = NULL, copy = FALSE, ...) {
	check_join(x, y)
	sf_join(NextMethod(), attr(x, "sf_column"))
}
     
#' @name dplyr
#' @export
anti_join.sf = function(x, y, by = NULL, copy = FALSE, ...) {
	check_join(x, y)
	sf_join(NextMethod(), attr(x, "sf_column"))
}
