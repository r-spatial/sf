check_join = function(x, y) {
	if (inherits(y, "sf"))
		stop("y should be a data.frame; no spatial joins supported yet", .call = FALSE)
}

sf_join = function(g, sf_column) {
	g[[ sf_column ]] = st_sfc(fix_NULL_values(g[[ sf_column ]]))
	class(g) = setdiff(class(g), "sf")
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


#' spatial left or inner join
#'
#' spatial left or inner join
#' @param x object of class \code{sf}
#' @param y object of class \code{sf}
#' @param join geometry predicate function with the same profile as \link{st_intersects}; see details
#' @param FUN deprecated;
#' @param suffix length 2 character vector; see \link[base]{merge}
#' @param prepared logical; see \link{st_intersects}
#' @param left logical; if \code{TRUE} carry out left join, else inner join; 
#' see also \link[dplyr]{left_join}
#' @param ... arguments passed on to the \code{join} function (e.g. a pattern for \link{st_relate})
#' @details alternative values for argument \code{join} are: \link{st_disjoint}
#' \link{st_touches} \link{st_crosses} \link{st_within} \link{st_contains}
#' \link{st_overlaps} \link{st_covers} \link{st_covered_by} \link{st_equals} or
#' \link{st_equals_exact}, or user-defined functions of the same profile
#' @return an object of class \code{sf}, joined based on geometry
#' @examples
#' a = st_sf(a = 1:3,
#'  geom = st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3))))
#' b = st_sf(a = 11:14,
#'  geom = st_sfc(st_point(c(10,10)), st_point(c(2,2)), st_point(c(2,2)), st_point(c(3,3))))
#' st_join(a, b)
#' st_join(a, b, left = FALSE)
#' # two ways to aggregate y's attribute values outcome over x's geometries:
#' st_join(a, b) %>% aggregate(list(.$a.x), mean)
#' library(dplyr)
#' st_join(a, b) %>% group_by(a.x) %>% summarise(mean(a.y))
#' @export
st_join = function(x, y, join = st_intersects, FUN, suffix = c(".x", ".y"), 
		prepared = TRUE, left = TRUE, ...) {
	stopifnot(inherits(x, "sf") && inherits(y, "sf"))
	if (!missing(FUN)) {
		.Deprecated("aggregate")
		stop("for aggregation/summarising after st_join, see examples in ?st_join")
	}
	i = join(x, y, prepared = prepared, ...)
	st_geometry(y) = NULL
	which.x = which(names(x) %in% names(y))
	which.y = which(names(y) %in% names(x))
	if (length(which.x))
		names(x)[which.x] = paste0(names(x)[which.x], suffix[1])
	if (length(which.y))
		names(y)[which.y] = paste0(names(y)[which.y], suffix[2])
	ix = rep(seq_len(nrow(x)), lengths(i))
	xNAs = seq_len(nrow(x))
	xNAs[sapply(i, function(x) length(x)==0)] = NA_integer_
	if (left) { # fill NA y values when no match:
		i = lapply(i, function(x) { if (length(x) == 0) NA_integer_ else x })
		ix = rep(seq_len(nrow(x)), lengths(i))
	}
	st_sf(cbind(as.data.frame(x)[ix,], y[unlist(i),,drop=FALSE]))
}
