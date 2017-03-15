check_join = function(x, y) {
	if (inherits(y, "sf"))
		stop("y should be a data.frame; no spatial joins supported yet", .call = FALSE)
}

sf_join = function(g, sf_column) {
	g[[ sf_column ]] = st_sfc(fix_NULL_values(g[[ sf_column ]]))
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
#' @param FUN aggregation function, see \link[stats]{aggregate}; in case of multiple matches, if \code{FUN} is defined, attributes of \code{y} will be aggregated using \code{FUN}; else, all combinations of \code{x} and \code{y} are returned.
#' @param suffix length 2 character vector; see \link[base]{merge}
#' @param prepared logical; see \link{st_intersects}
#' @param left logical; if \code{TRUE} carry out left join, else inner join; 
#' see also \link[dplyr]{left_join}
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
#' st_join(a, b, FUN = mean)
#' st_join(a, b, FUN = mean, left = FALSE)
#' @export
st_join = function(x, y, join = st_intersects, FUN, suffix = c(".x", ".y"), 
        prepared = TRUE, left = TRUE) {
    stopifnot(inherits(x, "sf") && inherits(y, "sf"))
    i = join(x, y, prepared = prepared)
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
	if (missing(FUN)) {
		if (left) {
			i = lapply(i, function(x) { if (length(x) == 0) NA_integer_ else x })
    		ix = rep(seq_len(nrow(x)), lengths(i))
		}
		st_sf(cbind(as.data.frame(x)[ix,], y[unlist(i),,drop=FALSE]))
	} else { # aggregate y:
		y = aggregate(y[unlist(i), , drop=FALSE], list(ix), FUN)
    	if (left) {
			xNAs[!is.na(xNAs)] = seq_len(nrow(y))
			st_sf(cbind(as.data.frame(x), y[xNAs, , drop = FALSE]))
		} else # inner:
			st_sf(cbind(as.data.frame(x)[!is.na(xNAs), , drop=FALSE], y))
	}
}
