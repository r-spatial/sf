check_join = function(x, y) {
	if (inherits(y, "sf"))
		stop("y should not have class sf; for spatial joins, use st_join", call. = FALSE)
}

sf_join = function(g, sf_column, suffix_x = ".x") {
	if (!(sf_column %in% names(g))) {
		sf_column = paste0(sf_column, suffix_x)
		stopifnot(sf_column %in% names(g))
	}
	attr(g[[ sf_column ]], "bbox") = NULL # remove, so that st_sfc() recomputes:
	g[[ sf_column ]] = st_sfc(g[[ sf_column ]])
	st_sf(g, sf_column_name = sf_column)
}

#' @name tidyverse
#' @inheritParams dplyr::inner_join
inner_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	class(x) = setdiff(class(x), "sf")
	sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
}

#' @name tidyverse
left_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	class(x) = setdiff(class(x), "sf")
	sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
}

#' @name tidyverse
right_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	class(x) = setdiff(class(x), "sf")
	sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
}

#' @name tidyverse
full_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	class(x) = setdiff(class(x), "sf")
	sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
}

#' @name tidyverse
semi_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	class(x) = setdiff(class(x), "sf")
	sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
}

#' @name tidyverse
anti_join.sf = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
	check_join(x, y)
	class(x) = setdiff(class(x), "sf")
	sf_join(NextMethod(), attr(x, "sf_column"), suffix[1])
}


#' spatial join, spatial filter
#'
#' spatial join, spatial filter
#' @name st_join
#' @export
st_join = function(x, y, join, ...) UseMethod("st_join")

#' @name st_join
#' @param x object of class \code{sf}
#' @param y object of class \code{sf}
#' @param join geometry predicate function with the same profile as \link{st_intersects}; see details
#' @param suffix length 2 character vector; see \link[base]{merge}
#' @param ... for \code{st_join}: arguments passed on to the \code{join} function or to \code{st_intersection} when \code{largest} is \code{TRUE}; for \code{st_filter} arguments passed on to the \code{.predicate} function, e.g. \code{prepared}, or a pattern for \link{st_relate}
#' @param left logical; if \code{TRUE} return the left join, otherwise an inner join; see details.
#' see also \link[dplyr:mutate-joins]{left_join}
#' @param largest logical; if \code{TRUE}, return \code{x} features augmented with the fields of \code{y} that have the largest overlap with each of the features of \code{x}; see https://github.com/r-spatial/sf/issues/578
#' 
#' @details alternative values for argument \code{join} are:
#' \itemize{
#'   \item \link{st_contains_properly}
#'   \item \link{st_contains}
#'   \item \link{st_covered_by}
#'   \item \link{st_covers}
#'   \item \link{st_crosses}
#'   \item \link{st_disjoint}
#'   \item \link{st_equals_exact}
#'   \item \link{st_equals}
#'   \item \link{st_is_within_distance}
#'   \item \link{st_nearest_feature}
#'   \item \link{st_overlaps}
#'   \item \link{st_touches}
#'   \item \link{st_within}
#'   \item any user-defined function of the same profile as the above
#' }
#' A left join returns all records of the \code{x} object with \code{y} fields for non-matched records filled with \code{NA} values; an inner join returns only records that spatially match.
#' 
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
#' # example of largest = TRUE:
#' nc <- st_transform(st_read(system.file("shape/nc.shp", package="sf")), 2264)                
#' gr = st_sf(
#'     label = apply(expand.grid(1:10, LETTERS[10:1])[,2:1], 1, paste0, collapse = " "),
#'     geom = st_make_grid(st_as_sfc(st_bbox(nc))))
#' gr$col = sf.colors(10, categorical = TRUE, alpha = .3)
#' # cut, to check, NA's work out:
#' gr = gr[-(1:30),]
#' nc_j <- st_join(nc, gr, largest = TRUE)
#' # the two datasets:
#' opar = par(mfrow = c(2,1), mar = rep(0,4))
#' plot(st_geometry(nc_j))
#' plot(st_geometry(gr), add = TRUE, col = gr$col)
#' text(st_coordinates(st_centroid(gr)), labels = gr$label)
#' # the joined dataset:
#' plot(st_geometry(nc_j), border = 'black', col = nc_j$col)
#' text(st_coordinates(st_centroid(nc_j)), labels = nc_j$label, cex = .8)
#' plot(st_geometry(gr), border = 'green', add = TRUE)
#' par(opar)
#' @export
st_join.sf = function(x, y, join = st_intersects, ..., suffix = c(".x", ".y"), 
		left = TRUE, largest = FALSE) {

	if (!inherits(y, "sf"))
		stop("second argument should be of class sf: maybe revert the first two arguments?") # nocov

	i = if (largest) {
		x$.grp_a = seq_len(nrow(x))
		y$.grp_b = seq_len(nrow(y))
		st_intersection(x, y, ...)
	} else 
		join(x, y, ...)

	st_geometry(y) = NULL
	which.x = which(names(x) %in% names(y))
	which.y = which(names(y) %in% names(x))
	if (length(which.x))
		names(x)[which.x] = paste0(names(x)[which.x], suffix[1])
	if (length(which.y))
		names(y)[which.y] = paste0(names(y)[which.y], suffix[2])

	# create match index ix & i:
	if (largest) {
		x$.grp_a = y$.grp_b = NULL # clean up
		i$.size = if (all(st_dimension(i) < 2)) st_length(i) else st_area(i)
		l = lapply(split(i, i$.grp_a), function(x) x[which.max(x$.size), ]$.grp_b)
		ix = as.integer(names(l)) # non-empty x features
		i = unlist(l) # matching largest y feature
		if (left) { # fill NA's
			idx = rep(NA_integer_, nrow(x)) # all x features
			idx[ix] = i
			ix = seq_len(nrow(x))
			i = idx
		}
	} else {
		if (left) # fill NA y values when no match:
			i = lapply(i, function(x) { if (length(x) == 0) NA_integer_ else x })
		ix = rep(seq_len(nrow(x)), lengths(i))
	}
	if (inherits(x, "tbl_df") && requireNamespace("dplyr", quietly = TRUE))
		st_sf(dplyr::bind_cols(x[ix,], y[unlist(i), , drop = FALSE]))
  	else
		st_sf(cbind(as.data.frame(x)[ix,], y[unlist(i), , drop = FALSE]))	
}

#' @export
#' @name st_join
st_filter = function(x, y, ...) UseMethod("st_filter")

#' @export
#' @name st_join
#' @param .predicate geometry predicate function with the same profile as \link{st_intersects}; see details
st_filter.sf = function(x, y, ..., .predicate = st_intersects) {
	if (!requireNamespace("dplyr", quietly = TRUE))
		stop("dplyr is not installed: install first?")

    dplyr::filter(x, lengths(.predicate(x, y, ...)) > 0) # will call filter.sf
}
