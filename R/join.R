check_join = function(x, y) {
	if (inherits(y, "sf"))
		stop("y should be a data.frame; for spatial joins, use st_join", .call = FALSE)
}

sf_join = function(g, sf_column) {
	g[[ sf_column ]] = st_sfc(g[[ sf_column ]])
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
#' @param ... arguments passed on to the \code{join} function (e.g. \code{prepared}, or a pattern for \link{st_relate})
#' @param left logical; if \code{TRUE} carry out left join, else inner join; 
#' see also \link[dplyr]{left_join}
#' @param largest logical; if \code{TRUE}, return \code{x} features augmented with the fields of \code{y} that have the largest overlap with each of the features of \code{x}; see https://github.com/r-spatial/sf/issues/578
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
#' # example of largest = TRUE:
#' nc <- st_transform(st_read(system.file("shape/nc.shp", package="sf")), 2264)                
#' gr = st_sf(
#'     label = apply(expand.grid(1:10, LETTERS[10:1])[,2:1], 1, paste0, collapse = " "),
#'     geom = st_make_grid(nc))
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
st_join = function(x, y, join = st_intersects, FUN, suffix = c(".x", ".y"), 
		..., left = TRUE, largest = FALSE) {

	stopifnot(inherits(x, "sf") && inherits(y, "sf"))
	if (!missing(FUN)) {
		.Deprecated("aggregate")
		stop("for aggregation/summarising after st_join, see examples in ?st_join")
	}

	i = if (largest) {
		x$.grp_a = seq_len(nrow(x))
		y$.grp_b = seq_len(nrow(y))
		st_intersection(x, y)
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
	st_sf(cbind(as.data.frame(x)[ix,], y[unlist(i), , drop = FALSE]))
}
