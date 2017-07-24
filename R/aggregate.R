#' aggregate an \code{sf} object
#'
#' aggregate an \code{sf} object, possibly union-ing geometries
#' @param x object of class \link{sf}
#' @param by either a list of grouping elements, each as long as the variables in the data frame \code{x} (see \link[stats]{aggregate}), or an object of class \code{sf} or \code{sfc}, the geometries of which are used to find aggregation groups of \code{x} by using function \code{join}
#' @param FUN function passed on to \link[stats]{aggregate}, in case \code{ids} was specified and attributes need to be grouped
#' @param ... arguments passed on to \code{FUN}
#' @param do_union logical; should grouped geometries be unioned using \link{st_union}?
#' @param simplify logical; see \link[stats]{aggregate}
#' @param join logical spatial predicate function to use if \code{by} is a simple features object or geometry; see \link{st_join}
#' @return an \code{sf} object with aggregated attributes and geometries; additional grouping variables having the names of \code{names(ids)} or are named \code{Group.i} for \code{ids[[i]]}; see \link[stats]{aggregate}.
#' @aliases aggregate
#' @examples
#' m1 = cbind(c(0, 0, 1, 0), c(0, 1, 1, 0))
#' m2 = cbind(c(0, 1, 1, 0), c(0, 0, 1, 0))
#' pol = st_sfc(st_polygon(list(m1)), st_polygon(list(m2)))
#' library(sf)
#' set.seed(1985)
#' d = data.frame(matrix(runif(15), ncol = 3))
#' p = st_as_sf(x = d, coords = 1:2)
#' plot(pol)
#' plot(p, add = TRUE)
#' (p_ag = aggregate(p, pol, mean))
#' plot(p_ag) # geometry same as pol
#' # works when x overlaps multiple objects in 'by':
#' p_buff = st_buffer(p, 0.2)
#' plot(p_buff, add = TRUE)
#' aggregate(p_buff, pol, mean) # increased mean of second
#' @export
aggregate.sf = function(x, by, FUN, ..., do_union = TRUE, simplify = TRUE,
		join = st_intersects) {

	if (inherits(by, "sf") || inherits(by, "sfc")) {
		if (inherits(by, "sfc"))
			by = st_sf(by)
		i = join(st_geometry(by), st_geometry(x))
		st_geometry(x) = NULL
		# dispatch to stats::aggregate:
		a = aggregate(x[unlist(i), , drop = FALSE],
			list(rep(seq_len(nrow(by)), lengths(i))), FUN, ...)
		a$Group.1 = NULL
		st_set_geometry(a, st_geometry(by)[lengths(i) > 0])
	} else {
		crs = st_crs(x)
		lst = lapply(split(st_geometry(x), by), function(y) do.call(c, y))
		geom = do.call(st_sfc, lst[!sapply(lst, is.null)])

		if (do_union)
			geom = st_union(geom, by_feature = TRUE)
		st_geometry(x) = NULL
		x = aggregate(x, by, FUN, ..., simplify = simplify)
		st_geometry(x) = geom # coerces to sf
		st_crs(x) = crs

		# now set agr:
		geoms = which(vapply(x, function(vr) inherits(vr, "sfc"), TRUE))
		agr_names = names(x)[-geoms]
		agr = rep("aggregate", length(agr_names))
		names(agr) = agr_names
		# which ones are identity variables?
		n = if (!is.null(names(by)))
			names(by)
		else
			paste0("Group.", seq_along(by))
		agr[n] = "identity"
		st_agr(x) = agr

		x
	}
}


#' Areal-weighted interpolation of polygon data
#'
#' Areal-weighted interpolation of polygon data
#' @param x object of class \code{sf}, for which we want to aggregate attributes
#' @param to object of class \code{sf} or \code{sfc}, with the target geometries
#' @param extensive logical; if TRUE, the attribute variables are assumed to be spatially extensive (like population) and the sum is preserved, otherwise, spatially intensive (like population density) and the mean is preserved.
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' g = st_make_grid(nc, n = c(20,10))
#' a1 = st_interpolate_aw(nc["BIR74"], g, extensive = FALSE)
#' sum(a1$BIR74) / sum(nc$BIR74) # not close to one: property is assumed spatially intensive
#' a2 = st_interpolate_aw(nc["BIR74"], g, extensive = TRUE)
#' sum(a2$BIR74) / sum(nc$BIR74)
#' a1$intensive = a1$BIR74
#' a1$extensive = a2$BIR74
#' plot(a1[c("intensive", "extensive")])
#' @export
st_interpolate_aw = function(x, to, extensive) {
	if (!inherits(to, "sf") && !inherits(to, "sfc"))
		stop("st_interpolate_aw requires geometries in argument to")
	i = st_intersection(st_geometry(x), st_geometry(to))
	idx = attr(i, "idx")
	i = st_cast(i, "MULTIPOLYGON")
	x$...area_s = unclass(st_area(x))
	st_geometry(x) = NULL # sets back to data.frame
	x = x[idx[,1], ]			# create st table
	x$...area_st = unclass(st_area(i))
	x$...area_t = unclass(st_area(to)[idx[,2]])
	x = if (extensive)
		lapply(x, function(v) v * x$...area_st / x$...area_s)
	else
		lapply(x, function(v) v * x$...area_st / x$...area_t)
	x = aggregate(x, list(idx[,2]), sum)
	df = st_sf(x, geometry = st_geometry(to)[x$Group.1])
	df$...area_t = df$...area_st = df$...area_s = NULL
	if (! all_constant(df))
		warning("st_interpolate_aw assumes attributes are constant over areas of x")
	st_agr(df) = "aggregate"
	df
}
