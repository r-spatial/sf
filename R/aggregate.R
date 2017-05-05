#' aggregate an \code{sf} object
#'
#' aggregate an \code{sf} object, possibly union-ing geometries
#' @param x object of class \link{sf}
#' @param by see \link[stats]{aggregate}
#' @param FUN function passed on to \link[stats]{aggregate}, in case \code{ids} was specified and attributes need to be grouped
#' @param ... arguments passed on to \code{FUN}
#' @param do_union logical; should grouped geometries be unioned using \link{st_union}? 
#' @param simplify logical; see \link[stats]{aggregate}
#' @return an \code{sf} object with aggregated attributes and geometries, with an additional grouping variable called \code{Group.1}.
#' @export
aggregate.sf = function(x, by, FUN, ..., do_union = TRUE, simplify = TRUE) {

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


#' Areal-weighted interpolation of polygon data
#' 
#' Areal-weighted interpolation of polygon data
#' @param x object of class \code{sf}, for which we want to aggregate attributes
#' @param to object of class \code{sf} or \code{sfc}, with the target geometries
#' @param extensive logical; if TRUE, the attribute variables are assumed to be spatially extensive (like population) and the sum is preserved, otherwise, spatially intensive (like population density) and the mean is preserved.
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' g = sf:::st_make_grid(nc, n = c(20,10))
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
		stop("aggregate.sf requires geometries in argument to")
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
