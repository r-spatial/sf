#' aggregate an \code{sf} object
#'
#' aggregate an \code{sf} object, possibly union-ing geometries
#' 
#' @note Does not work using the formula notation involving \code{~} defined in \link[stats]{aggregate}.
#' 
#' @param x object of class \link{sf}
#' @param by either a list of grouping vectors with length equal to \code{nrow(x)} (see \link[stats]{aggregate}), or an object of class \code{sf} or \code{sfc} with geometries that are used to generate groupings, using the binary predicate specified by the argument \code{join}
#' @param FUN function passed on to \link[stats]{aggregate}, in case \code{ids} was specified and attributes need to be grouped
#' @param ... arguments passed on to \code{FUN}
#' @param do_union logical; should grouped geometries be unioned using \link{st_union}? See details.
#' @param simplify logical; see \link[stats]{aggregate}
#' @param join logical spatial predicate function to use if \code{by} is a simple features object or geometry; see \link{st_join}
#' @return an \code{sf} object with aggregated attributes and geometries; additional grouping variables having the names of \code{names(ids)} or are named \code{Group.i} for \code{ids[[i]]}; see \link[stats]{aggregate}.
#' @details In case \code{do_union} is \code{FALSE}, \code{aggregate} will simply combine geometries using \link{c.sfg}. When polygons sharing a boundary are combined, this leads to geometries that are invalid; see \url{https://github.com/r-spatial/sf/issues/681}.
#' @aliases aggregate
#' @examples
#' m1 = cbind(c(0, 0, 1, 0), c(0, 1, 1, 0))
#' m2 = cbind(c(0, 1, 1, 0), c(0, 0, 1, 0))
#' pol = st_sfc(st_polygon(list(m1)), st_polygon(list(m2)))
#' set.seed(1985)
#' d = data.frame(matrix(runif(15), ncol = 3))
#' p = st_as_sf(x = d, coords = 1:2)
#' plot(pol)
#' plot(p, add = TRUE)
#' (p_ag1 = aggregate(p, pol, mean))
#' plot(p_ag1) # geometry same as pol
#' # works when x overlaps multiple objects in 'by':
#' p_buff = st_buffer(p, 0.2)
#' plot(p_buff, add = TRUE)
#' (p_ag2 = aggregate(p_buff, pol, mean)) # increased mean of second
#' # with non-matching features
#' m3 = cbind(c(0, 0, -0.1, 0), c(0, 0.1, 0.1, 0))
#' pol = st_sfc(st_polygon(list(m3)), st_polygon(list(m1)), st_polygon(list(m2)))
#' (p_ag3 = aggregate(p, pol, mean))
#' plot(p_ag3)
#' # In case we need to pass an argument to the join function:
#' (p_ag4 = aggregate(p, pol, mean, 
#'      join = function(x, y) st_is_within_distance(x, y, dist = 0.3)))
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
		nrow_diff = nrow(by) - nrow(a)

		if (is.matrix(a[[2]])) # https://github.com/r-spatial/sf/issues/2375
			a = data.frame(a[1], as.data.frame(a[[2]]))

		if(nrow_diff > 0) {
			a_na = a[rep(NA, nrow(by)),] # 'top-up' missing rows
			a_na[a$Group.1,] = a
			a = a_na
		}
		a$Group.1 = NULL # remove
		row.names(a) = row.names(by)
		st_set_geometry(a, st_geometry(by))
	} else {
		crs = st_crs(x)
		lst = lapply(split(st_geometry(x), by), function(y) do.call(c, y))
		geom = do.call(st_sfc, lst[!sapply(lst, is.null)])

		if (do_union)
			geom = st_union(st_set_precision(geom, st_precision(x)), by_feature = TRUE)

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
		st_set_agr(x, agr)
	}
}

#' Areal-weighted interpolation of polygon data
#'
#' Areal-weighted interpolation of polygon data
#' @name interpolate_aw
#' @param x object of class \code{sf}, for which we want to aggregate attributes
#' @param to object of class \code{sf} or \code{sfc}, with the target geometries
#' @param extensive logical; if TRUE, the attribute variables are assumed to be spatially extensive (like population) and the sum is preserved, otherwise, spatially intensive (like population density) and the mean is preserved.
#' @param ... ignored
#' @param keep_NA logical; if \code{TRUE}, return all features in \code{to}, if \code{FALSE} return only those with non-NA values (but with \code{row.names} the index corresponding to the feature in \code{to})
#' @param na.rm logical; if `TRUE` remove features with `NA` attributes from `x` before interpolating
#' @param include_non_intersected logical; for the case when `extensive=FALSE`, when set to `TRUE` divide by the target areas (including non-intersected areas), 
#' when `FALSE` divide by the sum of the source areas.
#' @param weights character; name of variable in `x` (and possibly `to`) with weights for dasymetric mapping; if omitted, areas are used for weighting
#' @details if `extensive` is `TRUE` and `na.rm` is set to `TRUE`, geometries with `NA` attribute values are effectively treated as having zero attribute values.
#'  The `weights` column in `to` is required only when `extensive=FALSE` and `include_non_intersected=TRUE`
#' @examples
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' g = st_make_grid(nc, n = c(10, 5))
#' a1 = st_interpolate_aw(nc["BIR74"], g, extensive = FALSE)
#' sum(a1$BIR74) / sum(nc$BIR74) # not close to one: property is assumed spatially intensive
#' a2 = st_interpolate_aw(nc["BIR74"], g, extensive = TRUE)
#' # verify mass preservation (pycnophylactic) property:
#' sum(a2$BIR74) / sum(nc$BIR74)
#' a1$intensive = a1$BIR74
#' a1$extensive = a2$BIR74
#' \donttest{plot(a1[c("intensive", "extensive")], key.pos = 4)}
#' @export
st_interpolate_aw = function(x, to, extensive, ...) UseMethod("st_interpolate_aw")

#' @export
#' @name interpolate_aw
st_interpolate_aw.sf = function(x, to, extensive, ..., keep_NA = FALSE, na.rm = FALSE,
								include_non_intersected = FALSE, weights = character(0)) {
	if (!inherits(to, "sf") && !inherits(to, "sfc")) {
		to <- try(st_as_sf(to))
		if (inherits(to, "try-error"))
			stop("st_interpolate_aw requires an argument to that has an st_as_sf() method")
	}
	if (isTRUE(na.rm))
		x = x[! apply(is.na(x), 1, any),]

	if (! all_constant(x))
		warning("st_interpolate_aw assumes attributes are constant or uniform over areas of x")
	i = st_intersection(st_geometry(x), st_geometry(to), dimensions = "polygon")
	idx = attr(i, "idx")
	i = st_set_geometry(x[idx[,1], , drop = FALSE], st_geometry(i))
	get_wts = if (length(weights)) {
			stopifnot(is.character(weights), length(weights)==1, !is.null(x[[weights]]), !is.null(i[[weights]]))
			function(x) x[[weights]]
		} else
			st_area

	# https://stackoverflow.com/questions/57767022/how-do-you-use-st-interpolate-aw-with-polygon-layers-that-legitimately-include-p
	gc = which(st_is(i, "GEOMETRYCOLLECTION"))
	# i[gc] = st_collection_extract(i[gc], "POLYGON") ## breaks if there are several POLYGONs in a GC
	if (length(gc)) {
		g = st_geometry(i)
		g[gc] = do.call(c, lapply(g[gc,], function(x) st_sfc(st_union(st_collection_extract(x, "POLYGON")))))
		st_geometry(i) = g
	}
	two_d = which(st_dimension(i) == 2)
	if (any(two_d)) {
		g = st_geometry(i)
		g[two_d] = st_cast(g[two_d], "MULTIPOLYGON")
		st_geometry(i) = g
	}

	x_st = st_set_geometry(x, NULL)[idx[,1],, drop=FALSE]   # create st table, remove geom
	if (any(!sapply(x_st, is.numeric)))
		stop("x contains non-numeric column(s)")
	area_i = try(get_wts(i), silent = TRUE)
	if (inherits(area_i, "try-error"))
		area_i <- get_wts(st_make_valid(i)) # work-around for https://github.com/r-spatial/sf/issues/1810
	x_st$...area_st = as.numeric(area_i)

	x_st = if (extensive) {
			# compute source weights:
			x_st$...area_s = as.numeric(get_wts(x))[idx[,1]]
			lapply(x_st, function(v) v * x_st$...area_st / x_st$...area_s)
		} else {
			# compute target area:
			target = if (include_non_intersected) {
						if (length(weights))
							stopifnot(is.data.frame(to) && !is.null(to[[weights]]))
						setNames(as.numeric(get_wts(to)), seq_len(NROW(to)))
					} else
						sapply(split(area_i, idx[, 2]), sum)
			df = data.frame(area = target, idx = as.integer(names(target)))
			x_st$...area_t = merge(data.frame(idx = idx[,2]), df)$area
			lapply(x_st, function(v) v * x_st$...area_st / x_st$...area_t)
		}
	x_st = aggregate(x_st, list(idx[,2]), sum)
	df = if (keep_NA) {
			ix = rep(NA_integer_, length(to))
			ix[x_st$Group.1] = seq_along(x_st$Group.1)
			st_sf(x_st[ix,], geometry = st_geometry(to))
		} else
			st_sf(x_st, geometry = st_geometry(to)[x_st$Group.1], row.names = x_st$Group.1)
	# clean up:
	df$...area_t = df$...area_st = df$...area_s = df$Group.1 = NULL
	st_set_agr(df, "aggregate")
}
