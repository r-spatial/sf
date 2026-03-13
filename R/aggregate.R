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

#' Area-weighted interpolation or dasymetric mapping of polygon data
#'
#' Area-weighted interpolation or dasymetric mapping of polygon data
#' @name interpolate_aw
#' @param x object of class \code{sf}, for which we want to aggregate attributes
#' @param to object of class \code{sf} or \code{sfc}, with the target geometries
#' @param extensive logical; if TRUE, the attribute variables are assumed to be spatially extensive (like population) and the sum is preserved, otherwise, spatially intensive (like population density) and the mean is preserved.
#' @param na.rm logical; if `TRUE` remove features with `NA` attributes from `x` before interpolating
#' @param ... ignored
#' @param keep_NA logical; if \code{TRUE}, return all features in \code{to}, if \code{FALSE} return only those with non-NA values (but with \code{row.names} the index corresponding to the feature in \code{to})
#' @param include_non_intersected logical; for the case when `extensive=FALSE`, when set to `TRUE` divide by the target areas (including non-intersected areas), 
#' when `FALSE` divide by the sum of the source areas.
#' @param weights character; name of column in `to` that indicates (extensive) weights, to be used instead of areas, for redistributing attributes in `x`; currently only works for `extensive=TRUE`.
#' @details if `extensive` is `TRUE` and `na.rm` is set to `TRUE`, geometries with `NA` are effectively treated as having zero attribute values. Dasymetric mapping is obtained when `weights` are specified.
#' @examples
#' # example Area-weighted interpolation:
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
#' 
#' # example Dasymetric mapping:
#' # load nr of addresses per 10 km grid cell, to proxy population -> birth density:
#' grd.addr = system.file("gpkg/grd_addr.gpkg", package="sf") |> read_sf()
#' xgrd.addr = grd.addr # copy for plotting
#' xgrd.addr$ones[grd.addr$ones==0] = 1 # so that logz shows finite values
#' \donttest{plot(xgrd.addr, logz=TRUE) # log scale}
#' nc = st_transform(nc, st_crs(grd.addr))
#' # avoid "assumes attributes are constant or uniform over areas" warnings:
#' st_agr(nc) = c(BIR74 = "constant", BIR79 = "constant")
#' st_agr(grd.addr) = c(ones = "constant")
#' # dasymetric mapping
#' bir.grd = st_interpolate_aw(nc[c("BIR74","BIR79")], extensive = TRUE, grd.addr, weights = "ones")
#' xbir.grd = bir.grd # copy for plotting
#' xbir.grd$BIR74[xbir.grd$BIR74 == 0] = 1 # so that logz shows finite values
#' \donttest{plot(xbir.grd["BIR74"], logz = TRUE)}
#' # verify sums:
#' apply(as.data.frame(bir.grd)[1:2], 2, sum)
#' apply(as.data.frame(nc)[c("BIR74", "BIR79")], 2, sum)
#' # compare county-wise:
#' st_agr(bir.grd) = c(BIR74 = "constant")
#' aw = st_interpolate_aw(bir.grd["BIR74"], st_geometry(nc), extensive = TRUE)
#' plot(nc$BIR74, aw$BIR74, log = 'xy', xlab = 'county-value', ylab = 'area-w interpolated')
#' abline(0,1)
#' @export
st_interpolate_aw = function(x, to, extensive, ...) UseMethod("st_interpolate_aw")

#' @export
#' @name interpolate_aw
st_interpolate_aw.sf = function(x, to, extensive, ..., keep_NA = FALSE, na.rm = FALSE,
		include_non_intersected = FALSE, weights = character(0)) {

	if (!inherits(to, "sf") && !inherits(to, "sfc")) {
		to <- try(st_as_sf(to))
		if (inherits(to, "try-error"))
			stop("st_interpolate_aw requires geometries in argument to")
	}
	if (isTRUE(na.rm))
		x = x[! apply(is.na(x), 1, any),]

	if (length(weights)) {
		stopifnot(length(weights) == 1, is.character(weights), inherits(to, "sf"), weights %in% names(to))
		return(dasymetric(x, to[weights], extensive, keep_NA, include_non_intersected))
	}

	if (! all_constant(x))
		warning("st_interpolate_aw assumes attributes are constant or uniform over areas of x")
	i = st_intersection(st_geometry(x), st_geometry(to), dimensions = "polygon")
	idx = attr(i, "idx")

	# https://stackoverflow.com/questions/57767022/how-do-you-use-st-interpolate-aw-with-polygon-layers-that-legitimately-include-p
	gc = which(st_is(i, "GEOMETRYCOLLECTION"))
	# i[gc] = st_collection_extract(i[gc], "POLYGON") ## breaks if there are several POLYGONs in a GC
	i[gc] = do.call(c, lapply(i[gc], function(x) st_sfc(st_union(st_collection_extract(x, "POLYGON")))))
	two_d = which(st_dimension(i) == 2)
	i[two_d] = st_cast(i[two_d], "MULTIPOLYGON")

	x_st = st_set_geometry(x, NULL)[idx[,1],, drop=FALSE]   # create st table, remove geom
	if (any(!sapply(x_st, is.numeric)))
		stop("x contains non-numeric column(s)")
	area_i = try(st_area(i), silent = TRUE)
	if (inherits(area_i, "try-error"))
		area_i <- st_area(st_make_valid(i)) # work-around for https://github.com/r-spatial/sf/issues/1810
	x_st$...area_st = unclass(area_i)

	x_st = if (extensive) {
			# compute area_s:
			x_st$...area_s = unclass(st_area(x))[idx[,1]]
			lapply(x_st, function(v) v * x_st$...area_st / x_st$...area_s)
		} else {
			# compute target area:
			target = if (include_non_intersected)
						setNames(as.numeric(st_area(to)), seq_len(NROW(to))) # use all of "to"
					else
						sapply(split(area_i, idx[, 2]), sum) # sum "to" parts intersecting "x"
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

dasymetric = function(x, to, extensive, keep_NA, include_non_intersected) {
	stopifnot(isTRUE(extensive), !keep_NA, !include_non_intersected) # later...
	stopifnot(length(to) == 2)
	if (!inherits(to[[2]], "sfc"))
		to = to[2:1] # swap: geom last
	i = st_intersection(st_geometry(x), st_geometry(to), dimensions = "polygon")
	idx = attr(i, "idx")
	st_geometry(x) = NULL
	stopifnot(!("...x_s" %in% names(x))) # avoid overwrite
	x = x[idx[,1], , drop=FALSE] # x attributes corresponding to each area of i
	i = st_set_geometry(x[idx[,1], , drop = FALSE], i) # add geometry

	# https://stackoverflow.com/questions/57767022/how-do-you-use-st-interpolate-aw-with-polygon-layers-that-legitimately-include-p
	# i[gc] = st_collection_extract(i[gc], "POLYGON") ## breaks if there are several POLYGONs in a GC
	gc = which(st_is(i, "GEOMETRYCOLLECTION"))
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

	x$...x_st = st_interpolate_aw(to, i, extensive = extensive)[[1]] # distribute weights over the intersection geometries
	# x$...x_st = to[[1]][idx[,2]]
	# x_s = to[[1]][idx[,2]]
	# split according to source regions:
	# copy over idx[,2], as split() will rearrange records
	x$...idx2 = idx[,2]
	spl = split(x, idx[,1])
	# reconstruct idx2:
	idx2 = do.call(c, lapply(spl, function(x) x$...idx2))
	# for each of the source regions, compute weighted sum
	spl = lapply(spl, function(u) {
			w = if ((s <- sum(u$...x_st)) == 0)
					NA_real_
				else
					u$...x_st / s
			u$...x_st = u$...idx2 = NULL # remove
			as.data.frame(lapply(u, function(v) v * w))
		})
	x = do.call(rbind, spl)
	# sum over the target regions:
	# x = aggregate(x, list(idx[,2]), sum)
	x = aggregate(x, list(idx2), sum)
	df = if (keep_NA) {
			ix = rep(NA_integer_, length(to))
			ix[x$Group.1] = seq_along(x$Group.1)
			st_sf(x[ix,], geometry = st_geometry(to))
		} else
			st_sf(x, geometry = st_geometry(to)[x$Group.1], row.names = x$Group.1)
	# clean up:
	df$Group.1 = NULL
	st_set_agr(df, "aggregate")
}
