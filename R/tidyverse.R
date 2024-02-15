## dplyr methods: ------
#group_map.sf <- function(.tbl, .f, ...) {
#	 st_as_sf(NextMethod()) # nocov
#}

# This is currently only used in `bind_rows()` and `bind_cols()`
# because sf overrides all default implementations
dplyr_reconstruct.sf = function(data, template) {
	sfc_name = attr(template, "sf_column")
	if (inherits(template, "tbl_df"))
		data = dplyr::as_tibble(data)
	# Return a bare data frame is the geometry column is no longer there
	if (!sfc_name %in% names(data))
		return(data)
	prec = st_precision(template)
	crs = st_crs(template)
	st_as_sf(
		data,
		sf_column_name = sfc_name,
		crs = crs,
		precision = prec
	)
}

#' Tidyverse methods for sf objects
#'
#' Tidyverse methods for sf objects. Geometries are sticky, use \link{as.data.frame} to let \code{dplyr}'s own methods drop them.
#' Use these methods after loading the tidyverse package with the generic (or after loading package tidyverse).
#' @param .data data object of class \link{sf}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name tidyverse
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc = read_sf(system.file("shape/nc.shp", package="sf"))
#'  nc %>% filter(AREA > .1) %>% plot()
#'  # plot 10 smallest counties in grey:
#'  st_geometry(nc) %>% plot()
#'  nc %>% select(AREA) %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
#'  title("the ten counties with smallest area")
#'  nc2 <- nc %>% mutate(area10 = AREA/10)
#'  nc %>% slice(1:2)
#' }
filter.sf <- function(.data, ..., .dots) {
	agr = st_agr(.data)
	class(.data) <- setdiff(class(.data), "sf")
	.re_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"), agr)
}

#' @name tidyverse
#' @examples
#' # plot 10 smallest counties in grey:
#' if (require(dplyr, quietly = TRUE)) {
#'  st_geometry(nc) %>% plot()
#'  nc %>% select(AREA) %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
#'  title("the ten counties with smallest area")
#' }
arrange.sf <- function(.data, ..., .dots) {
	sf_column_name = attr(.data, "sf_column")
	class(.data) = setdiff(class(.data), "sf")
	st_as_sf(NextMethod(), sf_column_name = sf_column_name)
}

#' @name tidyverse
#' @param add see corresponding function in dplyr
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#'  nc %>% group_by(area_cl) %>% class()
#' }
group_by.sf <- function(.data, ..., add = FALSE) {
	sf_column_name = attr(.data, "sf_column")
	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(NextMethod(), sf_column_name = sf_column_name)
}

#' @name tidyverse
ungroup.sf <- function(x, ...) {
	sf_column_name = attr(x, "sf_column")
	class(x) <- setdiff(class(x), "sf")
	st_as_sf(NextMethod(), sf_column_name = sf_column_name)
}

#' @name tidyverse
rowwise.sf <- function(x, ...) {
	sf_column_name = attr(x, "sf_column")
	class(x) <- setdiff(class(x), "sf")
	st_as_sf(NextMethod(), sf_column_name = sf_column_name)
}

.re_sf = function(x, sf_column_name, agr, geom = NULL) {
	stopifnot(!inherits(x, "sf"), !missing(sf_column_name), !missing(agr))
	# non-geom attribute names
	att = names(x)[!sapply(x, inherits, what = "sfc")]
	agr = setNames(agr[att], att) # NA's new columns
	if (!is.null(geom)) {
		stopifnot(length(geom) == nrow(x))
		x[[ sf_column_name ]] = geom
	}
	structure(x,
		sf_column = sf_column_name,
		agr = agr,
		class = c("sf", class(x)))
}

#' @name tidyverse
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc2 <- nc %>% mutate(area10 = AREA/10)
#' }
mutate.sf <- function(.data, ..., .dots) {
	#st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
	agr = st_agr(.data)
	sf_column_name = attr(.data, "sf_column")
	class(.data) <- setdiff(class(.data), "sf")
	.re_sf(NextMethod(), sf_column_name = sf_column_name, agr)
}


#' @name tidyverse
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc %>% transmute(AREA = AREA/10) %>% class()
#' }
transmute.sf <- function(.data, ..., .dots) {
	sf_column_name = attr(.data, "sf_column")
	agr = st_agr(.data)
	geom = st_geometry(.data)
	class(.data) = setdiff(class(.data), "sf")
	.re_sf(NextMethod(), sf_column_name = sf_column_name, agr, geom)
}

#' @name tidyverse
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc %>% select(SID74, SID79) %>% names()
#'  nc %>% select(SID74, SID79) %>% class()
#' }
#' @details \code{select} keeps the geometry regardless whether it is selected or not; to deselect it, first pipe through \code{as.data.frame} to let dplyr's own \code{select} drop it.
select.sf <- function(.data, ...) {

	if (!requireNamespace("tidyselect", quietly = TRUE))
		stop("tidyselect required: install that first") # nocov
	loc = tidyselect::eval_select(quote(c(...)), .data)

	sf_column = attr(.data, "sf_column")
	sf_column_loc = match(sf_column, names(.data))

	if (length(sf_column_loc) != 1 || is.na(sf_column_loc))
		stop("internal error: can't find sf column") # nocov

	agr = st_agr(.data)
	#vars = names(.data)[setdiff(loc, sf_column_loc)] # see #1886, change into:
	lloc = loc
	if (sf_column_loc %in% loc)
		lloc = lloc[loc != sf_column_loc]
	vars = names(.data)[lloc]

	sf_column_loc_loc = match(sf_column_loc, loc)
	if (is.na(sf_column_loc_loc)) {
		# The sf column was subsetted out, select it back in
		new_agr = setNames(agr[vars], names(loc))
		loc = c(loc, sf_column_loc)
		names(loc)[[length(loc)]] = sf_column
	} else {
		# The sf column was not subsetted out but it might have been renamed
		sf_column = names(loc[sf_column_loc_loc])
		new_agr = setNames(agr[vars], setdiff(names(loc), sf_column))
	}

	ret = .data
	class(ret) = setdiff(class(ret), "sf")
	ret = ret[loc]
	names(ret) = names(loc)

	st_set_agr(st_as_sf(ret, sf_column_name = sf_column), new_agr)
}


#' @name tidyverse
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc2 <- nc %>% rename(area = AREA)
#' }
rename.sf <- function(.data, ...) {

	if (!requireNamespace("tidyselect", quietly = TRUE))
		stop("tidyselect required: install that first") # nocov
	loc = tidyselect::eval_rename(quote(c(...)), .data)

	sf_column = attr(.data, "sf_column")
	sf_column_loc = match(sf_column, names(.data))

	if (length(sf_column_loc) != 1 || is.na(sf_column_loc))
		stop("internal error: can't find sf column") # nocov

	agr = st_agr(.data)
	agr_loc = match(names(agr), setdiff(names(.data), sf_column))

	if (anyNA(agr_loc))
		stop("internal error: can't find `agr` columns") # nocov

	vars_loc = loc[loc %in% agr_loc]

	# https://github.com/r-spatial/sf/issues/1472
	# but only fixes for the single sfc column case
	sfcs = which(sapply(.data, inherits, "sfc"))
	if (length(vars_loc) == 1 && any(vars_loc > sfcs[1])) {
		w = which(vars_loc > sfcs)
		vars_loc[w] = vars_loc[w] - 1
	}

	names(agr)[vars_loc] = names(vars_loc)

	sf_column_loc_loc = match(sf_column_loc, loc)
	if (!is.na(sf_column_loc_loc))
		sf_column = names(loc[sf_column_loc_loc])

	ret = .data
	class(ret) = setdiff(class(ret), "sf")
	names(ret)[loc] = names(loc)

	st_set_agr(st_as_sf(ret, sf_column_name = sf_column), agr)
}

#' @name tidyverse
#' @param .fn,.cols see original docs
rename_with.sf = function(.data, .fn, .cols, ...) {
	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install that first") # nocov
	.fn = rlang::as_function(.fn)
	is_tibble = inherits(.data, "tbl")
	
	sf_column = attr(.data, "sf_column")
	sf_column_loc = match(sf_column, names(.data))
	
	if (length(sf_column_loc) != 1 || is.na(sf_column_loc))
		stop("internal error: can't find sf column") # nocov
	
	agr = st_agr(.data)
	
	.data = as.data.frame(.data)
	ret = if (missing(.cols)) {
		if (!requireNamespace("tidyselect", quietly = TRUE)) {
			stop("tidyselect required: install that first") # nocov
		}
		dplyr::rename_with(
			.data = .data,
			.fn = .fn,
			.cols = tidyselect::everything(), 
			...
		)
	} else {
		dplyr::rename_with(
			.data = .data,
			.fn = .fn,
			.cols = {{ .cols }}, 
			...
		)
	}
	if (is_tibble)
		ret = dplyr::as_tibble(ret)
	ret = st_as_sf(ret, sf_column_name = names(ret)[sf_column_loc])
	
	names(agr) = .fn(names(agr), ...)
	st_agr(ret) = agr
	ret
}


#' @name tidyverse
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc %>% slice(1:2)
#' }
slice.sf <- function(.data, ..., .dots) {
	class(.data) <- setdiff(class(.data), "sf")
	sf_column <- attr(.data, "sf_column")
	st_as_sf(NextMethod(), sf_column_name = sf_column)
}


#' @name tidyverse
#' @aliases summarise
#' @param do_union logical; in case \code{summary} does not create a geometry column, should geometries be created by unioning using \link{st_union}, or simply by combining using \link{st_combine}? Using \link{st_union} resolves internal boundaries, but in case of unioning points, this will likely change the order of the points; see Details.
#' @param is_coverage logical; if \code{do_union} is \code{TRUE}, use an optimized algorithm for features that form a polygonal coverage (have no overlaps)
#' @return an object of class \link{sf}
#' @details
#' In case one or more of the arguments (expressions) in the \code{summarise} call creates a geometry list-column, the first of these will be the (active) geometry of the returned object. If this is not the case, a geometry column is created, depending on the value of \code{do_union}.
#'
#' In case \code{do_union} is \code{FALSE}, \code{summarise} will simply combine geometries using \link{c.sfg}. When polygons sharing a boundary are combined, this leads to geometries that are invalid; see for instance \url{https://github.com/r-spatial/sf/issues/681}.
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#'  nc.g <- nc %>% group_by(area_cl)
#'  nc.g %>% summarise(mean(AREA))
#'  nc.g %>% summarise(mean(AREA)) %>% plot(col = grey(3:6 / 7))
#'  nc %>% as.data.frame %>% summarise(mean(AREA))
#' }
summarise.sf <- function(.data, ..., .dots, do_union = TRUE, is_coverage = FALSE) {
	sf_column = attr(.data, "sf_column")
	precision = st_precision(.data)
	crs = st_crs(.data)
	geom = st_geometry(.data)
	class(.data) = setdiff(class(.data), "sf")
	ret = NextMethod()
	if (!missing(do_union))
		ret$do_union = NULL
	if (!missing(is_coverage))
		ret$is_coverage = NULL

	if (! any(sapply(ret, inherits, what = "sfc"))) {
		geom = if (inherits(.data, "grouped_df") || inherits(.data, "grouped_dt")) {
				if (!requireNamespace("dplyr", quietly = TRUE))
					stop("dplyr required: install that first") # nocov
				i = dplyr::group_indices(.data)
				# geom = st_geometry(.data)
				geom = if (do_union)
						lapply(sort(unique(i)), function(x) {
							if (x == 1)
								st_union(geom[i == x], is_coverage = is_coverage)
							else
								suppressMessages(st_union(geom[i == x], is_coverage = is_coverage))
						})
					else
						lapply(sort(unique(i)), function(x) st_combine(geom[i == x]))
				geom = unlist(geom, recursive = FALSE)
				if (is.null(geom))
					geom = list() #676 #nocov
				do.call(st_sfc, c(geom, crs = list(crs), precision = precision))
			} else { # single group:
				if (nrow(ret) > 1)
					stop(paste0("when using .by, also add across(", sf_column, ", st_union) as argument")) # https://github.com/r-spatial/sf/issues/2207
				if (do_union)
					st_union(geom, is_coverage = is_coverage)
				else
					st_combine(geom)
			}
		ret[[ sf_column ]] = geom
	}
	# need to re-sort out the geometry column class now:
	st_as_sf(structure(ret, sf_column = NULL))
}


#' @name tidyverse
#' @param .keep_all see corresponding function in dplyr
#' @examples
#' if (require(dplyr, quietly = TRUE)) {
#'  nc[c(1:100, 1:10), ] %>% distinct() %>% nrow()
#' }
#' @details \code{distinct} gives distinct records for which all attributes and geometries are distinct; \link{st_equals} is used to find out which geometries are distinct.
distinct.sf <- function(.data, ..., .keep_all = FALSE) {
	sf_column = attr(.data, "sf_column")
	geom = st_geometry(.data)
	eq = sapply(st_equals(.data), head, n = 1)
	if (is.list(eq) && length(eq) == 0) # empty list: geometry was empty set
		eq = integer(0)
	empties = which(lengths(eq) == 0)
	eq[ empties ] = empties[1] # first empty record
	.data[[ sf_column ]] = unlist(eq)
	class(.data) = setdiff(class(.data), "sf")

	if (!requireNamespace("dplyr", quietly = TRUE))
		stop("dplyr required: install that first") # nocov
	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")

	.data = dplyr::distinct(.data, ..., .keep_all = .keep_all)
	if (is.null(.data[[ sf_column ]]))
		.data
	else {
		.data[[ sf_column ]] = geom[ .data[[ sf_column ]] ]
		st_as_sf(.data, sf_column_name = sf_column)
	}
}

## tidyr methods: --------

#' @name tidyverse
#' @param data see original function docs
#' @param key see original function docs
#' @param value see original function docs
#' @param na.rm see original function docs
#' @param factor_key see original function docs
#' @examples
#' if (require(tidyr, quietly = TRUE) && require(dplyr, quietly = TRUE) && "geometry" %in% names(nc)) {
#'  nc %>% select(SID74, SID79) %>% gather("VAR", "SID", -geometry) %>% summary()
#' }
gather.sf <- function(data, key, value, ..., na.rm = FALSE, convert = FALSE, factor_key = FALSE) {

	if (! requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")

	key = rlang::enquo(key)
	value = rlang::enquo(value)

	if (!requireNamespace("tidyr", quietly = TRUE))
		stop("tidyr required: install first?")

	class(data) <- setdiff(class(data), "sf")
    st_as_sf(tidyr::gather(data, !!key, !!value, ...,
		na.rm = na.rm, convert = convert, factor_key = factor_key),
		sf_column_name = attr(data, "sf_column"))
}

#' @name tidyverse
#' @param data see original function docs
#' @param cols see original function docs
#' @param names_to,names_pattern,names_ptypes,names_transform see [tidyr::pivot_longer()]
#' @param values_to,values_drop_na,values_ptypes,values_transform See [tidyr::pivot_longer()]
pivot_longer.sf <- function (data, cols, names_to = "name", names_prefix = NULL,
		names_sep = NULL, names_pattern = NULL, names_ptypes = NULL,
		names_transform = NULL, names_repair = "check_unique",
		values_to = "value", values_drop_na = FALSE, values_ptypes = NULL,
		values_transform = NULL, ...) {

  sf_column_name = attr(data, "sf_column")
  data = as.data.frame(data)

# instead of:
#	st_as_sf(NextMethod(), sf_column_name = sf_column_name)
# we avoid NextMethod(); for the reason
# see https://github.com/tidyverse/tidyr/issues/1171:
  if (!requireNamespace("tidyr", quietly = TRUE))
    stop("tidyr required: install first?")
  out <- tidyr::pivot_longer(
    data = data,
    cols = {{ cols }},
    names_to = names_to,
    names_prefix = names_prefix,
    names_sep = names_sep,
    names_pattern = names_pattern,
    names_ptypes = names_ptypes,
    names_transform = names_transform,
    names_repair = names_repair,
    values_to = values_to,
    values_drop_na = values_drop_na,
    values_ptypes = values_ptypes,
    values_transform = values_transform,
    ...
  )
  st_as_sf(out, sf_column_name = sf_column_name)
}

globalVariables(c("name", "value"))
# https://github.com/r-spatial/sf/issues/1915
#' @name tidyverse
#' @param id_cols,id_expand,names_from,names_sort,names_glue,names_vary,names_expand see [tidyr::pivot_wider()]
# names_prefix,names_sep and names_repair are shared between pivot_longer() and pivot_wider()
#' @param names_prefix,names_sep,names_repair see original function docs.
#' @param values_from,values_fill,values_fn,unused_fn see [tidyr::pivot_wider()]
pivot_wider.sf = function(data, 
						  ..., 
						  id_cols = NULL, 
						  id_expand = FALSE, 
						  names_from = name, 
						  names_prefix = "", 
						  names_sep = "_", 
						  names_glue = NULL, 
						  names_sort = FALSE, 
						  names_vary = "fastest", 
						  names_expand = FALSE, 
						  names_repair = "check_unique", 
						  values_from = value, 
						  values_fill = NULL, 
						  values_fn = NULL, 
						  unused_fn = NULL) {

	agr = st_agr(data)
	sf_column_name = attr(data, "sf_column")
	data = as.data.frame(data)
	if (!requireNamespace("tidyr", quietly = TRUE))
		stop("tidyr required: install first?")
	ret = tidyr::pivot_wider(
		data = data, 
		..., 
		id_cols = {{ id_cols }}, 
		id_expand = id_expand, 
		names_from = {{ names_from }}, 
		names_prefix = names_prefix, 
		names_sep = names_sep, 
		names_glue = names_glue, 
		names_sort = names_sort, 
		names_vary = names_vary, 
		names_expand = names_expand, 
		names_repair = names_repair, 
		values_from = {{ values_from }}, 
		values_fill = values_fill, 
		values_fn = values_fn, 
		unused_fn = unused_fn
	)
	st_as_sf(ret, sf_column_name = sf_column_name, agr = agr)
}



#' @name tidyverse
#' @param fill see original function docs
#' @param drop see original function docs
#' @examples
#' if (require(tidyr, quietly = TRUE) && require(dplyr, quietly = TRUE) && "geometry" %in% names(nc)) {
#'  nc$row = 1:100 # needed for spread to work
#'  nc %>% select(SID74, SID79, geometry, row) %>%
#'		gather("VAR", "SID", -geometry, -row) %>%
#'		spread(VAR, SID) %>% head()
#' }
spread.sf <- function(data, key, value, fill = NA, convert = FALSE, drop = TRUE,
	        sep = NULL) {

	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")
	key = rlang::enquo(key)
	value = rlang::enquo(value)

	class(data) <- setdiff(class(data), "sf")
	st_as_sf(tidyr::spread(data, !!key, !!value, fill = fill, convert = convert,
		drop = drop, sep = sep), sf_column_name = attr(data, "sf_column"))
}

#' @name tidyverse
#' @param tbl see original function docs
#' @param size see original function docs
#' @param replace see original function docs
#' @param weight see original function docs
#' @param .env see original function docs
sample_n.sf <- function(tbl, size, replace = FALSE, weight = NULL, .env = parent.frame()) {
	st_sf(NextMethod(), sf_column_name = attr(tbl, "sf_column"))
}

#' @name tidyverse
sample_frac.sf <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = parent.frame()) {
	st_sf(NextMethod(), sf_column_name = attr(tbl, "sf_column"))
}
#' @name tidyverse
#' @param .tbl see original function docs
#' @param .keep see original function docs
group_split.sf <- function(.tbl, ..., .keep = TRUE) {
	class(.tbl) = setdiff(class(.tbl), "sf")
	if (inherits(.tbl, "rowwise_df")) {
		lapply(dplyr::group_split(.tbl, ...), st_as_sf)
	} else {
		lapply(dplyr::group_split(.tbl, ..., .keep = .keep), st_as_sf)	
	}
}
#' @name tidyverse
#' @examples
#' if (require(tidyr, quietly = TRUE) && require(dplyr, quietly = TRUE)) {
#'  storms.sf = st_as_sf(storms, coords = c("long", "lat"), crs = 4326)
#'  x <- storms.sf %>% group_by(name, year) %>% nest
#'  trs = lapply(x$data, function(tr) st_cast(st_combine(tr), "LINESTRING")[[1]]) %>%
#'     st_sfc(crs = 4326)
#'  trs.sf = st_sf(x[,1:2], trs)
#'  plot(trs.sf["year"], axes = TRUE)
#' }
#' @details \code{nest} assumes that a simple feature geometry list-column was among the columns that were nested.
nest.sf = function(.data, ...) {

	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")
	if (!requireNamespace("tidyr", quietly = TRUE))
		stop("tidyr required: install first?")

	class(.data) <- setdiff(class(.data), "sf")
	ret = tidyr::nest(.data, ...)
	lst = which(sapply(ret, inherits, "list"))[1]
	# re-sf:
	ret[[lst]] = lapply(ret[[lst]], st_as_sf, sf_column_name = attr(.data, "sf_column"))
	ret
}


#' @name tidyverse
#' @param col see \link[tidyr]{separate}
#' @param into see \link[tidyr]{separate}
#' @param remove see \link[tidyr]{separate}
#' @param extra see \link[tidyr]{separate}
separate.sf = function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE,
	convert = FALSE, extra = "warn", fill = "warn", ...) {

	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")
	col = rlang::enquo(col)

	if (!requireNamespace("tidyr", quietly = TRUE))
		stop("tidyr required: install first?")

	class(data) <- setdiff(class(data), "sf")
	st_as_sf(tidyr::separate(data, !!col, into = into,
		sep = sep, remove = remove, convert = convert, extra = extra, fill = fill, ...),
			sf_column_name = attr(data, "sf_column"))
}

#' @name tidyverse
#' @param sep see \link[tidyr]{separate_rows}
#' @param convert see \link[tidyr]{separate_rows}
separate_rows.sf <- function(data, ..., sep = "[^[:alnum:]]+", convert = FALSE) {
	if (!requireNamespace("tidyr", quietly = TRUE))
		stop("tidyr required: install first?")
	class(data) <- setdiff(class(data), "sf")
	ret = tidyr::separate_rows(data, ..., sep = sep, convert = convert)
	st_as_sf(ret, sf_column_name = attr(data, "sf_column"))
}

#' @name tidyverse
unite.sf <- function(data, col, ..., sep = "_", remove = TRUE) {
	class(data) <- setdiff(class(data), "sf")
	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")
	col = rlang::enquo(col)
	st_as_sf(tidyr::unite(data, !!col, ..., sep = sep, remove = remove),
		sf_column_name = attr(data, "sf_column"))
}

#' @name tidyverse
#' @param .preserve see \link[tidyr:nest]{unnest}
unnest.sf = function(data, ..., .preserve = NULL) {
	# nocov start
	sf_column_name = attr(data, "sf_column", exact = TRUE)
	if (!requireNamespace("tidyr", quietly = TRUE))
		stop("unnest requires tidyr; install that first")
	class(data) = setdiff(class(data), "sf")
	st_sf(NextMethod(), sf_column_name = sf_column_name)
	# nocov end
}

#' @name tidyverse
drop_na.sf <- function(x, ...) {
	sf_column_name = attr(x, "sf_column")
	class(x) <- setdiff(class(x), "sf")
	st_as_sf(NextMethod(), sf_column_name = sf_column_name)
}

## tibble methods: -------

#' Summarize simple feature type for tibble
#'
#' Summarize simple feature type / item for tibble
#' @param x object of class `sfc`
#' @param ... ignored
#' @name tibble
#' @details see \link[pillar]{type_sum}
type_sum.sfc <- function(x, ...) {
	cls = substring(class(x)[1], 5)
	u = st_crs(x)$ud_unit
	if (!is.null(u)) # add [units]:
		cls = paste0(cls, " [", enc2utf8(as.character(units(u))), "]")
	cls
}

#' @rdname tibble
obj_sum.sfc <- function(x) {
	vapply(x, function(sfg) format(sfg, width = 15L), "")
}

#' @rdname tibble
pillar_shaft.sfc <- function(x, ...) {
	digits = options("pillar.sigfig")$pillar.sigfig
	if (is.null(digits))
		digits = options("digits")$digits
	out <- format(x, width = 100, digits = digits, ...)
	if (!inherits(x, "sfc_GEOMETRY") && !inherits(x, "sfc_GEOMETRYCOLLECTION"))
		out <- sub("[A-Z]+ ", "", out)
	pillar::new_pillar_shaft_simple(out, align = "right", min_width = 25)
}

#nocov start
register_all_s3_methods = function() {
	has_dplyr_1.0 =
		requireNamespace("dplyr", quietly = TRUE) &&
		utils::packageVersion("dplyr") >= "0.8.99.9000"

	if (!has_dplyr_1.0) stop("dplyr (>= 1.0) is required.", call. = FALSE)
		
	s3_register("dplyr::dplyr_reconstruct", "sf")
	s3_register("dplyr::anti_join", "sf")
	s3_register("dplyr::arrange", "sf")
	s3_register("dplyr::distinct", "sf")
	s3_register("dplyr::filter", "sf")
	s3_register("dplyr::full_join", "sf")
	s3_register("dplyr::group_by", "sf")
#	s3_register("dplyr::group_map", "sf")
	s3_register("dplyr::group_split", "sf")
	s3_register("dplyr::inner_join", "sf")
	s3_register("dplyr::left_join", "sf")
	s3_register("dplyr::mutate", "sf")
	s3_register("dplyr::rename", "sf")
	s3_register("dplyr::rename_with", "sf")
	s3_register("dplyr::right_join", "sf")
	s3_register("dplyr::rowwise", "sf")
	s3_register("dplyr::sample_frac", "sf")
	s3_register("dplyr::sample_n", "sf")
	s3_register("dplyr::select", "sf")
	s3_register("dplyr::semi_join", "sf")
	s3_register("dplyr::slice", "sf")
	s3_register("dplyr::summarise", "sf")
	s3_register("dplyr::transmute", "sf")
	s3_register("dplyr::ungroup", "sf")
	s3_register("tidyr::drop_na", "sf")
	s3_register("tidyr::gather", "sf")
	s3_register("tidyr::pivot_longer", "sf")
	s3_register("tidyr::pivot_wider", "sf")
	s3_register("tidyr::spread", "sf")
	s3_register("tidyr::nest", "sf")
	s3_register("tidyr::separate", "sf")
	s3_register("tidyr::separate_rows", "sf")
	s3_register("tidyr::unite", "sf")
	s3_register("tidyr::unnest", "sf")
	s3_register("pillar::obj_sum", "sfc")
	s3_register("pillar::type_sum", "sfc")
	s3_register("pillar::pillar_shaft", "sfc")
	s3_register("spatstat.geom::as.ppp", "sfc")
	s3_register("spatstat.geom::as.ppp", "sf")
	s3_register("spatstat.geom::as.owin", "POLYGON")
	s3_register("spatstat.geom::as.owin", "MULTIPOLYGON")
	s3_register("spatstat.geom::as.owin", "sfc_POLYGON")
	s3_register("spatstat.geom::as.owin", "sfc_MULTIPOLYGON")
	s3_register("spatstat.geom::as.owin", "sfc")
	s3_register("spatstat.geom::as.owin", "sf")
	s3_register("spatstat.geom::as.psp", "LINESTRING")
	s3_register("spatstat.geom::as.psp", "MULTILINESTRING")
	s3_register("spatstat.geom::as.psp", "sfc_MULTILINESTRING")
	s3_register("spatstat.geom::as.psp", "sfc")
	s3_register("spatstat.geom::as.psp", "sf")
	s3_register("s2::as_s2_geography", "sfg")
	s3_register("s2::as_s2_geography", "sfc")
	s3_register("s2::as_s2_geography", "sf")
	register_vctrs_methods()
}
# nocov end
