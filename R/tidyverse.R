## dplyr methods:

#' Dplyr verb methods for sf objects
#'
#' Dplyr verb methods for sf objects. Geometries are sticky, use \link{as.data.frame} to let \code{dplyr}'s own methods drop them.
#' @param .data data object of class \link{sf}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name dplyr
#' @examples
#' library(dplyr)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' nc %>% filter(AREA > .1) %>% plot()
#' @export
filter.sf <- function(.data, ..., .dots) {
	#st_as_sf(NextMethod())
	sf_column = attr(.data, "sf_column")
	geom = .data[[sf_column]]
	.data[[sf_column]] = seq_len(nrow(.data))
	ret = NextMethod()
	sel = ret[[sf_column]]
	ret[[sf_column]] = geom[sel]
	st_as_sf(ret, sf_column_name = sf_column)
}

#' @name dplyr
#' @export
#' @examples
#' # plot 10 smallest counties in grey:
#' st_geometry(nc) %>% plot()
#' nc %>% select(AREA) %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
#' title("the ten counties with smallest area")
arrange.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name dplyr
#' @param add see corresponding function in dplyr
#' @export
#' @examples
#' nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#' nc %>% group_by(area_cl) %>% class()
group_by.sf <- function(.data, ..., add = FALSE) {
	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name dplyr
#' @export
ungroup.sf <- function(x, ...) {
	class(x) <- setdiff(class(x), "sf")
	st_as_sf(NextMethod(), sf_column_name = attr(x, "sf_column"))
}

#' @name dplyr
#' @export
#' @examples
#' nc2 <- nc %>% mutate(area10 = AREA/10)
mutate.sf <- function(.data, ..., .dots) {
	#st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name dplyr
#' @export
#' @examples
#' nc %>% transmute(AREA = AREA/10, geometry = geometry) %>% class()
#' nc %>% transmute(AREA = AREA/10) %>% class()
transmute.sf <- function(.data, ..., .dots) {
	ret = NextMethod()
	if (attr(ret, "sf_column") %in% names(ret))
		st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
	else
		ret
}

#' @name dplyr
#' @export
#' @examples
#' nc %>% select(SID74, SID79) %>% names()
#' nc %>% select(SID74, SID79, geometry) %>% names()
#' nc %>% select(SID74, SID79) %>% class()
#' nc %>% select(SID74, SID79, geometry) %>% class()
#' @details \code{select} keeps the geometry regardless whether it is selected or not; to deselect it, first pipe through \code{as.data.frame} to let dplyr's own \code{select} drop it.
select.sf <- function(.data, ...) {

	if (!requireNamespace("dplyr", quietly = TRUE))
		stop("dplyr required: install that first") # nocov

	class(.data) <- setdiff(class(.data), "sf")
	sf_column <- attr(.data, "sf_column")

	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")

	ret <- dplyr::select(.data, ..., !! rlang::sym(sf_column))
	st_as_sf(ret)
}


#' @name dplyr
#' @export
#' @examples
#' nc2 <- nc %>% rename(area = AREA)
rename.sf <- function(.data, ...) {

	if (!requireNamespace("dplyr", quietly = TRUE))
		stop("dplyr required: install that first") # nocov

	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(dplyr::rename(.data, ...))
	#st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name dplyr
#' @export
#' @examples
#' nc %>% slice(1:2)
slice.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name dplyr
#' @export
#' @aliases summarise
#' @param do_union logical; should geometries be unioned by using \link{st_union}, or simply be combined using \link{st_combine}? Using \link{st_union} resolves internal boundaries, but in case of unioning points may also change the order of the points.
#' @examples
#' nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#' nc.g <- nc %>% group_by(area_cl)
#' nc.g %>% summarise(mean(AREA))
#' nc.g %>% summarise(mean(AREA)) %>% plot(col = grey(3:6 / 7))
#' nc %>% as.data.frame %>% summarise(mean(AREA))
summarise.sf <- function(.data, ..., .dots, do_union = TRUE) {
	sf_column = attr(.data, "sf_column")
	crs = st_crs(.data)
	ret = NextMethod()

	geom = if (inherits(.data, "grouped_df") || inherits(.data, "grouped_dt")) {
		geom = st_geometry(.data)
		i = lapply(attr(.data, "indices"), function(x) x + 1) # they are 0-based!!
		# merge geometry:
		geom = if (do_union)
			unlist(lapply(i, function(x) st_union(geom[x])), recursive = FALSE)
		else
			unlist(lapply(i, function(x) st_combine(geom[x])), recursive = FALSE)
		if (is.null(geom))
			st_sfc() #676 #nocov
		else
			do.call(st_sfc, geom)
	} else { # single group:
		if (do_union)
			st_union(st_geometry(.data))
		else
			st_combine(st_geometry(.data))
	}
	ret[[ sf_column ]] = geom
	ret$do_union = NULL
	st_as_sf(ret, crs = crs, precision = st_precision(.data),
		sf_column_name = attr(.data, "sf_column"))
}

#' @name dplyr
#' @param .keep_all see corresponding function in dplyr
#' @export
#' @examples
#' nc[c(1:100, 1:10), ] %>% distinct() %>% nrow()
#' @details \code{distinct.sf} gives distinct records for which all attributes and geometries are distinct; \link{st_equals} is used to find out which geometries are distinct.
distinct.sf <- function(.data, ..., .keep_all = FALSE) {
	sf_column = attr(.data, "sf_column")
	geom = st_geometry(.data)
	.data[[ sf_column ]] = vapply(st_equals(.data), head, NA_integer_, n = 1)
	class(.data) = setdiff(class(.data), "sf")
	.data = NextMethod()
	.data[[ sf_column ]] = geom[ .data[[ sf_column ]] ]
	st_as_sf(.data)
}

## tidyr methods:

#' @name dplyr
#' @export
#' @param data see original function docs
#' @param key see original function docs
#' @param value see original function docs
#' @param na.rm see original function docs
#' @param factor_key see original function docs
#' @examples
#' library(tidyr)
#' nc %>% select(SID74, SID79) %>% gather(VAR, SID, -geometry) %>% summary()
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


#' @name dplyr
#' @param fill see original function docs
#' @param convert see original function docs
#' @param drop see original function docs
#' @param sep see original function docs
#' @export
#' @examples
#' library(tidyr)
#' nc$row = 1:100 # needed for spread to work
#' nc %>% select(SID74, SID79, geometry, row) %>%
#'		gather(VAR, SID, -geometry, -row) %>%
#'		spread(VAR, SID) %>% head()
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

#' @name dplyr
#' @param tbl see original function docs
#' @param size see original function docs
#' @param replace see original function docs
#' @param weight see original function docs
#' @param .env see original function docs
#' @export
sample_n.sf <- function(tbl, size, replace = FALSE, weight = NULL, .env = parent.frame()) {
	st_sf(NextMethod(), sf_column_name = attr(tbl, "sf_column"))
}

#' @name dplyr
#' @export
sample_frac.sf <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = parent.frame()) {
	st_sf(NextMethod(), sf_column_name = attr(tbl, "sf_column"))
}

#' @name dplyr
#' @param .key see \link[tidyr]{nest}
#' @export
#' @examples
#' storms.sf = st_as_sf(storms, coords = c("long", "lat"), crs = 4326)
#' x <- storms.sf %>% group_by(name, year) %>% nest
#' trs = lapply(x$data, function(tr) st_cast(st_combine(tr), "LINESTRING")[[1]]) %>% st_sfc(crs = 4326)
#' trs.sf = st_sf(x[,1:2], trs)
#' plot(trs.sf["year"], axes = TRUE)
#' @details \code{nest.sf} assumes that a simple feature geometry list-column was among the columns that were nested.
nest.sf = function (data, ..., .key = "data") {
	class(data) <- setdiff(class(data), "sf")

	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")
	key = rlang::enquo(.key)

	if (!requireNamespace("tidyr", quietly = TRUE))
		stop("tidyr required: install first?")
	ret = tidyr::nest(data, ..., .key = !! key)
	# should find out first if geometry column was in ... !
	ret[[.key]] = lapply(ret[[.key]], st_as_sf, sf_column_name = attr(data, "sf_column"))
	ret
}


#' @name dplyr
#' @param col see \link[tidyr]{separate}
#' @param into see \link[tidyr]{separate}
#' @param remove see \link[tidyr]{separate}
#' @param extra see \link[tidyr]{separate}
#' @export
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

#' @name dplyr
#' @export
unite.sf <- function(data, col, ..., sep = "_", remove = TRUE) {
	class(data) <- setdiff(class(data), "sf")
	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")
	col = rlang::enquo(col)
	st_as_sf(tidyr::unite(data, !!col, ..., sep = sep, remove = remove),
		sf_column_name = attr(data, "sf_column"))
}

#' @name dplyr
#' @param .preserve see \link[tidyr]{unnest}
#' @export
unnest.sf = function(data, ..., .preserve = NULL) {
	# nocov start
	if (!requireNamespace("tidyr", quietly = TRUE) ||
			utils::packageVersion("tidyr") <= "0.7.2")
		stop("unnest requires tidyr > 0.7.2; install that first")
	if (! requireNamespace("tidyselect", quietly = TRUE))
		stop("unnest requires tidyselect; install that first")
	if (! requireNamespace("rlang", quietly = TRUE))
		stop("unnest requires rlang; install that first")

	# The user might want to preserve other columns. Get these as a character
	# vector of variable names, using any valid dplyr (i.e. rlang)
	# variable selection syntax. By default, with .preserve = NULL, this will be
	# empty. Note: the !!! is from rlang.
	preserve = tidyselect::vars_select(names(data), !!! rlang::enquo(.preserve))
	# Get the name of the geometry column(s)
	sf_column_name = attr(data, "sf_column", exact = TRUE)
	preserve_incl_sf = c(preserve, sf_column_name)

	# Drop the "sf" class and call unnest again, providing the updated .preserve.
	# (Normally it wouldn't be necessary to drop the class, but tidyr calls
	# dplyr::transmute (not sf::transmute.sf), so the geometry column is
	# inadvertantly included in some of the unnest.data.frame code.
	# The .preserve argument will go through the vars_select/enquo
	# process again in unnest.data.frame, but that's fine.
	class(data) = setdiff(class(data), "sf")
	ret = st_sf(NextMethod(.preserve = preserve_incl_sf),
		sf_column_name = sf_column_name)
	ret # nocov end
}



## tibble methods:

#' Summarize simple feature type for tibble
#'
#' Summarize simple feature type for tibble
#' @param x object of class sfc
#' @param ... ignored
#' @name tibble
#' @details see \link[pillar]{type_sum}
#' @export
type_sum.sfc <- function(x, ...) {
	cls = substring(class(x)[1], 5)
	if (is.na(st_is_longlat(x)))
		cls
	else
		paste0(cls, " [", as.character(units(st_crs(x, parameters = TRUE)$ud_unit)), "]")
}

#' Summarize simple feature item for tibble
#'
#' Summarize simple feature item for tibble
#' @name tibble
#' @export
obj_sum.sfc <- function(x) {
	vapply(x, function(sfg) format(sfg, width = 15L), "")
}

#' @name tibble
#' @export
pillar_shaft.sfc <- function(x, ...) {
	digits = options("pillar.sigfig")$pillar.sigfig
	if (is.null(digits))
		digits = options("digits")$digits
	out <- format(x, width = 100, digits = digits, ...)
	if (!inherits(x, "sfc_GEOMETRY") && !inherits(x, "sfc_GEOMETRYCOLLECTION"))
		out <- sub("[A-Z]+ ", "", out)
	pillar::new_pillar_shaft_simple(out, align = "right", min_width = 25)
}
