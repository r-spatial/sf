## dplyr methods:

#' Dplyr verb methods for sf objects
#' 
#' Dplyr verb methods for sf objects. Geometries are sticky, use \link{as.data.frame} to let code{dplyr}'s own methods drop them.
#' @param .data data object of class \link{sf}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name dplyr
#' @examples
#' library(dplyr)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' nc %>% filter(AREA > .1) %>% plot()
#' @export
filter_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}
#' @name dplyr
#' @export
filter.sf <- function(.data, ...) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
#' @examples
#' # plot 10 smallest counties in grey:
#' st_geometry(nc) %>% plot()
#' nc %>% select(AREA) %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
#' title("the ten counties with smallest area")
arrange_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}
#' @name dplyr
#' @export
arrange.sf <- function(.data, ...) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @param .keep_all see corresponding function in dplyr
#' @export
#' @examples
#' nc[c(1:100, 1:10), ] %>% distinct() %>% nrow()
distinct_.sf <- function(.data, ..., .dots, .keep_all = FALSE) {
	st_as_sf(NextMethod())
}
#' @name dplyr
#' @export
distinct.sf <- function(.data, ..., .dots, .keep_all = FALSE) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @param add see corresponding function in dplyr
#' @export
#' @examples
#' nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#' nc %>% group_by(area_cl) %>% class()
group_by_.sf <- function(.data, ..., .dots, add = FALSE) {
	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(NextMethod())
}
#' @name dplyr
#' @export
group_by.sf <- function(.data, ..., .dots, add = FALSE) {
	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
ungroup.sf <- function(x, ...) {
	class(x) <- setdiff(class(x), "sf")
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
#' @examples
#' nc2 <- nc %>% mutate(area10 = AREA/10)
mutate_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}
#' @name dplyr
#' @export
mutate.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
#' @examples
#' nc %>% transmute(AREA = AREA/10, geometry = geometry) %>% class()
#' nc %>% transmute(AREA = AREA/10) %>% class()
transmute_.sf <- function(.data, ..., .dots) {
	ret = NextMethod()
	if (attr(ret, "sf_column") %in% names(ret))
		st_as_sf(NextMethod())
	else
		ret
}
#' @name dplyr
#' @export
transmute.sf <- function(.data, ..., .dots) {
	ret = NextMethod()
	if (attr(ret, "sf_column") %in% names(ret))
		st_as_sf(NextMethod())
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
select_.sf <- function(.data, ..., .dots = NULL) {
  .dots <- c(.dots, attr(.data, "sf_column")) 
  ret = NextMethod()
  structure(ret, agr = st_agr(ret))
}

unclass_sf <- function(x) {
	i <- match("sf", class(x))
	class <- class(x)[-seq_len(i)]
	structure(x, class = class)
}

#' @name dplyr
#' @export
#' @details \code{select} keeps the geometry regardless whether it is selected or not; to deselect it, first pipe through \code{as.data.frame} to let dplyr's own \code{select} drop it.
select.sf <- if (requireNamespace("dplyr", quietly = TRUE) && utils::packageVersion("dplyr") > "0.5.0") {
  function(.data, ...) {
	.data <- unclass_sf(.data)
	sf_column <- attr(.data, "sf_column")

	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")

	ret <- dplyr::select(.data, ..., !! rlang::sym(sf_column))
	st_as_sf(ret)
  }
} else {
  function(.data, ...) {
	stop("requires dplyr > 0.5.0: install that first, then reinstall sf")
  }
}


#' @name dplyr
#' @export
#' @examples
#' nc2 <- nc %>% rename(area = AREA)
rename_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
rename.sf <- function(.data, ...) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
#' @examples
#' nc %>% slice(1:2)
slice_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
slice.sf <- function(.data, ...) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
#' @aliases summarise
#' @param do_union logical; should geometries be unioned by using \link{st_union}, or simply be combined using \link{st_combine}? Using \link{st_union} resolves internal boundaries, but in case of unioning points may also change the order of the points.
summarise.sf <- function(.data, ..., .dots, do_union = TRUE) {
	sf_column = attr(.data, "sf_column")
	crs = st_crs(.data)
	ret = NextMethod()

	if (utils::packageVersion("dplyr") <= "0.5.0") {
		stopifnot(requireNamespace("lazyeval", quietly = TRUE))
		do_union = is.null(.dots$do_union) || isTRUE(lazyeval::lazy_eval(.dots$do_union))
	}

	geom = if (inherits(.data, "grouped_df") || inherits(.data, "grouped_dt")) {
		geom = st_geometry(.data)
		i = lapply(attr(.data, "indices"), function(x) x + 1) # they are 0-based!!
		# merge geometry:
		geom = if (do_union)
			unlist(lapply(i, function(x) st_union(geom[x])), recursive = FALSE)
		else
			unlist(lapply(i, function(x) st_combine(geom[x])), recursive = FALSE)
		do.call(st_sfc, geom)
	} else { # single group:
		if (do_union)
			st_union(st_geometry(.data))
		else
			st_combine(st_geometry(.data))
	}
	ret[[ sf_column ]] = geom
	ret$do_union = NULL
	st_as_sf(ret, crs = crs)
}
#' @name dplyr
#' @export
#' @examples
#' nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#' nc.g <- nc %>% group_by(area_cl)
#' nc.g %>% summarise(mean(AREA))
#' nc.g %>% summarise(mean(AREA)) %>% plot(col = grey(3:6 / 7))
#' nc %>% as.data.frame %>% summarise(mean(AREA))
summarise_.sf = summarise.sf

## summarize_ not needed

## tidyr methods:

#' @name dplyr
#' @export
#' @param data see original function docs
#' @param key_col see original function docs
#' @param value_col see original function docs
#' @param gather_cols see original function docs
#' @param na.rm see original function docs
#' @param factor_key see original function docs
#' @examples 
#' library(tidyr)
#' nc %>% select(SID74, SID79, geometry) %>% gather(VAR, SID, -geometry) %>% summary()
gather_.sf <- function(data, key_col, value_col, gather_cols, na.rm = FALSE, 
		convert = FALSE, factor_key = FALSE) {
	st_as_sf(NextMethod())
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
#'      gather(VAR, SID, -geometry, -row) %>% 
#'		spread(VAR, SID) %>% head()
spread_.sf <- function(data, key_col, value_col, fill = NA, 
		convert = FALSE, drop = TRUE, sep = NULL) {
	data <- as.data.frame(data)
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @param tbl see original function docs
#' @param size see original function docs
#' @param replace see original function docs
#' @param weight see original function docs
#' @param .env see original function docs
#' @export
sample_n.sf <- function(tbl, size, replace = FALSE, weight = NULL, .env = parent.frame()) {
	st_sf(NextMethod())
}
	      
#' @name dplyr
#' @export
sample_frac.sf <- function(tbl, size = 1, replace = FALSE, weight = NULL, .env = parent.frame()) {
	st_sf(NextMethod())
}

#' @name dplyr
#' @param nest_cols see \link[tidyr]{nest}
#' @export
nest_.sf <- function(data, key_col, nest_cols) {
	class(data) <- setdiff(class(data), "sf")
	ret = NextMethod()
	ret$data = lapply(ret$data, st_as_sf)
	ret
}

## tibble methods:

#' Summarize simple feature type for tibble
#'
#' Summarize simple feature type for tibble
#' @param x object of class sfc
#' @param ... ignored
#' @name tibble
#' @export
type_sum.sfc <- function(x, ...) {
   "simple_feature"
}

#' Summarize simple feature item for tibble
#'
#' Summarize simple feature item for tibble
#' @name tibble
#' @export
obj_sum.sfc <- function(x) {
	vapply(x, function(sfg) format(sfg, digits = 15L), "")
}
