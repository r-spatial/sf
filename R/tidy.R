## dplyr methods:

#' dplyr verb methods for sf objects
#' 
#' dplyr verb methods for sf objects
#' @param .data data object of class \link{sf}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name dplyr
#' @export
#' @examples
#' library(dplyr)
#' nc = st_read(system.file("shape/nc.shp", package="sf"), crs = 4267)
#' nc %>% filter(AREA > .1) %>% plot()
filter_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
#' @examples
#' # plot 10 smallest counties in grey:
#' nc %>% plot()
#' nc %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
arrange_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @param .keep_all see corresponding function in dplyr
#' @export
#' @examples
#' nc %>% distinct() %>% class()
distinct_.sf <- function(.data, ..., .dots, .keep_all = FALSE) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @param add see corresponding function in dplyr
#' @export
group_by_.sf <- function(.data, ..., .dots, add = FALSE) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
mutate_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
transmute_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
select_.sf <- function(.data, ..., .dots) {
	ret = NextMethod()
	if (any(sapply(ret, function(x) inherits(x, "sfc"))))
		st_as_sf(ret)
	else
		structure(ret, class = class(ret)[-1])
}

#' @name dplyr
#' @export
rename_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
slice_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

## needs work: modifies geometry --
#summarise_ <- function(.data, ..., .dots) {
#}
#summarize_ <- function(.data, ..., .dots) {
#}

## tidyr methods:

#' @name dplyr
#' @param data see original function docs
#' @param key_col see original function docs
#' @param value_col see original function docs
#' @param fill see original function docs
#' @param convert see original function docs
#' @param drop see original function docs
#' @param sep see original function docs
#' @export
spread_.sf <- function(data, key_col, value_col, fill = NA, 
		convert = FALSE, drop = TRUE, sep = NULL) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
#' @param gather_cols see original function docs
#' @param na.rm see original function docs
#' @param factor_key see original function docs
gather_.sf <- function(data, key_col, value_col, gather_cols, na.rm = FALSE, 
		convert = FALSE, factor_key = FALSE) {
	st_as_sf(NextMethod())
}

## tibble methods:

#' summarize simple feature type for tibble
#'
#' summarize simple feature type for tibble
#' @param x object of class sfc
#' @param ... ignored
#' @name tibble
#' @export
type_sum.sfc <- function(x, ...) {
   "simple_feature"
}

#' summarize simple feature item for tibble
#'
#' summarize simple feature item for tibble
#' @name tibble
#' @export
obj_sum.sfc <- function(x) {
	sapply(x, function(sfg) format(sfg, digits = 15L))
}
