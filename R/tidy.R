## dplyr methods:

#' dplyr verb methods for sf objects
#' 
#' dplyr verb methods for sf objects
#' @param .data data object of class \link{sf}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name dplyr
#' @export
filter_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @export
arrange_.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @param .keep_all see corresponding function in dplyr
#' @export
distinct_.sf <- function(.data, ..., .dots, .keep_all = FALSE) {
	st_as_sf(NextMethod())
}

#' @name dplyr
#' @param .chunk_size see corresponding function in dplyr
#' @export
do_.sf <- function(.data, ..., .dots, .chunk_size = 10000L) {
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
	st_as_sf(NextMethod())
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
