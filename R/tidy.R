## dplyr methods:

#' Dplyr verb methods for sf objects
#' 
#' Dplyr verb methods for sf objects
#' @param .data data object of class \link{sf}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name dplyr
#' @export
#' @examples
#' library(dplyr)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' nc %>% filter(AREA > .1) %>% plot()
filter_.sf <- function(.data, ..., .dots) {
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
#' @param .keep_all see corresponding function in dplyr
#' @export
#' @examples
#' nc[c(1:100, 1:10), ] %>% distinct() %>% nrow()
distinct_.sf <- function(.data, ..., .dots, .keep_all = FALSE) {
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
#' @examples
#' nc2 <- nc %>% mutate(area10 = AREA/10)
mutate_.sf <- function(.data, ..., .dots) {
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

#' @name dplyr
#' @export
#' @examples
#' nc2 <- nc %>% rename(area = AREA)
rename_.sf <- function(.data, ..., .dots) {
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
#' @examples
#' nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#' nc.g <- nc %>% group_by(area_cl)
#' nc.g %>% summarise(mean(AREA))
#' nc.g %>% summarize(mean(AREA))
#' nc.g %>% summarize(mean(AREA)) %>% plot(col = grey(3:6 / 7))
summarise_.sf <- function(.data, ..., .dots) {
	if (inherits(.data, "grouped_df") || inherits(.data, "grouped_dt")) {
		geom = st_geometry(.data)
		i = lapply(attr(.data, "indices"), function(x) x + 1) # they are 0-based!!
		sf_column = attr(.data, "sf_column")
		ret = NextMethod()
		# merge geometry:
		geoms = unlist(lapply(i, function(x) st_union(geom[x])), recursive = FALSE)
		ret[[sf_column]] = do.call(st_sfc, geoms)
		st_as_sf(ret, crs = st_crs(.data))
	} else
		as.data.frame(NextMethod())
}

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
#	g = st_geometry(data)
#	st_geometry(data) = NULL # drop geometry
#	row = setdiff(names(data), c(key_col, value_col))
#	ret = NextMethod()
#	if (length(row))
#		st_geometry(ret) = g[ match(ret[[1]], data[[ row[1] ]]) ]
#	ret
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
