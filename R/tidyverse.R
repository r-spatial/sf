## dplyr methods:
group_map.sf <- function(.tbl, .f, ...) {
	 st_as_sf(NextMethod())
}

group_split.sf <- function(.tbl, ..., keep = TRUE) {
	 class(.tbl) = setdiff(class(.tbl), "sf")
     lapply(dplyr::group_split(.tbl, ..., keep = keep), st_as_sf)
}

#' Tidyverse methods for sf objects (remove .sf suffix!)
#'
#' Tidyverse methods for sf objects. Geometries are sticky, use \link{as.data.frame} to let \code{dplyr}'s own methods drop them. Use these methods without the .sf suffix and after loading the tidyverse package with the generic (or after loading package tidyverse).
#' @param .data data object of class \link{sf}
#' @param .dots see corresponding function in package \code{dplyr}
#' @param ... other arguments
#' @name tidyverse
#' @examples
#' library(dplyr)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' nc %>% filter(AREA > .1) %>% plot()
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

#' @name tidyverse
#' @examples
#' # plot 10 smallest counties in grey:
#' st_geometry(nc) %>% plot()
#' nc %>% select(AREA) %>% arrange(AREA) %>% slice(1:10) %>% plot(add = TRUE, col = 'grey')
#' title("the ten counties with smallest area")
arrange.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name tidyverse
#' @param add see corresponding function in dplyr
#' @examples
#' nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#' nc %>% group_by(area_cl) %>% class()
group_by.sf <- function(.data, ..., add = FALSE) {
	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name tidyverse
ungroup.sf <- function(x, ...) {
	class(x) <- setdiff(class(x), "sf")
	st_as_sf(NextMethod(), sf_column_name = attr(x, "sf_column"))
}

#' @name tidyverse
#' @examples
#' nc2 <- nc %>% mutate(area10 = AREA/10)
mutate.sf <- function(.data, ..., .dots) {
	#st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name tidyverse
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

#' @name tidyverse
#' @examples
#' nc %>% select(SID74, SID79) %>% names()
#' nc %>% select(SID74, SID79, geometry) %>% names()
#' nc %>% select(SID74, SID79) %>% class()
#' nc %>% select(SID74, SID79, geometry) %>% class()
#' @details \code{select} keeps the geometry regardless whether it is selected or not; to deselect it, first pipe through \code{as.data.frame} to let dplyr's own \code{select} drop it.
select.sf <- function(.data, ...) {

	if (!requireNamespace("dplyr", quietly = TRUE))
		stop("dplyr required: install that first") # nocov

	agr <- st_agr(.data)
	class(.data) <- setdiff(class(.data), "sf")
	sf_column <- attr(.data, "sf_column")

	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")

	ret <- dplyr::select(.data, ..., !! rlang::sym(sf_column))
	vars <- setdiff(names(ret), sf_column)
	st_set_agr(st_as_sf(ret), agr[vars])
}


#' @name tidyverse
#' @examples
#' nc2 <- nc %>% rename(area = AREA)
rename.sf <- function(.data, ...) {

	if (!requireNamespace("dplyr", quietly = TRUE))
		stop("dplyr required: install that first") # nocov

	class(.data) <- setdiff(class(.data), "sf")
	st_as_sf(dplyr::rename(.data, ...))
	#st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name tidyverse
#' @examples
#' nc %>% slice(1:2)
slice.sf <- function(.data, ..., .dots) {
	st_as_sf(NextMethod(), sf_column_name = attr(.data, "sf_column"))
}

#' @name tidyverse
#' @aliases summarise
#' @param do_union logical; in case \code{summary} does not create a geometry column, should geometries be created by unioning using \link{st_union}, or simply by combining using \link{st_combine}? Using \link{st_union} resolves internal boundaries, but in case of unioning points, this will likely change the order of the points; see Details.
#' @return an object of class \link{sf}
#' @details 
#' In case one or more of the arguments (expressions) in the \code{summarise} call creates a geometry list-column, the first of these will be the (active) geometry of the returned object. If this is not the case, a geometry column is created, depending on the value of \code{do_union}.
#' 
#' In case \code{do_union} is \code{FALSE}, \code{summarise} will simply combine geometries using \link{c.sfg}. When polygons sharing a boundary are combined, this leads to geometries that are invalid; see for instance \url{https://github.com/r-spatial/sf/issues/681}.
#' @examples
#' nc$area_cl = cut(nc$AREA, c(0, .1, .12, .15, .25))
#' nc.g <- nc %>% group_by(area_cl)
#' nc.g %>% summarise(mean(AREA))
#' nc.g %>% summarise(mean(AREA)) %>% plot(col = grey(3:6 / 7))
#' nc %>% as.data.frame %>% summarise(mean(AREA))
summarise.sf <- function(.data, ..., .dots, do_union = TRUE) {
	sf_column = attr(.data, "sf_column")
	ret = NextMethod()
	if (!missing(do_union))
		ret$do_union = NULL

	if (! any(sapply(ret, inherits, what = "sfc"))) {
		geom = if (inherits(.data, "grouped_df") || inherits(.data, "grouped_dt")) {
			if (!requireNamespace("dplyr", quietly = TRUE))
				stop("dplyr required: install that first") # nocov
			i = dplyr::group_indices(.data)
			geom = st_geometry(.data)
			geom = if (do_union)
					lapply(sort(unique(i)), function(x) st_union(geom[i == x]))
				else
					lapply(sort(unique(i)), function(x) st_combine(geom[i == x]))
			geom = unlist(geom, recursive = FALSE)
			if (is.null(geom))
				geom = list() #676 #nocov
			do.call(st_sfc, c(geom, crs = list(st_crs(.data)), precision = st_precision(.data)))
		} else { # single group:
			if (do_union)
				st_union(st_geometry(.data))
			else
				st_combine(st_geometry(.data))
		}
		ret[[ sf_column ]] = geom
	}
	st_as_sf(structure(ret, class = setdiff(class(ret), "sf"), "sf_column" = NULL))
}


#' @name tidyverse
#' @param .keep_all see corresponding function in dplyr
#' @examples
#' nc[c(1:100, 1:10), ] %>% distinct() %>% nrow()
#' @details \code{distinct} gives distinct records for which all attributes and geometries are distinct; \link{st_equals} is used to find out which geometries are distinct.
distinct.sf <- function(.data, ..., .keep_all = FALSE) {
	sf_column = attr(.data, "sf_column")
	geom = st_geometry(.data)
	.data[[ sf_column ]] = vapply(st_equals(.data), head, NA_integer_, n = 1)
	class(.data) = setdiff(class(.data), "sf")

	if (!requireNamespace("dplyr", quietly = TRUE))
		stop("dplyr required: install that first") # nocov
	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")

	.data = dplyr::distinct(.data, ..., !! rlang::sym(sf_column), .keep_all = .keep_all)
	.data[[ sf_column ]] = geom[ .data[[ sf_column ]] ]
	st_as_sf(.data)
}

## tidyr methods:

#' @name tidyverse
#' @param data see original function docs
#' @param key see original function docs
#' @param value see original function docs
#' @param na.rm see original function docs
#' @param factor_key see original function docs
#' @examples
#' library(tidyr)
#' nc %>% select(SID74, SID79) %>% gather("VAR", "SID", -geometry) %>% summary()
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
#' @param fill see original function docs
#' @param convert see original function docs
#' @param drop see original function docs
#' @param sep see original function docs
#' @examples
#' library(tidyr)
#' nc$row = 1:100 # needed for spread to work
#' nc %>% select(SID74, SID79, geometry, row) %>%
#'		gather("VAR", "SID", -geometry, -row) %>%
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
#' @param .key see \link[tidyr]{nest}
#' @examples
#' storms.sf = st_as_sf(storms, coords = c("long", "lat"), crs = 4326)
#' x <- storms.sf %>% group_by(name, year) %>% nest
#' trs = lapply(x$data, function(tr) st_cast(st_combine(tr), "LINESTRING")[[1]]) %>% st_sfc(crs = 4326)
#' trs.sf = st_sf(x[,1:2], trs)
#' plot(trs.sf["year"], axes = TRUE)
#' @details \code{nest} assumes that a simple feature geometry list-column was among the columns that were nested.
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
unite.sf <- function(data, col, ..., sep = "_", remove = TRUE) {
	class(data) <- setdiff(class(data), "sf")
	if (!requireNamespace("rlang", quietly = TRUE))
		stop("rlang required: install first?")
	col = rlang::enquo(col)
	st_as_sf(tidyr::unite(data, !!col, ..., sep = sep, remove = remove),
		sf_column_name = attr(data, "sf_column"))
}

#' @name tidyverse
#' @param .preserve see \link[tidyr]{unnest}
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
type_sum.sfc <- function(x, ...) {
	cls = substring(class(x)[1], 5)
	if (is.na(st_is_longlat(x)))
		cls
	else
		paste0(cls, " [", enc2utf8(as.character(units(st_crs(x, parameters = TRUE)$ud_unit))), "]")
}

#' Summarize simple feature item for tibble
#'
#' Summarize simple feature item for tibble
#' @name tibble
obj_sum.sfc <- function(x) {
	vapply(x, function(sfg) format(sfg, width = 15L), "")
}

#' @name tibble
pillar_shaft.sfc <- function(x, ...) {
	digits = options("pillar.sigfig")$pillar.sigfig
	if (is.null(digits))
		digits = options("digits")$digits
	out <- format(x, width = 100, digits = digits, ...)
	if (!inherits(x, "sfc_GEOMETRY") && !inherits(x, "sfc_GEOMETRYCOLLECTION"))
		out <- sub("[A-Z]+ ", "", out)
	pillar::new_pillar_shaft_simple(out, align = "right", min_width = 25)
}

register_all_s3_methods = function() {
	register_s3_method("dplyr", "anti_join", "sf")
	register_s3_method("dplyr", "arrange", "sf")
	register_s3_method("dplyr", "distinct", "sf")
	register_s3_method("dplyr", "filter", "sf")
	register_s3_method("dplyr", "full_join", "sf")
	register_s3_method("dplyr", "group_by", "sf")
	register_s3_method("dplyr", "group_map", "sf")
	register_s3_method("dplyr", "group_split", "sf")
	register_s3_method("dplyr", "inner_join", "sf")
	register_s3_method("dplyr", "left_join", "sf")
	register_s3_method("dplyr", "mutate", "sf")
	register_s3_method("dplyr", "rename", "sf")
	register_s3_method("dplyr", "right_join", "sf")
	register_s3_method("dplyr", "sample_frac", "sf")
	register_s3_method("dplyr", "sample_n", "sf")
	register_s3_method("dplyr", "select", "sf")
	register_s3_method("dplyr", "semi_join", "sf")
	register_s3_method("dplyr", "slice", "sf")
	register_s3_method("dplyr", "summarise", "sf")
	register_s3_method("dplyr", "transmute", "sf")
	register_s3_method("dplyr", "ungroup", "sf")
	register_s3_method("tidyr", "gather", "sf")
	register_s3_method("tidyr", "spread", "sf")
	register_s3_method("tidyr", "nest", "sf")
	register_s3_method("tidyr", "separate", "sf")
	register_s3_method("tidyr", "unite", "sf")
	register_s3_method("tidyr", "unnest", "sf")
	register_s3_method("pillar", "obj_sum", "sfc")
	register_s3_method("pillar", "type_sum", "sfc")
	register_s3_method("pillar", "pillar_shaft", "sfc")
}

# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
#nocov start
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
