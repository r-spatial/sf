#' @name set_mc_option
#' @include init.R
#' @title Options for parallel support
#' @description Provides support for the use of parallel computation from the parallel package
#' @details Parallel processing options in the sf package are held in an environment local to the package namespace and not exported. Option values are set and retrieved with pairs of access functions, get and set. The \code{mc} option is set by default to FALSE on Windows systems, as they cannot fork the R session; by default it is TRUE on other systems, but may be set FALSE. If \code{mc} is FALSE, the \code{Cluster} option is used: if \code{mc} is FALSE and the \code{Cluster} option is NULL no parallel computing is done, or the \code{Cluster} option is passed a \dQuote{cluster} object created by the parallel or snow package for access without being passed as an argument. The \code{cores} option is set to NULL by default, and can be used to store the number of cores to use as an integer. If \code{cores} is NULL, facilities from the parallel package will not be used.
#' @param value logical; indicates the availability of multicore clusters, set to TRUE on package load unless platform is Windows, in which case FALSE
#' @export
set_mc_option <- function(value) {
        stopifnot(is.logical(value))
        stopifnot(length(value) == 1)
        if (.Platform$OS.type == "windows") {
            if (value) warning("multicore not available on Windows")
        } else {
	    assign("mc", value, envir = .sf_cache)
        }
	invisible(NULL)
}

#' @export
get_mc_option  <- function() {
	get("mc", envir = .sf_cache)
}

#' @export
set_cores_option <- function(value) {
        if (is.null(value)) {
            assign("cores", value, envir = .sf_cache)
        } else {
            stopifnot(is.integer(value))
            stopifnot(length(value) == 1)
            stopifnot(!is.na(value))
	    assign("cores", value, envir = .sf_cache)
        }
	invisible(NULL)
}

#' @export
get_cores_option  <- function() {
	get("cores", envir = .sf_cache)
}

#' @export
set_cluster_option <- function(cl) {
	if (!is.null(cl)) {
            if (!inherits(cl, "cluster")) stop ("cluster required")
        }
	assign("cluster", cl, envir = .sf_cache)
        invisible(NULL)
}

#' @export
get_cluster_option  <- function() {
	get("cluster", envir = .sf_cache)
}

#' @export
set_quiet_option <- function(value) {
        stopifnot(is.logical(value))
        stopifnot(length(value) == 1)
	assign("quiet", value, envir = .sf_cache)
	invisible(NULL)
}

#' @export
get_quiet_option  <- function() {
	get("quiet", envir = .sf_cache)
}

