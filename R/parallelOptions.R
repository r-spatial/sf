#' @name set_mcOption
#' @title Options for parallel support
#' @description Provides support for the use of parallel computation from the parallel package
#' @details 
#' @param value logical; indicates the availability of multicore clusters, set to TRUE on package load unless platform is Windows, in which case FALSE
#' @export
set_mcOption <- function(value) {
        stopifnot(is.logical(value))
        stopifnot(length(value) == 1)
	res <- get("mc", envir = .sf_cache)
        if (.Platform$OS.type == "windows") {
            if (value) warning("multicore not available on Windows")
        } else {
	    assign("mc", value, envir = .sf_cache)
        }
	res
}

get_mcOption  <- function() {
	get("mc", envir = .sf_cache)
}

set_coresOption <- function(value) {
	res <- get("cores", envir = .sf_cache)
        if (is.null(value)) {
            assign("cores", value, envir = .sf_cache)
        } else {
            stopifnot(is.integer(value))
            stopifnot(length(value) == 1)
            stopifnot(!is.na(value))
	    assign("cores", value, envir = .sf_cache)
        }
	res
}

get_coresOption  <- function() {
	get("cores", envir = .sf_cache)
}

set_ClusterOption <- function(cl) {
	if (!is.null(cl)) {
            if (!inherits(cl, "cluster")) stop ("cluster required")
        }
	assign("cluster", cl, envir = .sf_cache)
        invisible(NULL)
}

get_ClusterOption  <- function() {
	get("cluster", envir = .sf_cache)
}

