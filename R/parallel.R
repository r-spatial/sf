#' @importFrom RcppParallel RcppParallelLibs
NULL

#' Assert that argument is valid for threads
#' @param threads object to check for validity
#' @param stopiffalse logical; should an error be thrown if \code{x} is not valid? Defaults to \code{TRUE}
#' @return logical; indicating if \code{x} is valid for setting the number of threads
#' @examples
#' is_valid_thread_number(1) # TRUE
#' try(is_valid_thread_number(NA)) # ERROR
#' is_valid_thread_number(NA, FALSE) # FALSE
#' is_valid_thread_number(1e+100, FALSE) # FALSE (unless working on a super computer)
#' @noRd
is_valid_thread_number = function(threads, stopiffalse = TRUE) {
	out = TRUE
	if (is.na(threads) || !is.numeric(threads) ||!is.finite(threads) || 
		isTRUE(threads < 1) || isTRUE(length(threads) > 1)) {
		out = FALSE
		if (stopiffalse) {
			stop("argument threads must be a single positive finite integer greater than zero")
		}
	}
	if (out && !is_parallel_available() && isTRUE(threads > 1)) {
		stop("argument to threads must be 1 when parallel processing is not available")
	}
	if (out && is_parallel_available()) {
		if (threads > number_processors()) {
			out = FALSE
			if (stopiffalse) {
				stop("argument threads must be less than or equal to the number of available processors")
			}
		}
	}
	return(out)
}

#' Number of processors
#' @return integer; number of logical processors
#' @examples
#' try(number_processors())
#' @export
number_processors = function() {
	if (!is_parallel_available())
		stop("parallel processing is not available")
	CPL_number_processors()
}

#' Is parallel processing available?
#'
#' @return logical; indicating if parallel processing is available
#' @examples
#' is_parallel_available()
#' @export
is_parallel_available = function()
	!is.na(CPL_tbb_version())
