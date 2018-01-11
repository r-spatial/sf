#include <cstddef>
#include <string>

#include <Rcpp.h>
#include <RcppParallel.h>

// [[Rcpp::export]]
Rcpp::IntegerVector CPL_tbb_version() {
#if RCPP_PARALLEL_USE_TBB
	int out = tbb::TBB_runtime_interface_version();
#else
	int out = NA_INTEGER;
#endif
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::IntegerVector CPL_tbb_processors() {
#if RCPP_PARALLEL_USE_TBB
	int out = tbb::task_scheduler_init::default_num_threads();
#else
	int out = NA_INTEGER;
#endif
	return Rcpp::wrap(out);
}
