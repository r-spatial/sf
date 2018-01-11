#include <cstddef>
#include <string>

#include <Rcpp.h>

#if _OPENMP
#include <omp.h>
#else
// define omp_get_thread_num from omp.h if openmp is not available
// see https://gcc.gnu.org/onlinedocs/libgomp/omp_005fget_005fthread_005fnum.html
int omp_get_thread_num() {
	return 0;
}
// define omp_get_num_procs from omp.h if openmp is not available
// https://gcc.gnu.org/onlinedocs/libgomp/omp_005fget_005fnum_005fprocs.html
int omp_get_num_procs() {
	return 1;
}
#endif

// [[Rcpp::export]]
Rcpp::IntegerVector CPL_openmp_version() {
#if _OPENMP
	int out = _OPENMP;
#else
	int out = NA_INTEGER;
#endif
	return Rcpp::wrap(out);
}

// [[Rcpp::export]]
Rcpp::IntegerVector CPL_number_processors() {
	int out = omp_get_num_procs();
	return Rcpp::wrap(out);
}
