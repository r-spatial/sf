#include <Rcpp.h>
#include <RcppParallel.h>
#include <thread>

template <typename T>
void parallel_for_loop(std::size_t i, std::size_t j, T lambda) {
#if RCPP_PARALLEL_USE_TBB
	tbb::parallel_for(i, j, lambda);
#else
	std::vector<std::size_t> zzz(j - i);
	std::iota(zzz.begin(), zzz.end(), i);
	std::for_each(zzz.begin(), zzz.end(), lambda);
#endif
	return;
}
