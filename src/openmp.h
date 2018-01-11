#if _OPENMP
#include <omp.h>
#else
int omp_get_thread_num();
int omp_get_max_threads();
#endif
