#include <Rcpp.h>

void add_feature(SEXP &feature, SEXP &value) {
	double* p_feature = REAL(feature);
	double* p_value = REAL(value);
	int nval = LENGTH(value);
	if (Rf_isMatrix(feature)) {
		int nrow = Rf_nrows(feature);
		for (int i = 0; i < LENGTH(feature); i ++) {
			p_feature[i] = p_feature[i] + p_value[(i / nrow) % nval];
		}
	} else {
		for (int i = 0; i < LENGTH(feature); i ++) {
			p_feature[i] = p_feature[i] + p_value[i % nval];
		}
	}
}
void mult_feature(SEXP &feature, SEXP &value) {
	double* p_feature = REAL(feature);
	double* p_value = REAL(value);
	int nval = LENGTH(value);
	if (Rf_isMatrix(feature)) {
		int nrow = Rf_nrows(feature);
		for (int i = 0; i < LENGTH(feature); i ++) {
			p_feature[i] = p_feature[i] * p_value[(i / nrow) % nval];
		}
	} else {
		for (int i = 0; i < LENGTH(feature); i ++) {
			p_feature[i] = p_feature[i] * p_value[i % nval];
		}
	}
}

void recursive_opp(SEXP &feature, SEXP &value, int mult) {
	if (Rf_isVectorList(feature)) {
		for (int i = 0; i < LENGTH(feature); i++) {
			SEXP next_feature = VECTOR_ELT(feature, i);
			if (Rf_isInteger(next_feature)) {
				SEXP num_feature = PROTECT(Rf_coerceVector(next_feature, REALSXP));
				DUPLICATE_ATTRIB(num_feature, next_feature);
				next_feature = SET_VECTOR_ELT(feature, i, num_feature);
				UNPROTECT(1);
			}
			recursive_opp(next_feature, value, mult);
		}
	} else {
		if (mult) {
			mult_feature(feature, value);
		} else {
			add_feature(feature, value);
		}
	}
}


//[[Rcpp::export]]
SEXP opp_sfg(SEXP geom, SEXP value, SEXP mult) {
	SEXP new_geom = PROTECT(Rf_duplicate(geom));

	int multiply = INTEGER(mult)[0] == 1;

	recursive_opp(new_geom, value, multiply);

	UNPROTECT(1);
	return new_geom;
}
