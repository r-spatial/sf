#include <Rcpp.h>

void add_feature(SEXP &feature, SEXP &value) {
	double* p_feature = REAL(feature);
	double* p_value = REAL(value);
	int nval = LENGTH(value);
	if (Rf_isMatrix(feature)) {
		int nrow = Rf_nrows(feature);
		int ncol = Rf_ncols(feature);
		ncol = ncol > 2 ? 2 : ncol;
		for (int i = 0; i < nrow * ncol; i ++) {
			p_feature[i] = p_feature[i] + p_value[(i / nrow) % nval];
		}
	} else {
		int nfeat = LENGTH(feature);
		nfeat = nfeat > 2 ? 2 : nfeat;
		for (int i = 0; i < nfeat; i ++) {
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
		int ncol = Rf_ncols(feature);
		ncol = ncol > 2 ? 2 : ncol;
		for (int i = 0; i < nrow * ncol; i ++) {
			p_feature[i] = p_feature[i] * p_value[(i / nrow) % nval];
		}
	} else {
		int nfeat = LENGTH(feature); // # nocov start
		nfeat = nfeat > 2 ? 2 : nfeat;
		for (int i = 0; i < nfeat; i ++) {
			p_feature[i] = p_feature[i] * p_value[i % nval];
		} // # nocov end
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

void transform_bbox(SEXP &feature, SEXP &value, int mult) {
	double* p_bbox = REAL(Rf_getAttrib(feature, Rf_install("bbox")));
	double* p_value = REAL(value);
	if (mult) {
		p_bbox[0] = p_bbox[0] * p_value[0];
		p_bbox[2] = p_bbox[2] * p_value[0];
		p_bbox[1] = p_bbox[1] * p_value[1 % LENGTH(value)];
		p_bbox[3] = p_bbox[3] * p_value[1 % LENGTH(value)];
	} else {
		p_bbox[0] = p_bbox[0] + p_value[0];
		p_bbox[2] = p_bbox[2] + p_value[0];
		p_bbox[1] = p_bbox[1] + p_value[1 % LENGTH(value)];
		p_bbox[3] = p_bbox[3] + p_value[1 % LENGTH(value)];
	}
}

//[[Rcpp::export]]
SEXP opp_sfc(SEXP geom, SEXP value, SEXP mult, SEXP crs) {
	SEXP new_geom = PROTECT(Rf_duplicate(geom));

	int multiply = INTEGER(mult)[0] == 1;

	recursive_opp(new_geom, value, multiply);
	transform_bbox(new_geom, value, multiply);
	Rf_setAttrib(new_geom, Rf_install("crs"), crs);

	UNPROTECT(1);
	return new_geom;
}

//[[Rcpp::export]]
SEXP normalize_sfc(SEXP geom, SEXP min, SEXP range, SEXP crs) {
	SEXP new_geom = PROTECT(Rf_duplicate(geom));

	recursive_opp(new_geom, min, 0);
	recursive_opp(new_geom, range, 1);
	transform_bbox(new_geom, min, 0);
	transform_bbox(new_geom, range, 1);
	Rf_setAttrib(new_geom, Rf_install("crs"), crs);

	UNPROTECT(1);
	return new_geom;
}
