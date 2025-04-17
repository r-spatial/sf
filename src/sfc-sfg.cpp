#include <Rcpp.h>
using namespace Rcpp;

#include <unordered_set>

// [[Rcpp::export(rng=false)]]
LogicalVector sfc_is_null(List sfc) {
	LogicalVector out(sfc.size());
	
	// An element identical to NA_logical_ or NULL is considered NULL
	// for the purposes of the sfc constructor
	SEXP item;
	
	for (R_xlen_t i = 0; i < sfc.size(); i++) {
		item = sfc[i];
		out[i] = item == R_NilValue ||
			(TYPEOF(item) == LGLSXP && 
			 Rf_length(item) == 1 && 
			 LOGICAL(item)[0] == NA_LOGICAL);
	}
	
	return out;
}

// [[Rcpp::export(rng=false)]]
List sfc_unique_sfg_dims_and_types(List sfc) {
	if (sfc.size() == 0) {
		return List::create(
			_["class_dim"] = CharacterVector::create(),
			_["class_type"] = CharacterVector::create());
	}
	
	std::unordered_set<std::string> class_dim;
	std::unordered_set<std::string> class_type;
	SEXP item;
	
	for (R_xlen_t i = 0; i < sfc.size(); i++) {
		item = sfc[i];
		
		// Because Rf_inherits() is faster than using Rf_getAttrib
		// or RObject::attr()
		if (Rf_inherits(item, "XY")) {
			class_dim.insert("XY");
		} else if (Rf_inherits(item, "XYZ")) {
			class_dim.insert("XYZ");
		} else if (Rf_inherits(item, "XYM")) {
			class_dim.insert("XYM");
		} else if (Rf_inherits(item, "XYZM")) {
			class_dim.insert("XYZM");
		}
		
		if (!Rf_inherits(item, "sfg"))
			stop("object(s) should be of class 'sfg'");

		if (Rf_inherits(item, "POINT")) {
			class_type.insert("POINT");
			continue;
		} else if (Rf_inherits(item, "LINESTRING")) {
			class_type.insert("LINESTRING");
			continue;
		} else if (Rf_inherits(item, "POLYGON")) {
			class_type.insert("POLYGON");
			continue;
		} else if (Rf_inherits(item, "MULTIPOINT")) {
			class_type.insert("MULTIPOINT");
			continue;
		} else if (Rf_inherits(item, "MULTILINESTRING")) {
			class_type.insert("MULTILINESTRING");
			continue;
		} else if (Rf_inherits(item, "MULTIPOLYGON")) {
			class_type.insert("MULTIPOLYGON");
			continue;
		}
		
		// Other types exist, too, but are more rare are not optimized yet
		RObject itemSlow(sfc[i]);
		RObject classes = itemSlow.attr("class");
		if (classes == R_NilValue) {
			continue;
		}
		
		CharacterVector classes_chr(classes);
		if (classes_chr.size() == 3) {
			class_type.insert(static_cast<std::string>(classes_chr[1]));
		}
	}
	
	CharacterVector class_dim_chr(class_dim.begin(), class_dim.end());
	CharacterVector class_type_chr(class_type.begin(), class_type.end());
	
	return List::create(
		_["class_dim"] = class_dim_chr,
		_["class_type"] = class_type_chr);
}

// [[Rcpp::export(rng=false)]]
LogicalVector sfc_is_empty(List sfc) {
	LogicalVector out(sfc.size());
	
	SEXP item;
	
	for (R_xlen_t i = 0; i < sfc.size(); i++) {
		item = sfc[i];
		int item_len = Rf_length(item);
		bool is_empty = true;
		
		if (Rf_inherits(item, "POINT")) {
			if (TYPEOF(item) == REALSXP) {
				for (int j = 0; j < item_len; j++) {
					double val = REAL(item)[j];
					if (!ISNA(val) && !ISNAN(val)) {
						is_empty = false;
						break;
					}
				}
			} else if (TYPEOF(item) == INTSXP) {
				for (int j = 0; j < item_len; j++) {
					int val = INTEGER(item)[j];
					if (val != NA_INTEGER) {
						is_empty = false;
						break;
					}
				}
			}
		} else {
			if (item_len == 0) 
				is_empty = true;
			else if (TYPEOF(item) == VECSXP) { // #2463
				item = VECTOR_ELT(item, 0); 
				is_empty = Rf_length(item) == 0 || // e.g. POLYGON with 1 ring without coordinates
						(TYPEOF(item) == VECSXP && Rf_length(VECTOR_ELT(item, 0)) == 0); // same for one level deeper, e.g. MULTIPOLYGON:
			} else
				is_empty = false;
		}
		out[i] = is_empty;
	}
	
	return out;
}

// [[Rcpp::export(rng=false)]]
LogicalVector sfc_is_full(List sfc) {
	LogicalVector out(sfc.size());
	
	SEXP item;
	
	for (R_xlen_t i = 0; i < sfc.size(); i++) {
		item = sfc[i];
		int item_len = Rf_length(item);
		bool is_full = false;
		if (item_len == 1 && Rf_inherits(item, "POLYGON")) {
			SEXP m = VECTOR_ELT(item, 0);
			if (Rf_isMatrix(m) && Rf_nrows(m) == 2) /* we can go on and check the values, but... */
				is_full = true;
		}
		out[i] = is_full;
	}
	return out;
}

// [[Rcpp::export(rng=false)]]
List points_cpp(NumericMatrix pts, CharacterVector gdim = "XY") {
	int n = pts.nrow();
	List out(n);
	CharacterVector cls = CharacterVector::create(gdim[0], "POINT", "sfg");
	for (int i = 0; i < n; i++) {

		NumericVector lp = pts(i, _);
		lp.attr("class") = cls;
		out[i] = lp;
	}
	return out;
}
