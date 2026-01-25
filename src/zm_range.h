#ifndef SF_ZM_RANGE_H_
#define SF_ZM_RANGE_H_
//Rcpp::NumericVector CPL_get_zm_range(Rcpp::List sf, int depth);

int get_m_position(Rcpp::NumericVector& pt);

int get_m_position(Rcpp::NumericMatrix& nm);

int get_z_position(Rcpp::NumericVector& pt);

int get_z_position(Rcpp::NumericMatrix& nm);

Rcpp::NumericVector CPL_get_z_range(Rcpp::List sf, int depth);

Rcpp::NumericVector CPL_get_m_range(Rcpp::List sf, int depth);
#endif // SF_ZM_RANGE_H_
