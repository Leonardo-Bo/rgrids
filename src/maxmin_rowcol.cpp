#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name = ".rowMax")]]
NumericMatrix rowMax(NumericMatrix mat) {
  R_xlen_t nr = mat.nrow(), nc = mat.ncol();
  NumericMatrix result(nr, 2);

  for(R_xlen_t i = 0; i < nr; i++) {
    double current = mat(i, 0);
    R_xlen_t idx = 0;
    for (R_xlen_t j = 1; j < nc; j++) {
      if (mat(i, j) > current) {
        current = mat(i, j);
        idx = j;
      }
    }
    result(i, 0) = current;
    result(i, 1) = idx + 1;
  }
  return result;
}


// [[Rcpp::export(name = ".rowMin")]]
NumericMatrix rowMin(NumericMatrix mat) {
  R_xlen_t nr = mat.nrow(), nc = mat.ncol();
  NumericMatrix result(nr, 2);

  for(R_xlen_t i = 0; i < nr; i++) {
    double current = mat(i, 0);
    R_xlen_t idx = 0;
    for (R_xlen_t j = 1; j < nc; j++) {
      if (mat(i, j) < current) {
        current = mat(i, j);
        idx = j;
      }
    }
    result(i, 0) = current;
    result(i, 1) = idx + 1;
  }
  return result;
}


// [[Rcpp::export(name = ".colMax")]]
NumericMatrix colMax(NumericMatrix mat) {
  R_xlen_t nr = mat.nrow(), nc = mat.ncol();
  NumericMatrix result(nc, 2);

  for(R_xlen_t i = 0; i < nc; i++) {
    double current = mat(0, i);
    R_xlen_t idx = 0;
    for (R_xlen_t j = 1; j < nr; j++) {
      if (mat(j, i) > current) {
        current = mat(j, i);
        idx = j;
      }
    }
    result(i, 0) = current;
    result(i, 1) = idx + 1;
  }
  return result;
}


// [[Rcpp::export(name = ".colMin")]]
NumericMatrix colMin(NumericMatrix mat) {
  R_xlen_t nr = mat.nrow(), nc = mat.ncol();
  NumericMatrix result(nc, 2);

  for(R_xlen_t i = 0; i < nc; i++) {
    double current = mat(0, i);
    R_xlen_t idx = 0;
    for (R_xlen_t j = 1; j < nr; j++) {
      if (mat(j, i) < current) {
        current = mat(j, i);
        idx = j;
      }
    }
    result(i, 0) = current;
    result(i, 1) = idx + 1;
  }
  return result;
}
