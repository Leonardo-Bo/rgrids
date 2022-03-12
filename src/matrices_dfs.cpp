#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name = ".triang1")]]
NumericVector triang1(int ncell) {

  int N = sqrt(ncell);
  int dl;
  int l = 0;
  NumericVector triang(N*(N-1) / 2);

  for (int j = 0; j <= (N-2); j++) {
    for (int i = 1; i <= (N-j-1); i++) {
      dl = i + j * N;
      triang[l] = dl;
      l++;
    }
  }
  return triang;
}


// [[Rcpp::export(name = ".triang2")]]
NumericVector triang2(int ncell) {

  int N = sqrt(ncell);
  int dl;
  int l = 0;
  NumericVector triang((N+1)*N / 2);

  for (int j = 0; j <= (N-1); j++) {
    for (int i = 1; i <= (N-j); i++) {
      dl = i + j * N;
      triang[l] = dl;
      l++;
    }
  }
  return triang;
}


// [[Rcpp::export(name = ".triang3")]]
NumericVector triang3(int ncell) {

  int N = sqrt(ncell);
  int dl;
  int l = 0;
  NumericVector triang(N*(N-1) / 2);

  for (int j = 1; j <= (N-1); j++) {
    for (int i = 1; i <= j; i++) {
      dl = i + j * N;
      triang[l] = dl;
      l++;
    }
  }
  return triang;
}


// [[Rcpp::export(name = ".triang4")]]
NumericVector triang4(int ncell) {

  int N = sqrt(ncell);
  int dl;
  int l = 0;
  NumericVector triang((N+1)*N / 2);

  for (int j = 0; j <= (N-1); j++) {
    for (int i = 1; i <= (j+1); i++) {
      dl = i + j * N;
      triang[l] = dl;
      l++;
    }
  }
  return triang;
}
