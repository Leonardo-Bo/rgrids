#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name = ".getCount1d")]]
NumericVector getCount1d(
    unsigned xcell, double xmin, double xmax, NumericVector cell) {

  size_t len = cell.size();

  double scale_x1 = (xmax - xmin) / xcell;

  NumericVector result(len);

  for (size_t i = 0; i < len; i++) {
    double c = cell[i] - 1;
    result[i] = (c + 0.5) * scale_x1 + xmin;
  }

  return result;
}


// [[Rcpp::export(name = ".getCount2d")]]
NumericMatrix getCount2d(
    unsigned xcell, unsigned ycell,
    double xmin, double xmax, double ymin, double ymax,
    NumericVector cell) {

  size_t len = cell.size();

  double scale_y1 = (ymax - ymin) / ycell;
  double scale_x1 = (xmax - xmin) / xcell;

  NumericMatrix result(len, 2);

  for (size_t i = 0; i < len; i++) {
    double c = cell[i] - 1;
    double row = floor(c / xcell);
    double col = c - row * xcell;
    result(i,0) = (col + 0.5) * scale_x1 + xmin;
    result(i,1) = ymax - (row + 0.5) * scale_y1;
  }

  return result;
}


// [[Rcpp::export(name = ".getCount3d")]]
NumericMatrix getCount3d(
    int xcell, int ycell, int zcell,
    double xmin, double xmax,
    double ymin, double ymax,
    double zmin, double zmax,
    NumericVector cell) {

  size_t len = cell.size();

  double scale_z1 = (zmax - zmin) / zcell;
  double scale_y1 = (ymax - ymin) / ycell;
  double scale_x1 = (xmax - xmin) / xcell;

  NumericMatrix result(len, 3);

  for (size_t i = 0; i < len; i++) {
    int c = cell[i] - 1;
    int col = c % xcell;
    int row = ((c - col) / xcell) % ycell;
    int layer = (((c - col) / xcell ) - row) / ycell;
    result(i,0) = (col + 0.5) * scale_x1 + xmin;
    result(i,1) = ymax - (row + 0.5) * scale_y1;
    result(i,2) = zmax - (layer + 0.5) * scale_z1;
  }

  return result;
}
