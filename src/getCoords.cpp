#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name = ".getCoords1d")]]
NumericVector getCoords1d(
    int xcell, double xmin, double xmax, NumericVector cell) {

  size_t len = cell.size();

  double scale_x1 = (xmax - xmin) / xcell;

  NumericVector result(len);

  for (size_t i = 0; i < len; i++) {
    double c = cell[i] - 1;
    result[i] = (c + 0.5) * scale_x1 + xmin;
  }

  return result;
}


// [[Rcpp::export(name = ".getCoords2d")]]
NumericMatrix getCoords2d(
    unsigned xcell, unsigned ycell,
    double xmin, double xmax, double ymin, double ymax,
    String by, NumericVector cell) {

  size_t len = cell.size();

  double scale_y1 = (ymax - ymin) / ycell;
  double scale_x1 = (xmax - xmin) / xcell;

  NumericMatrix result(len, 2);

  if (by == "h") {
    for (size_t i = 0; i < len; i++) {
      double c = cell[i] - 1;
      double row = floor(c / xcell);
      double col = c - row * xcell;
      result(i,0) = (col + 0.5) * scale_x1 + xmin;
      result(i,1) = (row + 0.5) * scale_y1 + ymin;
    }
  } else {
    for (size_t i = 0; i < len; i++) {
      double c = cell[i] - 1;
      double col = floor(c / ycell);
      double row = c - col * ycell;
      result(i,0) = (col + 0.5) * scale_x1 + xmin;
      result(i,1) = (row + 0.5) * scale_y1 + ymin;
    }
  }

  return result;
}


// [[Rcpp::export(name = ".getCoords3d")]]
NumericMatrix getCoords3d(
    int xcell, int ycell, int zcell,
    double xmin, double xmax,
    double ymin, double ymax,
    double zmin, double zmax,
    String by, NumericVector cell) {

  size_t len = cell.size();

  double scale_z1 = (zmax - zmin) / zcell;
  double scale_y1 = (ymax - ymin) / ycell;
  double scale_x1 = (xmax - xmin) / xcell;

  NumericMatrix result(len, 3);

  if (by == "h") {
    for (size_t i = 0; i < len; i++) {
      int c = cell[i] - 1;
      int col = c % xcell;
      int row = ((c - col) / xcell) % ycell;
      int layer = (((c - col) / xcell ) - row) / ycell;
      result(i,0) = (col + 0.5) * scale_x1 + xmin;
      result(i,1) = (row + 0.5) * scale_y1 + ymin;
      result(i,2) = (layer + 0.5) * scale_z1 + zmin;
    }
  } else {
    for (size_t i = 0; i < len; i++) {
      int c = cell[i] - 1;
      int layer = c % zcell;
      int row = ((c - layer) / zcell) % ycell;
      int col = (((c - layer) / zcell ) - row) / ycell;
      result(i,0) = (col + 0.5) * scale_x1 + xmin;
      result(i,1) = (row + 0.5) * scale_y1 + ymin;
      result(i,2) = (layer + 0.5) * scale_z1 + zmin;
    }
  }

  return result;
}
