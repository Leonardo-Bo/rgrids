#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name = ".getCell1d")]]
NumericVector getCell1d(
    int xcell, double xmin, double xmax, NumericVector x) {

  size_t len = x.size();

  double scale_x = xcell / (xmax - xmin);

  NumericVector result(len);

  for (size_t i = 0; i < len; i++) {

    double col = floor((x[i] - xmin) * scale_x);
    // condition on left-right border cell:
    // points in between columns assign to the column right
    // except for the last column (not exist column right)
    if (x[i] == xmax) {
      col = xcell - 1;
    }

    if (col < 0 || col >= xcell) {
      result[i] = NA_REAL;
    } else {
      result[i] = col + 1;
    }
  }
  return result;
}


// [[Rcpp::export(name = ".getCell2d")]]
NumericVector getCell2d(
    int xcell, int ycell, double xmin, double xmax, double ymin, double ymax,
    String by, NumericVector x, NumericVector y) {

  size_t len = x.size();

  double scale_y = ycell / (ymax - ymin);
  double scale_x = xcell / (xmax - xmin);

  NumericVector result(len);

  if (by == "h") {
    for (size_t i = 0; i < len; i++) {
      double row = floor((y[i] - ymin) * scale_y);
      // condition on top bottom border cell:
      // points in between rows assign to the row below
      // except for the last row (not exist row below)
      if (y[i] == ymax) {
        row = ycell - 1;
      }

      double col = floor((x[i] - xmin) * scale_x);
      // condition on left-right border cell:
      // points in between columns assign to the column right
      // except for the last column (not exist column right)
      if (x[i] == xmax) {
        col = xcell - 1;
      }

      if (row < 0 || row >= ycell || col < 0 || col >= xcell) {
        result[i] = NA_REAL;
      } else {
        // index
        result[i] = row * xcell + col + 1;
      }
    }
  } else {
    for (size_t i = 0; i < len; i++) {
      double row = floor((y[i] - ymin) * scale_y);

      if (y[i] == ymax) {
        row = ycell - 1;
      }

      double col = floor((x[i] - xmin) * scale_x);

      if (x[i] == xmax) {
        col = xcell - 1;
      }

      if (row < 0 || row >= ycell || col < 0 || col >= xcell) {
        result[i] = NA_REAL;
      } else {
        result[i] = col * ycell + row + 1;
      }
    }
  }

  return result;
}


// [[Rcpp::export(name = ".getCell3d")]]
NumericVector getCell3d(
    int xcell, int ycell, int zcell,
    double xmin, double xmax,
    double ymin, double ymax,
    double zmin, double zmax,
    String by,
    NumericVector x, NumericVector y, NumericVector z) {

  size_t len = x.size();

  double scale_z = zcell / (zmax - zmin);
  double scale_y = ycell / (ymax - ymin);
  double scale_x = xcell / (xmax - xmin);

  //IntegerVector result(len);
  NumericVector result(len);

  if (by == "h") {
    for (size_t i = 0; i < len; i++) {
      double row = floor((y[i] - ymin) * scale_y);
      // condition on top bottom border cell:
      // points in between rows assign to the row below
      // except for the last row (not exist row below)
      if (y[i] == ymax) {
        row = ycell - 1;
      }

      double col = floor((x[i] - xmin) * scale_x);
      // condition on left-right border cell:
      // points in between columns assign to the column right
      // except for the last column (not exist column right)
      if (x[i] == xmax) {
        col = xcell - 1 ;
      }

      double layer = floor((z[i] - zmin) * scale_z);
      // condition on top bottom border cell:
      // points in between layers assign to the layer below
      // except for the last layer (not exist layer below)
      if (z[i] == zmax) {
        layer = zcell - 1;
      }

      if (layer < 0 || layer >= zcell || row < 0 || row >= ycell || col < 0 || col >= xcell) {
        result[i] = NA_REAL;
      } else {
        // index
        result[i] = layer * xcell * ycell + row * xcell + col + 1;
      }
    }
  } else {
    for (size_t i = 0; i < len; i++) {
      double row = floor((y[i] - ymin) * scale_y);

      if (y[i] == ymax) {
        row = ycell - 1;
      }

      double col = floor((x[i] - xmin) * scale_x);

      if (x[i] == xmax) {
        col = xcell - 1 ;
      }

      double layer = floor((z[i] - zmin) * scale_z);

      if (z[i] == zmax) {
        layer = zcell - 1;
      }

      if (layer < 0 || layer >= zcell || row < 0 || row >= ycell || col < 0 || col >= xcell) {
        result[i] = NA_REAL;
      } else {
        // index
        result[i] = col * zcell * ycell + row * zcell + layer + 1;
      }
    }
  }

  return result;
}
