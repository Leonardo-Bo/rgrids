#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name = ".blockmean")]]
NumericVector blockmean(
    NumericMatrix matrix, int width, int block_width,
    int height, int block_height) {

  NumericVector mean_matrix((width/block_width) * (height/block_height));
  double average;
  int cell_index = 0;
  for(size_t bx = 0; bx < width;  bx += block_width) {
    for(size_t by = 0; by < height; by += block_height) {
      double sum = 0;
      for(size_t x = 0; x < block_width; ++x) {
        for(size_t y = 0; y < block_height; ++y) {
          sum += matrix(bx + x, by + y);
        }
      }
      average = sum / (block_width * block_height);

      mean_matrix[cell_index] = average;
      cell_index++;
    }
  }
  return mean_matrix;
}


// [[Rcpp::export(name = ".blockmax")]]
NumericVector blockmax(
    NumericMatrix matrix, int width, int block_width,
    int height, int block_height) {

  NumericVector max_matrix((width/block_width) * (height/block_height));
  double maximum;
  int cell_index = 0;
  for(size_t bx = 0; bx < width;  bx += block_width) {
    for(size_t by = 0; by < height; by += block_height) {
      double maximum = matrix(bx, by);
      for(size_t x = 0; x < block_width; ++x) {
        for(size_t y = 0; y < block_height; ++y) {
          maximum = std::max(maximum, matrix(bx + x, by + y));
        }
      }
      max_matrix[cell_index] = maximum;
      cell_index++;
    }
  }
  return max_matrix;
}


// [[Rcpp::export(name = ".blockmin")]]
NumericVector blockmin(
    NumericMatrix matrix, int width, int block_width,
    int height, int block_height) {

  NumericVector min_matrix((width/block_width) * (height/block_height));
  double minimum;
  int cell_index = 0;
  for(size_t bx = 0; bx < width;  bx += block_width) {
    for(size_t by = 0; by < height; by += block_height) {
      double minimum = matrix(bx, by);
      for(size_t x = 0; x < block_width; ++x) {
        for(size_t y = 0; y < block_height; ++y) {
          minimum = std::min(minimum, matrix(bx + x, by + y));
        }
      }
      min_matrix[cell_index] = minimum;
      cell_index++;
    }
  }
  return min_matrix;
}
