#' Performs block mean
#'
#' @description Given a matrix and the size of a block, returns a new matrix
#'     containing the average of each block.
#'
#' @name blockmean
#' @param mat Matrix object
#' @param block Integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#' @usage
#' # Call blockmean
#' # blockmean(mat, block)
#' @return
#' A new reduced matrix with mean of each block
#' @export
blockmean <- function(mat, block) {
  if (class(mat)[1] != "matrix") {
    stop("mat must be a matrix")
  }
  if (any((block - as.integer(block)) != 0)) {
    stop("block values must be integer")
  }
  if (length(block) == 1) {
    br <- block
    bc <- block
  } else {
    br <- block[1]
    bc <- block[2]
  }
  if ((nrow(mat)%%br != 0) || (ncol(mat)%%bc != 0)) {
    stop("rows and columns must be multiple of respective block dimensions")
  }

  block_mean <- .blockmean(mat, nrow(mat), br, ncol(mat), bc)
  block_mean <- matrix(block_mean, nrow(mat)/br, byrow = TRUE)
  return(block_mean)
}


#' Performs block max
#'
#' @description Given a matrix and the size of a block, returns a new matrix
#'     containing the max value for each block.
#'
#' @name blockmax
#' @param mat Matrix object
#' @param block Integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#' @usage
#' # Call blockmax
#' # blockmax(mat, block)
#' @return
#' A new reduced matrix with max value of each block
#' @export
blockmax <- function(mat, block) {
  if (class(mat)[1] != "matrix") {
    stop("mat must be a matrix")
  }
  if (any((block - as.integer(block)) != 0)) {
    stop("block values must be integer")
  }
  if (length(block) == 1) {
    br <- block
    bc <- block
  } else {
    br <- block[1]
    bc <- block[2]
  }
  if ((nrow(mat)%%br != 0) || (ncol(mat)%%bc != 0)) {
    stop("rows and columns must be multiple of respective block dimensions")
  }

  block_max <- .blockmax(mat, nrow(mat), br, ncol(mat), bc)
  block_max <- matrix(block_max, nrow(mat)/br, byrow = TRUE)
  return(block_max)
}


#' Performs block min
#'
#' @description Given a matrix and the size of a block, returns a new matrix
#'     containing the min value for each block.
#'
#' @name blockmin
#' @param mat Matrix object
#' @param block Integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#' @usage
#' # Call blockmin
#' # blockmin(mat, block)
#' @return
#' A new reduced matrix with min value of each block
#' @export
blockmin <- function(mat, block) {
  if (class(mat)[1] != "matrix") {
    stop("mat must be a matrix")
  }
  if (any((block - as.integer(block)) != 0)) {
    stop("block values must be integer")
  }
  if (length(block) == 1) {
    br <- block
    bc <- block
  } else {
    br <- block[1]
    bc <- block[2]
  }
  if ((nrow(mat)%%br != 0) || (ncol(mat)%%bc != 0)) {
    stop("rows and columns must be multiple of respective block dimensions")
  }

  block_min <- .blockmin(mat, nrow(mat), br, ncol(mat), bc)
  block_min <- matrix(block_min, nrow(mat)/br, byrow = TRUE)
  return(block_min)
}
