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


#' Performs list of submatrices
#'
#' @description Given a matrix and the size of a block, returns a list
#'     containing all submatrices made by blocks.
#'
#' @name blocklist
#' @param mat Matrix object
#' @param block Integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#' @usage
#' # Call blocklist
#' # blockmin(mat, block)
#' @return
#' A list of submatrices made by blocks
#' @export
blocklist <- function(mat, block) {
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

  row_res <- nrow(mat)/br
  col_res <- ncol(mat)/bc
  mask <- matrix(1:(row_res*col_res), row_res, col_res, byrow = TRUE) %x% matrix(1, br, bc)
  block_list <- lapply(split(mat, mask), matrix, nr = br)
  return(block_list)
}


#' Performs list of diagonal submatrices
#'
#' @description Given a matrix and the size of a block, returns a list
#'     containing all diagonal submatrices made by blocks.
#'
#' @name dblocklist
#' @param mat Matrix object
#' @param block Integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#' @usage
#' # Call dblocklist
#' # dblockmin(mat, block)
#' @return
#' A list of diangonal submatrices made by blocks
#' @export
dblocklist <- function(mat, block) {
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
  if ((nrow(mat) == br) || (ncol(mat) == bc)) {
    stop("you cannot define a block that covers the entire matrix")
  }

  row_res <- nrow(mat)/br
  col_res <- ncol(mat)/bc

  if (row_res != col_res) {
    stop("defined blocks do not cover the whole diagonal")
  }

  mask <- diag(1:(row_res)) %x% matrix(1, br, bc)
  block_list_diagonal <- lapply(split(mat, mask)[-1], matrix, nr = br)
  return(block_list_diagonal)
}


#' Performs mean of matrices or vectors
#'
#' @description Given a list of matrices, returns a single matrix in which
#'     element (i, j) is the average of all corresponding elements (i, j) of
#'     all matrices in the list.
#'     Given a list of vectors, returns a single vector in which element i is
#'     the average of all corresponding elements i of all vectors in the list.
#'
#' @name meanMatrix
#' @param matricesList A list of numeric matrices with same dimensions
#' @usage
#' # Call meanMatrix
#' # meanMatrix(matricesList)
#' @return
#' A matrix in which element (i,j) is the average of all corresponding elements
#' (i,j) of all matrices in the original list
#' @export
meanMatrix <- function(matricesList) {
  c0 <- typeof(matricesList) == "list"
  c1 <- length(unique(lapply(matricesList, dim))) == 1 | length(unique(lapply(matricesList, length))) == 1
  c2 <- all(lapply(matricesList, typeof) == "integer" | lapply(matricesList, typeof) == "double")
  if (!c0) {
    stop("matricesList must be a list of matrices or vectors")
  }
  if (!c1) {
    stop("all matrices or vectors must have the same dimensions")
  }
  if (!c2) {
    stop("all list elements must be numeric matrices or vectors")
  }

  sumM <- Reduce(`+`, matricesList)
  meanM <- sumM / length(matricesList)
  return(meanM)
}


#' Performs sum of matrices or vectors
#'
#' @description Given a list of matrices, returns a single matrix in which
#'     element (i, j) is the sum of all corresponding elements (i, j) of
#'     all matrices in the list.
#'     Given a list of vectors, returns a single vector in which element i is
#'     the sum of all corresponding elements i of all vectors in the list.
#'
#' @name sumMatrix
#' @param matricesList A list of numeric matrices with same dimensions
#'
#' @usage
#' # Call sumMatrix
#' # sumMatrix(matricesList)
#' @return
#' A matrix in which element (i,j) is the sum of all corresponding elements
#' (i,j) of all matrices in the original list
#' @export
sumMatrix <- function(matricesList) {
  c0 <- typeof(matricesList) == "list"
  c1 <- length(unique(lapply(matricesList, dim))) == 1 | length(unique(lapply(matricesList, length))) == 1
  c2 <- all(lapply(matricesList, typeof) == "integer" | lapply(matricesList, typeof) == "double")
  if (!c0) {
    stop("matricesList must be a list of matrices or vectors")
  }
  if (!c1) {
    stop("all matrices or vectors must have the same dimensions")
  }
  if (!c2) {
    stop("all list elements must be numeric matrices or vectors")
  }

  sumM <- Reduce(`+`, matricesList)
  return(sumM)
}


#' Performs product of matrices or vectors
#'
#' @description Given a list of matrices, returns a single matrix in which
#'     element (i, j) is the product of all corresponding elements (i, j) of
#'     all matrices in the list.
#'     Given a list of vectors, returns a single vector in which element i is
#'     the product of all corresponding elements i of all vectors in the list.
#'
#' @name dotMatrix
#' @param matricesList A list of numeric matrices with same dimensions
#'
#' @usage
#' # Call dotMatrix
#' # dotMatrix(matricesList)
#' @return
#' A matrix in which element (i,j) is the product of all corresponding elements
#' (i,j) of all matrices in the original list
#' @export
dotMatrix <- function(matricesList) {
  c0 <- typeof(matricesList) == "list"
  c1 <- length(unique(lapply(matricesList, dim))) == 1 | length(unique(lapply(matricesList, length))) == 1
  c2 <- all(lapply(matricesList, typeof) == "integer" | lapply(matricesList, typeof) == "double")
  if (!c0) {
    stop("matricesList must be a list of matrices or vectors")
  }
  if (!c1) {
    stop("all matrices or vectors must have the same dimensions")
  }
  if (!c2) {
    stop("all list elements must be numeric matrices or vectors")
  }

  dotM <- Reduce(`*`, matricesList)
  return(dotM)
}
