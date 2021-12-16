#' Performs block mean
#'
#' @description Given a matrix and the size of a block, returns a new matrix
#'     containing the average of each block.
#'
#' @name blockmean
#'
#' @param mat numeric matrix
#' @param block integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#'
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
#'
#' @param mat numeric matrix
#' @param block integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#'
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
#'
#' @param mat numeric matrix
#' @param block integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#'
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
#'
#' @param mat numeric matrix
#' @param block integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#'
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
#'
#' @param mat numeric matrix
#' @param block Integer vector of length 2, containing the size of the block
#' (rows, columns). If only one integer is passed, the block is square
#'
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
#'
#' @param matricesList a list of numeric matrices with same dimensions
#'
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
#'
#' @param matricesList a list of numeric matrices with same dimensions
#'
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
#'
#' @param matricesList a list of numeric matrices with same dimensions
#'
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


#' Trasform matrix into dataframe
#'
#' @description Given a matrix, returns a dataframe with three columns, where the
#' first column is the row index of the original matrix, the second column is the
#' column index of the original matrix, and the third column is the value of the
#' original matrix element.
#'
#' @name pileMatrix
#'
#' @param mat numeric matrix
#' @param subset character indicating which array elements to take. The possible choices are:
#' "full": the whole matrix (default),
#' "u": upper triangular matrix without diagonal,
#' "ud": superior trinagular matrix with diagonal,
#' "l": lower triangular matrix without diagonal,
#' "ld": lower triangular matrix with diagonal,
#' "d": only the diagonal
#'
#' @return
#' A dataframe with three columns: row index (row), col index (col) and matrix element value (value)
#' @export
pileMatrix <- function(mat, subset = "full") {
  if (class(mat)[1] != "matrix") {
    stop("mat must be a matrix")
  }

  if (subset == "full") {
    ind <- which(mat == mat, arr.ind = TRUE)
  }

  if (subset == "ud") {
    ind <- which(upper.tri(mat, diag = TRUE), arr.ind = TRUE)
  }

  if (subset == "u") {
    ind <- which(upper.tri(mat, diag = FALSE), arr.ind = TRUE)
  }

  if (subset == "ld") {
    ind <- which(lower.tri(mat, diag = TRUE), arr.ind = TRUE)
  }

  if (subset == "l") {
    ind <- which(lower.tri(mat, diag = FALSE), arr.ind = TRUE)
  }

  if (subset == "d") {
    ind <- which(mat == diag(mat), arr.ind = TRUE)
  }

  if (subset == "d" & ncol(mat) != nrow(mat)) {
    stop("matrix is not square")
  }

  if (subset == "l" & ncol(mat) != nrow(mat)) {
    stop("matrix is not square")
  }

  if (subset == "u" & ncol(mat) != nrow(mat)) {
    stop("matrix is not square")
  }

  if (subset == "ld" & ncol(mat) != nrow(mat)) {
    stop("matrix is not square")
  }

  if (subset == "ud" & ncol(mat) != nrow(mat)) {
    stop("matrix is not square")
  }

  if (!subset %in% c("full", "ud", "ld", "u", "l", "d")) {
    stop("subset: wrong flag")
  }

  if (length(dimnames(mat)) == 0) {
    nn <- list(1:nrow(mat), 1:ncol(mat))
  } else {
    if (length(dimnames(mat)[[1]]) == 0) {
      dimnames(mat)[[1]] <- 1:nrow(mat)
    }
    if (length(dimnames(mat)[[2]]) == 0) {
      dimnames(mat)[[2]] <- 1:ncol(mat)
    }
    nn <- dimnames(mat)
  }

  df <- data.frame(
    row = nn[[1]][ind[, 1]],
    col = nn[[2]][ind[, 2]],
    value = mat[ind]
  )

  return(df)
}
