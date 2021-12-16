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
#'
#' @examples
#' mat <- matrix(1:64, nrow = 8, byrow = TRUE)
#'
#' blockmean(mat, 2) # or blockmean(mat, c(2, 2))
#' #      [,1] [,2] [,3] [,4]
#' # [1,]  5.5  7.5  9.5 11.5
#' # [2,] 21.5 23.5 25.5 27.5
#' # [3,] 37.5 39.5 41.5 43.5
#' # [4,] 53.5 55.5 57.5 59.5
#'
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
#'
#' @examples
#' mat <- matrix(1:64, nrow = 8, byrow = TRUE)
#'
#' blockmax(mat, 2) # or blockmax(mat, c(2, 2))
#' #      [,1] [,2] [,3] [,4]
#' # [1,]   10   12   14   16
#' # [2,]   26   28   30   32
#' # [3,]   42   44   46   48
#' # [4,]   58   60   62   64
#'
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
#'
#' @examples
#' mat <- matrix(1:64, nrow = 8, byrow = TRUE)
#'
#' blockmin(mat, 2) # or blockmin(mat, c(2, 2))
#' #      [,1] [,2] [,3] [,4]
#' # [1,]    1    3    5    7
#' # [2,]   17   19   21   23
#' # [3,]   33   35   37   39
#' # [4,]   49   51   53   55
#'
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
#'
#' @examples
#' mat <- matrix(1:64, nrow = 8, byrow = TRUE)
#'
#' blocklist(mat, 4)
#' # $`1`
#' #      [,1] [,2] [,3] [,4]
#' # [1,]    1    2    3    4
#' # [2,]    9   10   11   12
#' # [3,]   17   18   19   20
#' # [4,]   25   26   27   28
#' #
#' # $`2`
#' #      [,1] [,2] [,3] [,4]
#' # [1,]    5    6    7    8
#' # [2,]   13   14   15   16
#' # [3,]   21   22   23   24
#' # [4,]   29   30   31   32
#' #
#' # $`3`
#' #      [,1] [,2] [,3] [,4]
#' # [1,]   33   34   35   36
#' # [2,]   41   42   43   44
#' # [3,]   49   50   51   52
#' # [4,]   57   58   59   60
#' #
#' # $`4`
#' #      [,1] [,2] [,3] [,4]
#' # [1,]   37   38   39   40
#' # [2,]   45   46   47   48
#' # [3,]   53   54   55   56
#' # [4,]   61   62   63   64
#'
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
#'
#' @examples
#' mat <- matrix(1:64, nrow = 8, byrow = TRUE)
#'
#' dblocklist(mat, 4)
#' # $`1`
#' #      [,1] [,2] [,3] [,4]
#' # [1,]    1    2    3    4
#' # [2,]    9   10   11   12
#' # [3,]   17   18   19   20
#' # [4,]   25   26   27   28
#' #
#' # $`2`
#' #      [,1] [,2] [,3] [,4]
#' # [1,]   37   38   39   40
#' # [2,]   45   46   47   48
#' # [3,]   53   54   55   56
#' # [4,]   61   62   63   64
#'
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
#'
#' @examples
#' mat1 <- matrix(1:4, nrow = 2)
#' mat2 <- matrix(5:8, nrow = 2)
#' mat3 <- matrix(9:12, nrow = 2)
#' list_matrix <- list(mat1, mat2, mat3)
#'
#' meanMatrix(list_matrix)
#' #      [,1] [,2]
#' # [1,]    5    7
#' # [2,]    6    8
#'
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
#'
#' @examples
#' mat1 <- matrix(1:4, nrow = 2)
#' mat2 <- matrix(5:8, nrow = 2)
#' mat3 <- matrix(9:12, nrow = 2)
#' list_matrix <- list(mat1, mat2, mat3)
#'
#' sumMatrix(list_matrix)
#' #      [,1] [,2]
#' # [1,]   15   21
#' # [2,]   18   24
#'
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
#'
#' @examples
#' mat1 <- matrix(1:4, nrow = 2)
#' mat2 <- matrix(5:8, nrow = 2)
#' mat3 <- matrix(9:12, nrow = 2)
#' list_matrix <- list(mat1, mat2, mat3)
#'
#' dotMatrix(list_matrix)
#' #      [,1] [,2]
#' # [1,]   45  231
#' # [2,]  120  384
#'
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
#'
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#'
#' pileMatrix(mat)
#' #   row col value
#' # 1   1   1     1
#' # 2   2   1     2
#' # 3   3   1     3
#' # 4   1   2     4
#' # 5   2   2     5
#' # 6   3   2     6
#' # 7   1   3     7
#' # 8   2   3     8
#' # 9   3   3     9
#'
#' pileMatrix(mat, subset = "u")
#' # row col value
#' # 1   1   2     4
#' # 2   1   3     7
#' # 3   2   3     8
#'
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
