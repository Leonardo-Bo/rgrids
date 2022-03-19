#' Find max for each row
#'
#' @description Given a matrix, it returns a dataframe with two column:
#'     - first column are max values for each row of the original matrix
#'     - second column are indices of max values
#'
#' @name rowMax
#'
#' @param mat numeric matrix
#'
#' @return
#' A dataframe with values and indices of max for each row
#'
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' mat
#'
#' rowMax(mat)
#'
#' @export
rowMax <- function(mat) {
  c0 <- is.matrix(mat)
  c1 <- is.numeric(mat)
  if (!(c0 & c1)) {
    stop("mat must be a numeric matrix")
  }
  rowMax <- .rowMax(mat)
  df_rowMax <- data.frame(rowMax)
  colnames(df_rowMax) <- c("value", "index")
  return(df_rowMax)
}


#' Find min for each row
#'
#' @description Given a matrix, it returns a dataframe with two column:
#'     - first column are min values for each row of the original matrix
#'     - second column are indices of min values
#'
#' @name rowMin
#'
#' @param mat Matrix object
#'
#' @return
#' A dataframe with values and indices of min for each row
#'
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' mat
#'
#' rowMin(mat)
#'
#' @export
rowMin <- function(mat) {
  c0 <- is.matrix(mat)
  c1 <- is.numeric(mat)
  if (!(c0 & c1)) {
    stop("mat must be a numeric matrix")
  }
  rowMin <- .rowMin(mat)
  df_rowMin <- data.frame(rowMin)
  colnames(df_rowMin) <- c("value", "index")
  return(df_rowMin)
}


#' Find max for each column
#'
#' @description Given a matrix, it returns a dataframe with two column:
#'     - first column are max values for each column of the original matrix
#'     - second column are indices of max values
#'
#' @name colMax
#'
#' @param mat Matrix object
#'
#' @return
#' A dataframe with values and indices of max for each column
#'
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' mat
#'
#' colMax(mat)
#'
#' @export
colMax <- function(mat) {
  c0 <- is.matrix(mat)
  c1 <- is.numeric(mat)
  if (!(c0 & c1)) {
    stop("mat must be a numeric matrix")
  }
  colMax <- .colMax(mat)
  df_colMax <- data.frame(colMax)
  colnames(df_colMax) <- c("value", "index")
  return(df_colMax)
}


#' Find min for each column
#'
#' @description Given a matrix, it returns a dataframe with two column:
#'     - first column are min values for each column of the original matrix
#'     - second column are indices of min values
#'
#' @name colMin
#'
#' @param mat Matrix object
#'
#' @return
#' A dataframe with values and indices of min for each column
#'
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' mat
#'
#' colMin(mat)
#'
#' @export
colMin <- function(mat) {
  c0 <- is.matrix(mat)
  c1 <- is.numeric(mat)
  if (!(c0 & c1)) {
    stop("mat must be a numeric matrix")
  }
  colMin <- .colMin(mat)
  df_colMin <- data.frame(colMin)
  colnames(df_colMin) <- c("value", "index")
  return(df_colMin)
}
