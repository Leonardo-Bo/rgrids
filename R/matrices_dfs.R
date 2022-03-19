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
#' \itemize{
#'   \item "full": the whole matrix (default),
#'   \item "u": upper triangular matrix without diagonal,
#'   \item "ud": upper trinagular matrix with diagonal,
#'   \item "l": lower triangular matrix without diagonal,
#'   \item "ld": lower triangular matrix with diagonal,
#'   \item "d": only the diagonal
#' }
#'
#' @return
#' A dataframe with three columns: row index (row), col index (col) and matrix element value (value)
#'
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' mat
#'
#' pileMatrix(mat)
#'
#' pileMatrix(mat, subset = "u")
#'
#' @export
pileMatrix <- function(mat, subset = "full") {
  if (class(mat)[1] != "matrix") {
    stop("mat must be a matrix")
  }

  if (ncol(mat) != nrow(mat)) {
    stop("matrix is not square")
  }

  if (!subset %in% c("full", "ud", "ld", "u", "l", "d")) {
    stop("subset: wrong flag")
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


#' Get triangular items of a grid
#'
#' @description Given the number of elements of a square grid, it returns the
#' indices of the upper or lower triangular part, with or without diagonal, both
#' in the main direction and in the mirrored direction. The item count starts
#' from the bottom left and proceeds by incrementing x ("h") or y ("v") faster.
#'
#' @name getTriang
#'
#' @param ncell integer indicating number of cells in grid
#' @param part character indicating which part of grid to take. The possible choices
#' are "upper" and "lower"
#' @param mirror boolean indicating whether to take the main direction (FALSE) or
#' the mirrored one (TRUE). Default FALSE
#' @param diag boolean indicating wheater include (TRUE) diagonal or not (FALSE).
#' Default TRUE
#' @param by character indicating whether grid indices grow faster along x ("h") or y ("v")
#'
#' @return
#' A vector of indices
#'
#' @examples
#' df_grid <- expand.grid(x = 1:4, y = 1:4)
#' df_grid
#'
#' getTriang(nrow(df_grid), part = "upper", mirror = TRUE, diag = TRUE, by = "h")
#'
#' @export
getTriang <- function(ncell, part = "upper", mirror = FALSE, diag = TRUE, by = "h") {
  cond1 <- is.null(dim(ncell))               # exlude dataframe and matrix
  cond2 <- class(ncell) != "list"            # exclude list
  cond3 <- length(ncell) == 1                # exclude vector
  cond4 <- ncell - as.integer(ncell) == 0    # check if is integer

  if (!cond1 | !cond2 | !cond3 | !cond4) {
    stop("ncell must be integer")
  }

  if (sqrt(ncell) - as.integer(sqrt(ncell)) != 0) {
    stop("ncell is not the number of items in a square matrix")
  }

  c1 <- (part == "upper" | part == "lower")
  c2 <- is.logical(mirror)
  c3 <- is.logical(diag)
  c4 <- (by == "h" | by == "v")
  if (!c1 | !c2 | !c3 | !c4) {
    stop("wrong flag")
  }

  c01 <- (part == "upper") & (mirror == TRUE) & (diag == TRUE) & (by == "h")
  c02 <- (part == "upper") & (mirror == TRUE) & (diag == TRUE) & (by == "v")
  c03 <- (part == "upper") & (mirror == FALSE) & (diag == TRUE) & (by == "h")
  c04 <- (part == "upper") & (mirror == FALSE) & (diag == TRUE) & (by == "v")
  c05 <- (part == "upper") & (mirror == TRUE) & (diag == FALSE) & (by == "h")
  c06 <- (part == "upper") & (mirror == TRUE) & (diag == FALSE) & (by == "v")
  c07 <- (part == "upper") & (mirror == FALSE) & (diag == FALSE) & (by == "h")
  c08 <- (part == "upper") & (mirror == FALSE) & (diag == FALSE) & (by == "v")
  c09 <- (part == "lower") & (mirror == TRUE) & (diag == TRUE) & (by == "h")
  c10 <- (part == "lower") & (mirror == TRUE) & (diag == TRUE) & (by == "v")
  c11 <- (part == "lower") & (mirror == FALSE) & (diag == TRUE) & (by == "h")
  c12 <- (part == "lower") & (mirror == FALSE) & (diag == TRUE) & (by == "v")
  c13 <- (part == "lower") & (mirror == TRUE) & (diag == FALSE) & (by == "h")
  c14 <- (part == "lower") & (mirror == TRUE) & (diag == FALSE) & (by == "v")
  c15 <- (part == "lower") & (mirror == FALSE) & (diag == FALSE) & (by == "h")
  c16 <- (part == "lower") & (mirror == FALSE) & (diag == FALSE) & (by == "v")

  v_cells <- 1:ncell

  if (c15 | c16 | c03 | c04) {
    p_indices <- .triang1(ncell)

    if (c15 | c16) {
      indices <- v_cells[v_cells %in% p_indices]
    }

    if (c03 | c04) {
      indices <- v_cells[!(v_cells %in% p_indices)]
    }
  }


  if (c11 | c12 | c07 | c08) {
    p_indices <- .triang2(ncell)

    if (c11 | c12) {
      indices <- v_cells[v_cells %in% p_indices]
    }

    if (c07 | c08) {
      indices <- v_cells[!(v_cells %in% p_indices)]
    }
  }


  if (c05 | c14 | c09 | c02) {
    p_indices <- .triang3(ncell)

    if (c05 | c14) {
      indices <- v_cells[v_cells %in% p_indices]
    }

    if (c09 | c02) {
      indices <- v_cells[!(v_cells %in% p_indices)]
    }
  }


  if (c01 | c10 | c13 | c06) {
    p_indices <- .triang4(ncell)

    if (c01 | c10) {
      indices <- v_cells[v_cells %in% p_indices]
    }

    if (c13 | c06) {
      indices <- v_cells[!(v_cells %in% p_indices)]
    }
  }

  return(indices)

}
