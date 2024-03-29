#' Convert R table in LaTeX table
#'
#' @description Given a matrix, a data.frame, a tibble or a data.table
#'     returns a basic LaTeX table write with table and tabular packages.
#'     Rownames and colnames are highlighted with bold.
#'
#' @name tableToLatex
#' @param object A matrix, data.frame, tibble or data.table
#' @param digits Number of decimals. Default = 3
#' @param file If TRUE write file with object name in work directory. Default FALSE
#' @param double_space If TRUE add empty columns to increase space between columns
#'
#' @return
#' Text on terminal or on a file to copy in LaTeX environment
#'
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' mat
#'
#' tableToLatex(mat)
#'
#' @export
tableToLatex <- function(object, digits = 3, file = FALSE, double_space = FALSE) {

  if (!any(class(object) == "data.frame") & !any(class(object) == "matrix")) {
    stop("object must be matrix, data.frame, data.table or tibble")
  }

  df <- as.data.frame(object)
  numeric_columns <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[, numeric_columns] <- format(round(df[, numeric_columns], digits), nsmall = digits)
  mat <- as.matrix(df)

  header <- "\\begin{table}[htbp] \n\\centering \n"

  ## NULL rownames and colnames
  if (is.null(colnames(mat)) & is.null(rownames(mat))) {
    if (double_space) {
      n_col <- paste(rep("c", (ncol(mat)*2)-1), collapse = "")
      tabular <- paste("\\begin{tabular}{", n_col, "} \n", sep = "")

      body_half1 <- paste(mat[, 1:(ncol(mat)-1)], "& &")

    } else {
      n_col <- paste(rep("c", ncol(mat)), collapse = "")
      tabular <- paste("\\begin{tabular}{", n_col, "} \n", sep = "")

      body_half1 <- paste(mat[, 1:(ncol(mat)-1)], "&")
    }
  }

  ## not NULL colnames
  if (!is.null(colnames(mat)) & is.null(rownames(mat))) {
    mat <- rbind(colnames(mat), mat)
    colnames(mat) <- NULL

    mat[1, ] <- paste("\\textbf{", mat[1, ], "}")

    if (double_space) {
      n_col <- paste(rep("c", (ncol(mat)*2)-1), collapse = "")
      tabular <- paste("\\begin{tabular}{", n_col, "} \n", sep = "")

      body_half1 <- paste(mat[, 1:(ncol(mat)-1)], "& &")

    } else {
      n_col <- paste(rep("c", ncol(mat)), collapse = "")
      tabular <- paste("\\begin{tabular}{", n_col, "} \n", sep = "")

      body_half1 <- paste(mat[, 1:(ncol(mat)-1)], "&")
    }
  }

  ## not NULL rownames
  if (is.null(colnames(mat)) & !is.null(rownames(mat))) {
    mat <- cbind(` ` = rownames(mat), mat)
    rownames(mat) <- NULL

    mat[, 1] <- paste("\\textbf{", mat[, 1], "}")

    if (double_space) {
      n_col <- paste(rep("c", (ncol(mat)*2)-1), collapse = "")
      tabular <- paste("\\begin{tabular}{", n_col, "} \n", sep = "")

      body_half1 <- paste(mat[, 1:(ncol(mat)-1)], "& &")

    } else {
      n_col <- paste(rep("c", ncol(mat)), collapse = "")
      tabular <- paste("\\begin{tabular}{", n_col, "} \n", sep = "")

      body_half1 <- paste(mat[, 1:(ncol(mat)-1)], "&")
    }
  }

  ## not NULL rownames and colnames
  if (!is.null(colnames(mat)) & !is.null(rownames(mat))) {
    mat <- cbind(` ` = rownames(mat), mat)
    rownames(mat) <- NULL

    mat <- rbind(colnames(mat), mat)
    colnames(mat) <- NULL

    mat[1, 2:ncol(mat)] <- paste("\\textbf{", mat[1, 2:ncol(mat)], "}")
    mat[2:nrow(mat), 1] <- paste("\\textbf{", mat[2:nrow(mat), 1], "}")

    if (double_space) {
      n_col <- paste(rep("c", (ncol(mat)*2)-1), collapse = "")
      tabular <- paste("\\begin{tabular}{", n_col, "} \n", sep = "")

      body_half1 <- paste(mat[, 1:(ncol(mat)-1)], "& &")

    } else {
      n_col <- paste(rep("c", ncol(mat)), collapse = "")
      tabular <- paste("\\begin{tabular}{", n_col, "} \n", sep = "")

      body_half1 <- paste(mat[, 1:(ncol(mat)-1)], "&")
    }
  }

  body_half2 <- paste(mat[, ncol(mat)], "\\\\ \n")
  body_col <- c(body_half1, body_half2)

  body_char_mat <- matrix(body_col, ncol = ncol(mat))
  body <- apply( body_char_mat[ , 1:ncol(body_char_mat) ] , 1 , paste , collapse = " ")

  footer <- "\\end{tabular} \n\\caption{} \n\\label{} \n\\end{table}"

  if (file) {
    latex <- cat(c(header, tabular, body, footer),
                 file = paste(deparse(substitute(object)), ".txt", sep = ""))
  }
  else {
    latex <- cat(c(header, tabular, body, footer))
  }
}
