% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrices_dfs.R
\name{pileMatrix}
\alias{pileMatrix}
\title{Trasform matrix into dataframe}
\usage{
pileMatrix(mat, subset = "full")
}
\arguments{
\item{mat}{numeric matrix}

\item{subset}{character indicating which array elements to take. The possible choices are:
\itemize{
  \item "full": the whole matrix (default),
  \item "u": upper triangular matrix without diagonal,
  \item "ud": upper trinagular matrix with diagonal,
  \item "l": lower triangular matrix without diagonal,
  \item "ld": lower triangular matrix with diagonal,
  \item "d": only the diagonal
}}
}
\value{
A dataframe with three columns: row index (row), col index (col) and matrix element value (value)
}
\description{
Given a matrix, returns a dataframe with three columns, where the
first column is the row index of the original matrix, the second column is the
column index of the original matrix, and the third column is the value of the
original matrix element.
}
\examples{
mat <- matrix(1:9, nrow = 3)
mat

pileMatrix(mat)

pileMatrix(mat, subset = "u")

}
