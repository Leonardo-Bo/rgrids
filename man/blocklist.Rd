% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrices.R
\name{blocklist}
\alias{blocklist}
\title{Performs list of submatrices}
\usage{
blocklist(mat, block)
}
\arguments{
\item{mat}{numeric matrix}

\item{block}{integer vector of length 2, containing the size of the block
(rows, columns). If only one integer is passed, the block is square}
}
\value{
A list of submatrices made by blocks
}
\description{
Given a matrix and the size of a block, returns a list
    containing all submatrices made by blocks.
}
\examples{
mat <- matrix(1:64, nrow = 8, byrow = TRUE)
mat

blocklist(mat, 4)

}
