% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classGrid.R
\docType{class}
\name{makeGrid1d}
\alias{makeGrid1d}
\title{Grid1d class}
\value{
A uni-dimensional grid
}
\description{
Given the extremes of the range and the number of cells, it
    generates a one-dimensional grid. Cells are counted from minimum to
    maximum (left to right).
}
\section{Slots}{

\describe{
\item{\code{xcell}}{numeric, number of cells}

\item{\code{xmin}}{numeric, lower limit}

\item{\code{xmax}}{numeric, upper limit}
}}

\examples{
grid1d <- makeGrid1d(xmin = -50, xmax = 50, xcell = 100)
grid1d

makeGrid1d()

}
