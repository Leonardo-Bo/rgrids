#' @useDynLib rgrids
#' @importFrom Rcpp sourceCpp
NULL


getCell <- function(object, points) {
  if (class(object)[1] == "Grid1d") {
    x <- points[,1]

    cell <- .getCell1d(
      object@xcell, object@xmin, object@xmax, x
      )

    return(cell)
  }

  if (class(object)[1] == "Grid2d") {
    x <- points[,1]
    y <- points[,2]

    cell <- .getCell2d(
      object@xcell, object@ycell,
      object@xmin, object@xmax,
      object@ymin, object@ymax,
      x, y)

    return(cell)
  }

  if (class(object)[1] == "Grid3d") {
    x <- points[,1]
    y <- points[,2]
    z <- points[,3]

    cell <- .getCell3d(
      object@xcell, object@ycell, object@zcell,
      object@xmin, object@xmax,
      object@ymin, object@ymax,
      object@zmin, object@zmax,
      x, y, z)
  }
}


# getCounts <- function() {
#   x <- NULL
# }
