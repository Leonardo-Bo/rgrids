#' @useDynLib rgrids
#' @importFrom Rcpp sourceCpp
NULL

#' Get index of a point
#'
#' @description Given a grid object (Grid1d, Grid2d or Grid3d) and a matrix or
#'     dataframe of points, return the index of each point in the grid. For a
#'     1d grid it is counted from left to right; for a 2d grid it is counted
#'     from top-left corner to bottom-right corner; for a 3d grid for each layer
#'     it is counted like 2d grid and layers increase from top to bottom
#'
#' @name getCell
#' @param grid Grid1d, Grid2d or Grid3d object, made with makeGrid* function
#' @param points vector for 1d Grid, matrix or dataframe of points for 2d 3d Grid
#' @usage
#' # Call getCell
#' # getCell(Grid2d, points)
#' @return A vector of index
getCell <- function(grid, points) {
  if (class(grid)[1] == "Grid1d") {
    x <- points
    object <- grid

    cell <- .getCell1d(
      object@xcell, object@xmin, object@xmax, x
      )

    return(cell)
  }

  if (class(grid)[1] == "Grid2d") {
    x <- points[,1]
    y <- points[,2]
    object <- grid

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
    object <- grid

    cell <- .getCell3d(
      object@xcell, object@ycell, object@zcell,
      object@xmin, object@xmax,
      object@ymin, object@ymax,
      object@zmin, object@zmax,
      x, y, z)

    return(cell)
  }
}


#' Get dataframe of counts
#'
#' @description Given a grid object (Grid1d, Grid2d or Grid3d) and a index of
#'     each cell point in the grid obtained by getCell(), return a dataframe
#'     with coordinates and counts for each cell.
#'
#' @name getCounts
#' @param grid Grid1d, Grid2d or Grid3d object, made with makeGrid* function
#' @param indexCell vector of index made with getCell()
#' @usage
#' # Call getCounts
#' # getCounts(Grid2d, indexCell)
#' @return A dataframe with coordinates and counts
getCounts <- function(grid, indexCell) {
  if (class(grid)[1] == "Grid1d") {
    object <- grid
    coords <- .getCount1d(object@xcell, object@xmin, object@xmax, 1:object@xcell)
    counts <- as.numeric(table(indexCell))
    df_counts <- as.data.frame(cbind(coords, counts))
    colnames(df_counts) <- c("x", "counts")
    return(df_counts)
  }
  if (class(grid)[1] == "Grid2d") {
    object <- grid
    coords <- .getCount2d(object@xcell, object@ycell,
                          object@xmin, object@xmax,
                          object@ymin, object@ymax,
                          1:(object@xcell * object@ycell))

    counts <- as.numeric(table(indexCell))
    df_counts <- as.data.frame(cbind(coords, counts))
    colnames(df_counts) <- c("x", "y", "counts")
    return(df_counts)
  }
  if (class(grid)[1] == "Grid3d") {
    object <- grid
    coords <- .getCount3d(object@xcell, object@ycell, object@zcell,
                          object@xmin, object@xmax,
                          object@ymin, object@ymax,
                          object@zmin, object@zmax,
                          1:(object@xcell * object@ycell * object@zcell))

    counts <- as.numeric(table(indexCell))
    df_counts <- as.data.frame(cbind(coords, counts))
    colnames(df_counts) <- c("x", "y", "z", "counts")
    return(df_counts)
  }
}
