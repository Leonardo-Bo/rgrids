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
#' @export
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
#' @export
getCounts <- function(grid, indexCell) {
  if (class(grid)[1] == "Grid1d") {
    object <- grid
    coords <- .getCoords1d(object@xcell, object@xmin, object@xmax, 1:object@xcell)
    coords <- round(coords, 10)
    counts <- as.numeric(table(factor(indexCell, levels = 1:(object@xcell * object@ycell))))
    df_counts <- as.data.frame(cbind(coords, counts))
    colnames(df_counts) <- c("x", "counts")
    return(df_counts)
  }
  if (class(grid)[1] == "Grid2d") {
    object <- grid
    coords <- .getCoords2d(object@xcell, object@ycell,
                           object@xmin, object@xmax,
                           object@ymin, object@ymax,
                           1:(object@xcell * object@ycell))
    coords <- round(coords, 10)
    counts <- as.numeric(table(factor(indexCell, levels = 1:(object@xcell * object@ycell))))
    df_counts <- as.data.frame(cbind(coords, counts))
    colnames(df_counts) <- c("x", "y", "counts")
    return(df_counts)
  }
  if (class(grid)[1] == "Grid3d") {
    object <- grid
    coords <- .getCoords3d(object@xcell, object@ycell, object@zcell,
                           object@xmin, object@xmax,
                           object@ymin, object@ymax,
                           object@zmin, object@zmax,
                           1:(object@xcell * object@ycell * object@zcell))
    coords <- round(coords, 10)
    counts <- as.numeric(table(factor(indexCell, levels = 1:(object@xcell * object@ycell))))
    df_counts <- as.data.frame(cbind(coords, counts))
    colnames(df_counts) <- c("x", "y", "z", "counts")
    return(df_counts)
  }
}


#' Get dataframe to boxplot
#'
#' @description Given a Grid1d class object and a 2d dataframe of points
#'     return original dataframe with supplementary column that group y points
#'     to boxplot analysis.
#'
#' @name getBoxplot
#' @param grid Grid1d object, made with makeGrid1d function
#' @param points 2d matrix or dataframe of points
#' @usage
#' # Call getBoxplot
#' # getBoxplot(Grid1d, df_points)
#' @return
#' A dataframe with three columns: xbp (x relative to boxplot), x and y
#' (original points)
#' @export
getBoxplot <- function(grid, points) {
  if (class(grid)[1] != "Grid1d") {
    stop("grid must be a Grid1d class object")
  }
  else {
    object <- grid
    indexCell <- getCell(object, points[,1])
    coords_grid <- .getCoords1d(object@xcell, object@xmin, object@xmax, 1:object@xcell)
    coords_grid <- round(coords_grid, 10)
    coords_points <- coords_grid[indexCell]
    coords_points <- factor(coords_points, levels = coords_grid)
    df_boxplot <- data.frame(xbp = coords_points, x = points[,1], y = points[,2])
    return(df_boxplot)
  }
}
