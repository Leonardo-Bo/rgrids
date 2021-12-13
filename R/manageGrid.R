#' @useDynLib rgrids
#' @importFrom Rcpp sourceCpp
NULL

#' Get grid index of each point
#'
#' @description Given a grid object (Grid1d, Grid2d or Grid3d) and a matrix or
#'     dataframe of points, it returns the index of each point in the grid.
#'     For a 1d grid it is counted from min to max; for a 2d grid it is counted
#'     from xmin, ymin to xmax, ymax increasing x or y faster according to the
#'     "by" parameter of the grid; for a 3d grid it is counted from xmin, ymin,
#'     zmin to xmax, ymax, zmax increasing x or z faster according to the "by"
#'     parameter of the grid.
#'
#' @name getCell
#'
#' @param grid Grid1d, Grid2d or Grid3d object, made with makeGrid* function
#' @param points vector for 1d Grid, matrix or dataframe of points for 2d 3d Grid
#'
#' @return
#' A vector in which the i-th element represents the index of the grid cell to
#' which the i-th point belongs

#' @examples
#' # 1. Generate random points on a plane
#' df_points <- data.frame(
#'   x = c(rnorm(n = 50000, mean = -2), rnorm(n = 50000, mean = 2)),
#'   y = c(rnorm(n = 50000, mean = 1), rnorm(n = 50000, mean = -1))
#' )
#'
#' # 2. Define a grid that contains all the points generated
#' the_grid <- makeGrid2d(
#'   xmin = floor(min(df_points$x)), ymin = floor(min(df_points$y)),
#'   xmax = ceiling(max(df_points$x)), ymax = ceiling(max(df_points$y)),
#'   xcell = 50, ycell = 50, by = "v"
#' )
#'
#' # 3. Match each point with a grid element
#' grid_index <- getCell(the_grid, df_points)
#' df_points$grid_index <- grid_index
#' @export
getCell <- function(grid, points) {
  if (class(grid)[1] != "Grid1d" & class(grid)[1] != "Grid2d" & class(grid)[1] != "Grid3d") {
    stop("grid must be a Grid1d, Grid2d or Grid3d class object")
  }

  isvector <- is.numeric(points)
  ismatrix <- is.matrix(points)
  isdf <- is.data.frame(points)

  if (class(grid)[1] == "Grid1d") {
    if (!isvector & !ismatrix & !isdf) {
      stop("points must be numeric vector, matrix or data.frame")
    }

    if (isvector & is.null(dim(points))) {
      x <- points
    }

    if (ismatrix & is.numeric(points)) {
      warning("matrix passed to a Grid1d object; only first column was selected")
      x <- points[, 1]
    }

    if (ismatrix & !is.numeric(points)) {
      stop("matrix is not numeric")
    }

    if (isdf & is.numeric(points[[1]])) {
      warning("data.frame passed to a Grid1d object; only first column was selected")
      x <- points[[1]]
    }

    if (isdf & !is.numeric(points[[1]])) {
      stop("first column of data.frame is not numeric")
    }

    object <- grid

    cell <- .getCell1d(
      object@xcell, object@xmin, object@xmax, x
      )

    return(cell)
  }

  if (class(grid)[1] == "Grid2d") {

    if (!ismatrix & !isdf) {
      stop("points must be a numeric matrix or data.frame object")
    }

    if (ismatrix & !is.numeric(points)) {
      stop("matrix is not numeric")
    }

    if (ismatrix & ncol(points) < 2) {
      stop("matrix does not have enough columns")
    }

    if (ismatrix & ncol(points) > 2) {
      warning("matrix has more than two columns. Only first two columns were selected")
      x <- points[, 1]
      y <- points[, 2]
    }

    if (ismatrix & ncol(points) == 2) {
      x <- points[, 1]
      y <- points[, 2]
    }

    if (isdf & (!is.numeric(points[[1]]) | !is.numeric(points[[2]]))) {
      stop("first two columns of data.frame are not numeric")
    }

    if (isdf & ncol(points) < 2) {
      stop("data.frame does not have enough columns")
    }

    if (isdf & ncol(points) > 2) {
      warning("data.frame has more than two columns. Only first two columns were selected")
      x <- points[[1]]
      y <- points[[2]]
    }

    if (isdf & ncol(points) == 2) {
      x <- points[[1]]
      y <- points[[2]]
    }

    object <- grid

    cell <- .getCell2d(
      object@xcell, object@ycell,
      object@xmin, object@xmax,
      object@ymin, object@ymax,
      object@by,
      x, y)

    return(cell)
  }

  if (class(grid)[1] == "Grid3d") {

    if (!ismatrix & !isdf) {
      stop("points must be a numeric matrix or data.frame object")
    }

    if (ismatrix & !is.numeric(points)) {
      stop("matrix is not numeric")
    }

    if (ismatrix & ncol(points) < 3) {
      stop("matrix does not have enough columns")
    }

    if (ismatrix & ncol(points) > 3) {
      warning("matrix has more than three columns. Only first three columns were selected")
      x <- points[, 1]
      y <- points[, 2]
      z <- points[, 3]
    }

    if (ismatrix & ncol(points) == 3) {
      x <- points[, 1]
      y <- points[, 2]
      z <- points[, 3]
    }

    if (isdf & (!is.numeric(points[[1]]) | !is.numeric(points[[2]]) | !is.numeric(points[[3]]))) {
      stop("first three columns of data.frame are not numeric")
    }

    if (isdf & ncol(points) < 3) {
      stop("data.frame does not have enough columns")
    }

    if (isdf & ncol(points) > 3) {
      warning("data.frame has more than three columns. Only first three columns were selected")
      x <- points[[1]]
      y <- points[[2]]
      z <- points[[3]]
    }

    if (isdf & ncol(points) == 3) {
      x <- points[[1]]
      y <- points[[2]]
      z <- points[[3]]
    }

    object <- grid

    cell <- .getCell3d(
      object@xcell, object@ycell, object@zcell,
      object@xmin, object@xmax,
      object@ymin, object@ymax,
      object@zmin, object@zmax,
      object@by,
      x, y, z)

    return(cell)
  }
}


#' Get point count of each cell
#'
#' @description Given a grid object (Grid1d, Grid2d or Grid3d) and an index of
#'     each cell point in the grid obtained by getCell(), it returns a dataframe
#'     with coordinates and counts for each cell. According to the "by" parameter
#'     of the grid, the coordinates will be sorted with x or z increasing faster.
#'
#' @name getCounts
#'
#' @param grid Grid1d, Grid2d or Grid3d object, made with makeGrid* function
#' @param indexCell vector of index made with getCell()
#'
#' @return A dataframe with grid coordinates and and the count of how many
#' points fall within each cell of the grid
#' @examples
#' # 1. Generate random points on a plane
#' df_points <- data.frame(
#'   x = c(rnorm(n = 50000, mean = -2), rnorm(n = 50000, mean = 2)),
#'   y = c(rnorm(n = 50000, mean = 1), rnorm(n = 50000, mean = -1))
#' )
#'
#' # 2. Define a grid that contains all the points generated
#' the_grid <- makeGrid2d(
#'   xmin = floor(min(df_points$x)), ymin = floor(min(df_points$y)),
#'   xmax = ceiling(max(df_points$x)), ymax = ceiling(max(df_points$y)),
#'   xcell = 50, ycell = 50, by = "v"
#' )
#'
#' # 3. Match each point with a grid element
#' grid_index <- getCell(the_grid, df_points)
#'
#' # 4. Count how many points there are in each element of the grid
#' df_grid <- getCounts(the_grid, grid_index)
#' @export
getCounts <- function(grid, indexCell) {
  if (class(grid)[1] != "Grid1d" & class(grid)[1] != "Grid2d" & class(grid)[1] != "Grid3d") {
    stop("grid must be a Grid1d, Grid2d or Grid3d class object")
  }

  if (class(grid)[1] == "Grid1d") {
    object <- grid
    coords <- .getCoords1d(object@xcell, object@xmin, object@xmax, 1:object@xcell)
    coords <- round(coords, 10)
    counts <- as.numeric(table(factor(indexCell, levels = 1:(object@xcell))))
    df_counts <- as.data.frame(cbind(coords, counts))
    colnames(df_counts) <- c("x", "counts")
    return(df_counts)
  }
  if (class(grid)[1] == "Grid2d") {
    object <- grid
    coords <- .getCoords2d(object@xcell, object@ycell,
                           object@xmin, object@xmax,
                           object@ymin, object@ymax,
                           object@by,
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
    counts <- as.numeric(table(factor(indexCell, levels = 1:(object@xcell * object@ycell * object@zcell))))
    df_counts <- as.data.frame(cbind(coords, counts))
    colnames(df_counts) <- c("x", "y", "z", "counts")
    return(df_counts)
  }
}


#' Get dataframe to boxplot
#'
#' @description Given a Grid1d class object and a matrix or dataframe of points,
#'     it returns a dataframe with original points and a supplementary column
#'     which groups the y points that fall in the same vertical strip.
#'
#' @name getBoxplot
#'
#' @param grid Grid1d object, made with makeGrid1d function
#' @param points 2d matrix or dataframe of points
#'
#' @return
#' A dataframe with three columns: xbp (x relative to boxplot), x and y
#' (original points)
#' @examples
#' # 1. Generate random points on a plane
#' df_points <- data.frame(
#'   x = c(rnorm(n = 50000, mean = -2), rnorm(n = 50000, mean = 2)),
#'   y = c(rnorm(n = 50000, mean = 1), rnorm(n = 50000, mean = -1))
#' )
#'
#' # 2. Define a grid that contains all the points generated along x
#' stripes <- makeGrid1d(
#'   xmin = floor(min(df_points$x)), xmax = ceiling(max(df_points$x)), xcell = 50
#' )
#'
#' # 3. Obtain boxplot dataframe
#' df_boxplot <- getBoxplot(stripes, df_points)
#' @export
getBoxplot <- function(grid, points) {
  if (class(grid)[1] != "Grid1d") {
    stop("grid must be a Grid1d class object")
  }

  ismatrix <- is.matrix(points)
  isdf <- is.data.frame(points)

  if (!ismatrix & !isdf) {
    stop("points must be a numeric matrix or data.frame object")
  }

  if (ismatrix & !is.numeric(points)) {
    stop("matrix is not numeric")
  }

  if (ismatrix & ncol(points) < 2) {
    stop("matrix does not have enough columns")
  }

  if (ismatrix & ncol(points) > 2) {
    warning("matrix has more than two columns. Only first two columns were selected")
    x <- points[, 1]
    y <- points[, 2]
  }

  if (ismatrix & ncol(points) == 2) {
    x <- points[, 1]
    y <- points[, 2]
  }

  if (isdf & (!is.numeric(points[[1]]) | !is.numeric(points[[2]]))) {
    stop("first two columns of data.frame are not numeric")
  }

  if (isdf & ncol(points) < 2) {
    stop("data.frame does not have enough columns")
  }

  if (isdf & ncol(points) > 2) {
    warning("data.frame has more than two columns. Only first two columns were selected")
    x <- points[[1]]
    y <- points[[2]]
  }

  if (isdf & ncol(points) == 2) {
    x <- points[[1]]
    y <- points[[2]]
  }

  object <- grid
  indexCell <- getCell(object, x)
  coords_grid <- .getCoords1d(object@xcell, object@xmin, object@xmax, 1:object@xcell)
  coords_grid <- round(coords_grid, 10)
  coords_points <- coords_grid[indexCell]
  coords_points <- factor(coords_points, levels = coords_grid)
  df_boxplot <- data.frame(xbp = coords_points, x = x, y = y)
  return(df_boxplot)
}
