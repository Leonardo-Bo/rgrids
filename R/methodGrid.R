#' @import methods

setGeneric("makeGrid1d", function(x, ...) standardGeneric("makeGrid1d"))
setGeneric("makeGrid2d", function(x, ...) standardGeneric("makeGrid2d"))
setGeneric("makeGrid3d", function(x, ...) standardGeneric("makeGrid3d"))

#' Method for Grid1d class
#'
#' @description Given the extremes of the range and the number of cells, it
#'     generates a one-dimensional grid.
#' @name makeGrid1d
#' @param xcell numeric, number of cells
#' @param xmin numeric, lower limit
#' @param xmax numeric, upper limit
#' @usage
#' # Create S4 object with the following default values
#' # makeGrid1d(xmin = -5, xmax = 5, xcell = 100)
#' @return A uni-dimensional grid
#' @examples
#' grid1d <- makeGrid1d(xmin = -50, xmax = 50, xcell = 100)
#' grid1d
#' > class      : Grid1d
#' > dimensions : xcell = 100
#' > range      : xmin = -50, xmax = 50
#' @rdname makeGrid1d
#'
#' @export
setMethod("makeGrid1d",
          signature(x='missing'),
          function(xcell = 100, xmin = -5, xmax = 5) {
            xcell <- xcell
            xmin <- xmin
            xmax <- xmax
            grid <- methods::new("Grid1d", xcell = xcell, xmin = xmin, xmax = xmax)
          }
)


#' Method for Grid2d class
#'
#' @description Given the extremes of the range and the number of cells along x
#'     and y, it generates a two-dimensional grid.
#'
#' @name makeGrid2d
#' @param xcell numeric, number of cells along x
#' @param ycell numeric, number of cells along y
#' @param xmin numeric, lower limit along x
#' @param xmax numeric, upper limit along x
#' @param ymin numeric, lower limit along y
#' @param ymax numeric, upper limit along y
#'
#' @usage
#' # Create S4 object with the following default values
#' # makeGrid1d(xmin = -5, xmax = 5,
#'              ymin = -5, ymax = 5,
#'              xcell = 100, ycell = 100)
#' @return A two-dimensional grid
#' @examples
#' grid2d <- makeGrid2d(xmin = -50, xmax = 50,
#'                      ymin = -50, ymax = 50,
#'                      xcell = 100, ycell = 100)
#' grid2d
#' > class      : Grid2d
#' > dimensions : xcell = 100, ycell = 100, ncell = 10000
#' > range      : xmin = -50, xmax = 50
#'                ymin = -50, ymax = 50
#' @rdname makeGrid2d
#'
#' @export
setMethod("makeGrid2d",
          signature(x='missing'),
          function(xcell = 100, ycell = 100, xmin = -5, xmax = 5, ymin = -5, ymax = 5) {
            xcell <- xcell
            ycell <- ycell
            xmin <- xmin
            xmax <- xmax
            ymin <- ymin
            ymax <- ymax
            grid <- methods::new("Grid2d", xcell = xcell, ycell = ycell, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
          }
)


#' Method for Grid3d class
#'
#' @description Given the extremes of the range and the number of cells along x
#'     y, and z it generates a three-dimensional grid.
#'
#' @name makeGrid3d
#' @param xcell numeric, number of cells along x
#' @param ycell numeric, number of cells along y
#' @param zcell numeric, number of cells along z
#' @param xmin numeric, lower limit along x
#' @param xmax numeric, upper limit along x
#' @param ymin numeric, lower limit along y
#' @param ymax numeric, upper limit along y
#' @param zmin numeric, lower limit along z
#' @param zmax numeric, upper limit along z
#'
#' @usage
#' # Create S4 object with the following default values
#' # makeGrid1d(xmin = -5, xmax = 5,
#'              ymin = -5, ymax = 5,
#'              zmin = -5, zmax = 5,
#'              xcell = 100, ycell = 100, zcell = 100)
#' @return A three-dimensional grid
#' @examples
#' grid3d <- makeGrid3d(xmin = -50, xmax = 50,
#'                      ymin = -50, ymax = 50,
#'                      zmin = -50, zmax = 50,
#'                      xcell = 100, ycell = 100, zcell = 100)
#' grid3d
#' > class      : Grid3d
#' > dimensions : xcell = 100, ycell = 100, zcell = 100, ncell = 1e6
#' > range      : xmin = -50, xmax = 50
#'                ymin = -50, ymax = 50
#'                zmin = -50, zmax = 50
#' @rdname makeGrid3d
#'
#' @export
setMethod("makeGrid3d",
          signature(x='missing'),
          function(xcell = 100, ycell = 100, zcell = 100, xmin = -5, xmax = 5, ymin = -5, ymax = 5, zmin = -5, zmax = 5) {
            xcell <- xcell
            ycell <- ycell
            zcell <- zcell
            xmin <- xmin
            xmax <- xmax
            ymin <- ymin
            ymax <- ymax
            zmin <- zmin
            zmax <- zmax
            grid <- methods::new("Grid3d", xcell = xcell, ycell = ycell, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, zmin = zmin, zmax = zmax)
          }
)


setMethod("show",
          "Grid1d",
          function(object) {
            cat("class      : ", class(object), "\n", sep = "")
            cat("dimensions : ", "xcell = ", object@xcell, "\n", sep = "")
            cat("range      : ", "xmin = ", object@xmin, ", xmax = ", object@xmax, "\n", sep = "")
          }
)


setMethod("show",
          "Grid2d",
          function(object) {
            cat("class      : ", class(object), "\n", sep = "")
            cat("dimensions : ", "xcell = ", object@xcell,
                                 ", ycell = ", object@ycell,
                                 ", ncell = ", object@xcell * object@ycell, "\n", sep = "")
            cat("range      : ", "xmin = ", object@xmin, ", xmax = ", object@xmax, "\n",
                "             ymin = ", object@ymin, ", ymax = ", object@ymax, "\n", sep = "")
          }
)


setMethod("show",
          "Grid3d",
          function(object) {
            cat("class      : ", class(object), "\n", sep = "")
            cat("dimensions : ", "xcell = ", object@xcell,
                ", ycell = ", object@ycell,
                ", zcell = ", object@zcell,
                ", ncell = ", object@xcell * object@ycell * object@zcell, "\n", sep = "")
            cat("range      : ", "xmin = ", object@xmin, ", xmax = ", object@xmax, "\n",
                "             ymin = ", object@ymin, ", ymax = ", object@ymax, "\n",
                "             zmin = ", object@zmin, ", zmax = ", object@zmax, "\n",sep = "")
          }
)
