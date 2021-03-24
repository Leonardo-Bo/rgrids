#' Grid1d class
#'
#' @name makeGrid1d
#'
#' @description Given the extremes of the range and the number of cells, it
#'     generates a one-dimensional grid.
#'
#' @slot xcell numeric, number of cells
#' @slot xmin numeric, lower limit
#' @slot xmax numeric, upper limit
#'
#' @usage
#' # Create S4 object with the following default values
#' # makeGrid1d(xmin = -5, xmax = 5, xcell = 10)
#' @return
#' A uni-dimensional grid
#' @examples
#' # grid1d <- makeGrid1d(xmin = -50, xmax = 50, xcell = 100)
#' # grid1d
#' # class      : Grid1d
#' # dimensions : xcell = 100
#' # range      : xmin = -50, xmax = 50
makeGrid1d <- setClass("Grid1d",
         slots = list(
           xcell = "numeric",
           xmin = "numeric",
           xmax = "numeric"
         ),
         prototype = list(
           xcell = 10,
           xmin = -5,
           xmax = 5
         ),
         validity = function(object)	{
           c1 <- (object@xmin < object@xmax)
           c2 <- (object@xcell > 0)
           c3 <- ((object@xcell - as.integer(object@xcell)) == 0)

           if (is.na(c1)) c1 <- TRUE
           if (is.na(c2)) c2 <- TRUE
           if (is.na(c3)) c3 <- TRUE

           if (!c1) { stop("invalid range: xmin > xmax") }
           if (!c2) { stop("xcell must be positive")}
           if (!c3) { stop("xcell must be integer")}
           return(c1 & c2 & c3)
         }
)


#' Grid2d class
#'
#' @name makeGrid2d
#'
#' @description Given the extremes of the range and the number of cells along x
#'     and y, it generates a two-dimensional grid.
#'
#' @slot xcell xcell numeric, number of cells along x
#' @slot ycell ycell numeric, number of cells along y
#' @slot xmin numeric, lower limit along x
#' @slot xmax numeric, upper limit along x
#' @slot ymin numeric, lower limit along y
#' @slot ymax numeric, upper limit along y
#'
#' @usage
#' # Create S4 object with the following default values
#' # makeGrid2d(xmin = -5, xmax = 5,
#' #            ymin = -5, ymax = 5,
#' #            xcell = 10, ycell = 10)
#' @return
#' A two-dimensional grid
#' @examples
#' # grid2d <- makeGrid2d(xmin = -50, xmax = 50,
#' #                      ymin = -50, ymax = 50,
#' #                      xcell = 100, ycell = 100)
#' # grid2d
#' # class      : Grid2d
#' # dimensions : xcell = 100, ycell = 100, ncell = 10000
#' # range      : xmin = -50, xmax = 50
#' #              ymin = -50, ymax = 50
makeGrid2d <- setClass("Grid2d",
         contains = "Grid1d",
         slots = list(
           ycell = "numeric",
           ymin = "numeric",
           ymax = "numeric"
         ),
         prototype = list(
           ycell = 10,
           ymin = -5,
           ymax = 5
         ),
         validity = function(object)	{
           c4 <- (object@ymin < object@ymax)
           c5 <- (object@ycell > 0)
           c6 <- ((object@ycell - as.integer(object@ycell)) == 0)

           if (is.na(c4)) c4 <- TRUE
           if (is.na(c5)) c5 <- TRUE
           if (is.na(c6)) c6 <- TRUE

           if (!c4) { stop("invalid range: ymin > ymax") }
           if (!c5) { stop("ycell must be positive")}
           if (!c6) { stop("ycell must be integer")}
           return(c4 & c5 & c6)
         }
)


#' Grid3d class
#'
#' @name makeGrid3d
#'
#' @description Given the extremes of the range and the number of cells along x
#'     y, and z it generates a three-dimensional grid.
#'
#' @slot xcell xcell numeric, number of cells along x
#' @slot ycell ycell numeric, number of cells along y
#' @slot zcell zcell numeric, number of cells along z
#' @slot xmin numeric, lower limit along x
#' @slot xmax numeric, upper limit along x
#' @slot ymin numeric, lower limit along y
#' @slot ymax numeric, upper limit along y
#' @slot zmin numeric, lower limit along z
#' @slot zmax numeric, upper limit along z
#'
#' @usage
#' # Create S4 object with the following default values
#' # makeGrid1d(xmin = -5, xmax = 5,
#' #            ymin = -5, ymax = 5,
#' #            zmin = -5, zmax = 5,
#' #            xcell = 10, ycell = 10, zcell = 10)
#' @return
#' A three-dimensional grid
#' @examples
#' # grid3d <- makeGrid3d(xmin = -50, xmax = 50,
#' #                      ymin = -50, ymax = 50,
#' #                      zmin = -50, zmax = 50,
#' #                      xcell = 100, ycell = 100, zcell = 100)
#' # grid3d
#' # class      : Grid3d
#' # dimensions : xcell = 100, ycell = 100, zcell = 100, ncell = 1e+06
#' # range      : xmin = -50, xmax = 50
#' #              ymin = -50, ymax = 50
#' #              zmin = -50, zmax = 50
makeGrid3d <- setClass("Grid3d",
         contains = "Grid2d",
         slots = list(
           zcell = "numeric",
           zmin = "numeric",
           zmax = "numeric"
         ),
         prototype = list(
           zcell = 10,
           zmin = -5,
           zmax = 5
         ),
         validity = function(object)	{
           c7 <- (object@zmin < object@zmax)
           c8 <- (object@zcell > 0)
           c9 <- ((object@zcell - as.integer(object@zcell)) == 0)

           if (is.na(c7)) c7 <- TRUE
           if (is.na(c8)) c8 <- TRUE
           if (is.na(c9)) c9 <- TRUE

           if (!c7) { stop("invalid range: zmin > zmax") }
           if (!c8) { stop("zcell must be positive")}
           if (!c9) { stop("zcell must be integer")}
           return(c7 & c8 & c9)
         }
)

#' @import methods

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
