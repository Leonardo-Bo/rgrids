#' Grid1d class
#'
#' @name makeGrid1d
#'
#' @description Given the extremes of the range and the number of cells, it
#'     generates a one-dimensional grid. Cells are counted from minimum to
#'     maximum (left to right).
#'
#' @slot xcell numeric, number of cells
#' @slot xmin numeric, lower limit
#' @slot xmax numeric, upper limit
#'
#' @usage
#' # Create S4 object with the following default values
#' makeGrid1d(xmin = -5, xmax = 5, xcell = 10)
#' @return
#' A uni-dimensional grid
#' @examples
#' grid1d <- makeGrid1d(xmin = -50, xmax = 50, xcell = 100)
#'
#' # grid1d
#' class      : Grid1d
#' dimensions : xcell = 100
#' range      : xmin = -50, xmax = 50
#' @export makeGrid1d
makeGrid1d <- setClass(
  "Grid1d",
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
   validity = function(object) {
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
#'     and y, it generates a two-dimensional grid. Cells are counted from the
#'     minimum of x and y (bottom-left). You can choose whether to increase
#'     x or y faster.
#'
#' @slot xcell numeric, number of cells along x
#' @slot ycell numeric, number of cells along y
#' @slot xmin numeric, lower limit along x
#' @slot xmax numeric, upper limit along x
#' @slot ymin numeric, lower limit along y
#' @slot ymax numeric, upper limit along y
#' @slot by character, count cells increasing x ("h") or y ("v") faster
#'
#' @usage
#' # Create S4 object with the following default values
#' makeGrid2d(xmin = -5, xmax = 5,
#'            ymin = -5, ymax = 5,
#'            xcell = 10, ycell = 10,
#'            by = "h")
#' @return
#' A two-dimensional grid
#' @examples
#' grid2d <- makeGrid2d(xmin = -50, xmax = 50,
#'                      ymin = -50, ymax = 50,
#'                      xcell = 100, ycell = 100)
#'
#' # grid2d
#' class      : Grid2d
#' dimensions : xcell = 100, ycell = 100, ncell = 10000
#' range      : xmin = -50, xmax = 50
#'              ymin = -50, ymax = 50
#' by         : h, count starts from xmin, ymin (bottom-left)
#'              and x increase faster
#' @export makeGrid2d
makeGrid2d <- setClass(
  "Grid2d",
  contains = "Grid1d",
  slots = list(
    ycell = "numeric",
    ymin = "numeric",
    ymax = "numeric",
    by = "character"
  ),
  prototype = list(
    ycell = 10,
    ymin = -5,
    ymax = 5,
    by = "h"
  ),
  validity = function(object) {
    c4 <- (object@ymin < object@ymax)
    c5 <- (object@ycell > 0)
    c6 <- ((object@ycell - as.integer(object@ycell)) == 0)
    c7 <- (object@by == "h" | object@by == "v")

    if (is.na(c4)) c4 <- TRUE
    if (is.na(c5)) c5 <- TRUE
    if (is.na(c6)) c6 <- TRUE
    if (is.na(c7)) c7 <- TRUE

    if (!c4) { stop("invalid range: ymin > ymax") }
    if (!c5) { stop("ycell must be positive")}
    if (!c6) { stop("ycell must be integer")}
    if (!c7) { stop("by must be 'h' or 'v'")}
    return(c4 & c5 & c6 & c7)
  }
)


#' Grid3d class
#'
#' @name makeGrid3d
#'
#' @description Given the extremes of the range and the number of cells along x
#'     y and z, it generates a three-dimensional grid. Cells are counted from the
#'     minimum of x, y and z. You can choose whether to increase x or z faster.
#'
#' @slot xcell numeric, number of cells along x
#' @slot ycell numeric, number of cells along y
#' @slot zcell numeric, number of cells along z
#' @slot xmin numeric, lower limit along x
#' @slot xmax numeric, upper limit along x
#' @slot ymin numeric, lower limit along y
#' @slot ymax numeric, upper limit along y
#' @slot zmin numeric, lower limit along z
#' @slot zmax numeric, upper limit along z
#' @slot by character, count cells increasing x ("h") or z ("v") faster
#'
#' @usage
#' # Create S4 object with the following default values
#' makeGrid3d(xmin = -5, xmax = 5,
#'            ymin = -5, ymax = 5,
#'            zmin = -5, zmax = 5,
#'            xcell = 10, ycell = 10, zcell = 10
#'            by = "h")
#' @return
#' A three-dimensional grid
#' @examples
#' grid3d <- makeGrid3d(xmin = -50, xmax = 50,
#'                      ymin = -50, ymax = 50,
#'                      zmin = -50, zmax = 50,
#'                      xcell = 4, ycell = 5, zcell = 6
#'                      by = "v")
#'
#' # grid3d
#' class      : Grid3d
#' dimensions : xcell = 4, ycell = 5, zcell = 6, ncell = 120
#' range      : xmin = -50, xmax = 50
#'              ymin = -50, ymax = 50
#'              zmin = -50, zmax = 50
#' by         : v, count starts from xmin, ymin, zmin; "
#'              z increase faster, x slower"
#' @export makeGrid3d
makeGrid3d <- setClass(
  "Grid3d",
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
    c8 <- (object@zmin < object@zmax)
    c9 <- (object@zcell > 0)
    c10 <- ((object@zcell - as.integer(object@zcell)) == 0)

    if (is.na(c8)) c8 <- TRUE
    if (is.na(c9)) c9 <- TRUE
    if (is.na(c10)) c10 <- TRUE

    if (!c8) { stop("invalid range: zmin > zmax") }
    if (!c9) { stop("zcell must be positive")}
    if (!c10) { stop("zcell must be integer")}
    return(c8 & c9 & c10)
  }
)

#' @import methods

setMethod(
  "show",
  "Grid1d",
  function(object) {
    cat("class        : ", class(object), "\n", sep = "")
    cat("dimensions   : ", "xcell = ", object@xcell, "\n", sep = "")
    cat("range        : ", "xmin = ", object@xmin, ", xmax = ", object@xmax, "\n", sep = "")
  }
)


setMethod(
  "show",
  "Grid2d",
  function(object) {
    cat("class      : ", class(object), "\n", sep = "")
    cat("dimensions : ", "xcell = ", object@xcell,
        ", ycell = ", object@ycell,
        ", ncell = ", object@xcell * object@ycell, "\n", sep = "")
    cat("range      : xmin = ", object@xmin, ", xmax = ", object@xmax, "\n",
        "             ymin = ", object@ymin, ", ymax = ", object@ymax, "\n", sep = "")

    if(object@by == "h") {
      cat("by         : h, count starts from xmin, ymin (bottom-left) \n",
          "             and x increase faster", sep = "")
    }

    if(object@by == "v") {
      cat("by         : v, count starts from xmin, ymin (bottom-left) \n",
          "             and y increase faster", sep = "")
    }
  }
)


setMethod(
  "show",
  "Grid3d",
  function(object) {
    cat("class      : ", class(object), "\n", sep = "")
    cat("dimensions : ", "xcell = ", object@xcell,
        ", ycell = ", object@ycell,
        ", zcell = ", object@zcell,
        ", ncell = ", object@xcell * object@ycell * object@zcell, "\n", sep = "")
    cat("range      : xmin = ", object@xmin, ", xmax = ", object@xmax, "\n",
        "             ymin = ", object@ymin, ", ymax = ", object@ymax, "\n",
        "             zmin = ", object@zmin, ", zmax = ", object@zmax, "\n",sep = "")

    if(object@by == "h") {
      cat("by         : h, count starts from xmin, ymin, zmin;  \n",
          "             x increase faster, z slower", sep = "")
    }

    if(object@by == "v") {
      cat("by         : v, count starts from xmin, ymin, zmin;  \n",
          "             z increase faster, x slower", sep = "")
    }
  }
)
