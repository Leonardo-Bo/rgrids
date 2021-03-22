setGeneric("makeGrid1d", function(x, ...) standardGeneric("makeGrid1d"))
setGeneric("makeGrid2d", function(x, ...) standardGeneric("makeGrid2d"))
setGeneric("makeGrid3d", function(x, ...) standardGeneric("makeGrid3d"))


setMethod("makeGrid1d",
          signature(x='missing'),
          function(xcell = 100, xmin = -5, xmax = 5) {
            xcell <- xcell
            xmin <- xmin
            xmax <- xmax
            grid <- methods::new("Grid1d", xcell = xcell, xmin = xmin, xmax = xmax)
          }
)


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
