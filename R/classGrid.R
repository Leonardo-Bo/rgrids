#' Define Grid1d
#'
#' @slot xcell numeric
#' @slot xmin numeric
#' @slot xmax numeric
#'
#' @export
#'
setClass("Grid1d",
         slots = list(
           xcell = "numeric",
           xmin = "numeric",
           xmax = "numeric"
         ),
         prototype = list(
           xcell = 100,
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


setClass("Grid2d",
         contains = "Grid1d",
         slots = list(
           ycell = "numeric",
           ymin = "numeric",
           ymax = "numeric"
         ),
         prototype = list(
           ycell = 100,
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


setClass("Grid3d",
         contains = "Grid2d",
         slots = list(
           zcell = "numeric",
           zmin = "numeric",
           zmax = "numeric"
         ),
         prototype = list(
           zcell = 100,
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
