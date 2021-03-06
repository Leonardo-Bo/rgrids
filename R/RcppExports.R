# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

.getCell1d <- function(xcell, xmin, xmax, x) {
    .Call('_rgrids_getCell1d', PACKAGE = 'rgrids', xcell, xmin, xmax, x)
}

.getCell2d <- function(xcell, ycell, xmin, xmax, ymin, ymax, x, y) {
    .Call('_rgrids_getCell2d', PACKAGE = 'rgrids', xcell, ycell, xmin, xmax, ymin, ymax, x, y)
}

.getCell3d <- function(xcell, ycell, zcell, xmin, xmax, ymin, ymax, zmin, zmax, x, y, z) {
    .Call('_rgrids_getCell3d', PACKAGE = 'rgrids', xcell, ycell, zcell, xmin, xmax, ymin, ymax, zmin, zmax, x, y, z)
}

.getCoords1d <- function(xcell, xmin, xmax, cell) {
    .Call('_rgrids_getCoords1d', PACKAGE = 'rgrids', xcell, xmin, xmax, cell)
}

.getCoords2d <- function(xcell, ycell, xmin, xmax, ymin, ymax, cell) {
    .Call('_rgrids_getCoords2d', PACKAGE = 'rgrids', xcell, ycell, xmin, xmax, ymin, ymax, cell)
}

.getCoords3d <- function(xcell, ycell, zcell, xmin, xmax, ymin, ymax, zmin, zmax, cell) {
    .Call('_rgrids_getCoords3d', PACKAGE = 'rgrids', xcell, ycell, zcell, xmin, xmax, ymin, ymax, zmin, zmax, cell)
}

.blockmean <- function(matrix, width, block_width, height, block_height) {
    .Call('_rgrids_blockmean', PACKAGE = 'rgrids', matrix, width, block_width, height, block_height)
}

.blockmax <- function(matrix, width, block_width, height, block_height) {
    .Call('_rgrids_blockmax', PACKAGE = 'rgrids', matrix, width, block_width, height, block_height)
}

.blockmin <- function(matrix, width, block_width, height, block_height) {
    .Call('_rgrids_blockmin', PACKAGE = 'rgrids', matrix, width, block_width, height, block_height)
}

.rowMax <- function(mat) {
    .Call('_rgrids_rowMax', PACKAGE = 'rgrids', mat)
}

.rowMin <- function(mat) {
    .Call('_rgrids_rowMin', PACKAGE = 'rgrids', mat)
}

.colMax <- function(mat) {
    .Call('_rgrids_colMax', PACKAGE = 'rgrids', mat)
}

.colMin <- function(mat) {
    .Call('_rgrids_colMin', PACKAGE = 'rgrids', mat)
}

