# rgrids
[![Version](https://img.shields.io/badge/Version-0.3.0-orange)](https://github.com/Leonardo-Bo/rgrids)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://github.com/Leonardo-Bo/rgrids/blob/master/LICENSE.md)

#
### Overview
`rgrids` is an `R` package that allows you to work consistently with grids and matrices.  
`rgrids` facilitates density analysis in 1D, 2D and 3D. It is also possible to deal with block matrices and perform operations on lists of matrices. Further developments will concern operations on first neighbors and in general on matrix elements.

`rgrids` is mainly developed in `R`, but uses the `C++` language to increase performance and use less memory.

#
### Installation and update
`rgrids` hasn't been released on CRAN yet, so you can install it directly from this repository

```
devtools::install_github("Leonardo-Bo/rgrids")
```

If you have already installed the package, you can check for a new version and update it

```
devtools::update_packages("rgrids")
```

#
### Overview of functions
The purpose of `rgrids` is to collect a not too large number of functions that work in a coherent way to solve some recurring problems on grids and matrices. Below is a list of the functions currently available 

- the functions that operate mainly on the grids are:

    - `makeGrid1d()`: creates a 1D grid object, i.e. divides a plane into strips or a straight line
    - `makeGrid2d()`: creates a 2D grid object, i.e. divides a plane into rectangles
    - `makeGrid3d()`: creates a 2D grid object, i.e. divides a space into parallelepipeds
    - `getBoxplot()`: for a 1D grid it associates all y values within a strip to the same x
    - `getCell()`: associates each point of a 1D, 2D or 3D array to its respective strip, rectangle or parallelepiped
    - `getCounts()`: generates a dataframe with three columns: the coordinates of each cell of the grid and the number of elements that fall inside each cell

- the functions that operate mainly on block matrices are:

    - `blockmax()`: given a matrix and the size of a block, it returns a new matrix containing the `max` of each block
    - `blockmin()`: given a matrix and the size of a block, it returns a new matrix containing the `min` of each block
    - `blockmean()`: given a matrix and the size of a block, it returns a new matrix containing the `mean` of each block
    - `blocklist()`: given a matrix and the size of a block, it returns a list containing all submatrices made by blocks
    - `dblocklist()`: given a matrix and the size of a block, it returns a list all diagonal submatrices made by blocks
    - `meanMatrix()`: given a list of matrix, it returns a single matrix in which element (i, j) is the average of all corresponding elements (i, j) of all matrices in the list
    - `sumMatrix()`: given a list of matrix, it returns a single matrix in which element (i, j) is the sum of all corresponding elements (i, j) of all matrices in the list   
    - `dotMatrix()`: given a list of matrix, it returns a single matrix in which element (i, j) is the product of all corresponding elements (i, j) of all matrices in the list

- the functions that operate on rows and columns of a matrix are:
    - `colMax()`: given a matrix, it returns the `max` of each column 
    - `colMin()`: given a matrix, it returns the `min` of each column
    - `rowMax()`: given a matrix, it returns the `max` of each row
    - `rowMin()`: given a matrix, it returns the `min` of each row
    
- other functions:
    - `pileMatrix()`: it transforms a matrix into a dataframe with three columns: row and column coordinates of each matrix element and value of each respective matrix element; it allows different selection options, for example the whole matrix or only the upper triangular matrix, ...
    - `tableToLatex()`: given a numeric matrix or a data.frame, it returns a basic LaTeX table write with table and tabular packages; rownames and colnames are highlighted with bold.

#
### Some simple uses

#### 2d grids: density analysis
1. Generate random points on a plane

    ```r
    library(rgrids)

    df_points <- data.frame(x = c(rnorm(n = 50000, mean = -2), 
                                  rnorm(n = 50000, mean = 2)), 
                            y = c(rnorm(n = 50000, mean = 1), 
                                  rnorm(n = 50000, mean = -1))
    ) 
    ```

2. Define a grid that contains all the points generated

    ```r
    the_grid <- makeGrid2d(
        xmin = floor(min(df_points$x)), 
        ymin = floor(min(df_points$y)), 
        xmax = ceiling(max(df_points$x)), 
        ymax = ceiling(max(df_points$y)), 
        xcell = 50, ycell = 50
    )
    ```
    
    ```
    class      : Grid2d
    dimensions : xcell = 50, ycell = 50, ncell = 2500
    range      : xmin = -7, xmax = 7
                 ymin = -6, ymax = 6
    ```
    
3. Match each point with a grid element

    ```r
    grid_index <- getCell(the_grid, df_points)
    df_points$grid_index <- grid_index
    ```
    
4. Count the points within each grid element

    ```r
    df_grid <- getCounts(the_grid, grid_index)
    ```
    
#### Operations on submatrixes and matrix lists
Given a matrix

```r
mat1 <- matrix(1:64, nrow = 8, byrow = TRUE)
```

1. Calculate the average on 2x2 blocks

    ```r
    blockmean(mat1, 2)
    ```
    ```
    ##      [,1] [,2] [,3] [,4]
    ## [1,]  5.5  7.5  9.5 11.5
    ## [2,] 21.5 23.5 25.5 27.5
    ## [3,] 37.5 39.5 41.5 43.5
    ## [4,] 53.5 55.5 57.5 59.5
    ```

2. Get a list of 4x4 matrices

    ```r
    blocklist(mat1, 4)
    ```
    ```
    ## $`1`
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    9   10   11   12
    ## [3,]   17   18   19   20
    ## [4,]   25   26   27   28
    ## 
    ## $`2`
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    5    6    7    8
    ## [2,]   13   14   15   16
    ## [3,]   21   22   23   24
    ## [4,]   29   30   31   32
    ## 
    ## $`3`
    ##      [,1] [,2] [,3] [,4]
    ## [1,]   33   34   35   36
    ## [2,]   41   42   43   44
    ## [3,]   49   50   51   52
    ## [4,]   57   58   59   60
    ## 
    ## $`4`
    ##      [,1] [,2] [,3] [,4]
    ## [1,]   37   38   39   40
    ## [2,]   45   46   47   48
    ## [3,]   53   54   55   56
    ## [4,]   61   62   63   64
    ```

3. from a list of matrices obtain the sum of each element of the matrix

    ```r
    # first create a list of matrices starting from `mat1` and using `blocklist`
    list_matrix <- blocklist(mat1, 4)
    
    sumMatrix(list_matrix)
    ```
    ```
    ##      [,1] [,2] [,3] [,4]
    ## [1,]   76   80   84   88
    ## [2,]  108  112  116  120
    ## [3,]  140  144  148  152
    ## [4,]  172  176  180  184
    ```

#### Operations on rows and columns of a matrix
Given a matrix

```r
mat2 <- matrix(1:100, ncol = 10)
```
```
mat1
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
##  [1,]    1   11   21   31   41   51   61   71   81    91
##  [2,]    2   12   22   32   42   52   62   72   82    92
##  [3,]    3   13   23   33   43   53   63   73   83    93
##  [4,]    4   14   24   34   44   54   64   74   84    94
##  [5,]    5   15   25   35   45   55   65   75   85    95
##  [6,]    6   16   26   36   46   56   66   76   86    96
##  [7,]    7   17   27   37   47   57   67   77   87    97
##  [8,]    8   18   28   38   48   58   68   78   88    98
##  [9,]    9   19   29   39   49   59   69   79   89    99
## [10,]   10   20   30   40   50   60   70   80   90   100
```

```
rowMax(mat1)
##    value index
## 1     91    10
## 2     92    10
## 3     93    10
## 4     94    10
## 5     95    10
## 6     96    10
## 7     97    10
## 8     98    10
## 9     99    10
## 10   100    10

rowMin(mat1)
##    value index
## 1      1     1
## 2      2     1
## 3      3     1
## 4      4     1
## 5      5     1
## 6      6     1
## 7      7     1
## 8      8     1
## 9      9     1
## 10    10     1

colMax(mat1)
##    value index
## 1     10    10
## 2     20    10
## 3     30    10
## 4     40    10
## 5     50    10
## 6     60    10
## 7     70    10
## 8     80    10
## 9     90    10
## 10   100    10

colMin(mat1)
##    value index
## 1      1     1
## 2     11     1
## 3     21     1
## 4     31     1
## 5     41     1
## 6     51     1
## 7     61     1
## 8     71     1
## 9     81     1
## 10    91     1

```

A performance comparison with `apply` using the `bench` package

```r
mat3 <- matrix(runif(500000), ncol = 500)

rowMax2 <- function(mat) {
  df_rowMax <- data.frame(value = apply(mat, 1, max), 
                          index = apply(mat, 1, which.max))
  return(df_rowMax)
}

colMax2 <- function(mat) {
  df_colMax <- data.frame(value = apply(mat, 2, max), 
                          index = apply(mat, 2, which.max))
  return(df_colMax)
}
```

```
bench::mark(colMax(mat3), colMax2(mat3))
## # A tibble: 2 x 6
##   expression         min   median `itr/sec` mem_alloc `gc/sec`
##   <bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
## 1 colMax(mat3)  821.88µs  887.2µs    1065.     22.3KB     2.02
## 2 colMax2(mat3)   9.15ms   10.7ms      87.1    19.2MB    56.8
```

```
bench::mark(rowMax(mat3), rowMax2(mat3))
## # A tibble: 2 x 6
##   expression         min   median `itr/sec` mem_alloc `gc/sec`
##   <bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
## 1 rowMax(mat2)    1.07ms   1.15ms     776.     41.8KB      0  
## 2 rowMax2(mat2)  11.75ms  13.36ms      71.8    19.3MB     92.3
```

#
### To do
- implement first neighbors analysis and matrix elements
- further tests before publishing on the CRAN
