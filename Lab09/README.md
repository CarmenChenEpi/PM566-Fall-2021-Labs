Lab 09
================
Carmen Chen
10/29/2021

\#Problem 2

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  return(x)
}

fun1(5, 10)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ## [1,]    6    4    4    2    1    3    6    3    0     6
    ## [2,]    3    7    2    6    4    2    4    3    4     1
    ## [3,]    3    4    3    3    5    7    1    3    1     3
    ## [4,]    5    5    3    2    4    5    6    8    5     2
    ## [5,]    4    5    3    3    6    3    6    7    6     6

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n * k, lambda), nrow = n, ncol = k, byrow = TRUE)
}

fun1alt(5,10)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ## [1,]    4    4    5    1    1    4    2    5    7     7
    ## [2,]    3    2    4    3    4    3    3    6    6     1
    ## [3,]    6    8    4    5    3    5    3    5    5     4
    ## [4,]    7    7    3    5    1    1    4    5    3     9
    ## [5,]    6    7    4    4    1    6    2    1    3     5

``` r
microbenchmark::microbenchmark(
  fun1(n = 1000),
  fun1alt(n = 1000), unit = "relative"
)
```

    ## Unit: relative
    ##               expr      min       lq     mean   median       uq      max neval
    ##     fun1(n = 1000) 28.87759 30.36997 29.00052 31.37327 41.08138 4.035151   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.00000 1.000000   100

\#\#Problem 3

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  # YOUR CODE HERE
}

# Benchmarking
microbenchmark::microbenchmark(
  fun2(),
  fun2alt()
)
```
