Lab 09
================
Carmen Chen
10/29/2021

\#Question 2

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
    ## [1,]    4    3    3    2    7    9    3    6    8     2
    ## [2,]    4    3    5    2    4    3    1    7    6     6
    ## [3,]    4    2    4    3    5    3    3    6    3    11
    ## [4,]    5    4    1    5    4    6    1    4    4     5
    ## [5,]    6    2    6    2    4    3    5    3    6     7

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n * k, lambda), nrow = n, ncol = k, byrow = TRUE)
}

fun1alt(5,10)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
    ## [1,]    8    4    6    3    4    9    4    4    5     6
    ## [2,]    3    1    2    0    2    2    1    5    2     6
    ## [3,]    4    5    2    4    3    6    3    4    5     1
    ## [4,]    5    2    4    2    3    3    5    1    3     3
    ## [5,]    5    3    5    2    4    4    0    1    4     6

``` r
microbenchmark::microbenchmark(
  fun1(n = 1000),
  fun1alt(n = 1000), unit = "relative"
)
```

    ## Unit: relative
    ##               expr      min       lq     mean   median       uq      max neval
    ##     fun1(n = 1000) 30.49575 32.70792 30.32252 32.81572 33.75011 8.443044   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.00000 1.000000   100

\#\#Question 3

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  idx <- max.col(t(x))
  x[cbind(idx, 1:ncol(x))]
}

#Do we get the same matrix?
all(fun2(x) == fun2alt(x))
```

    ## [1] TRUE

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), unit = "relative"
)
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq       max neval
    ##     fun2(x) 10.22074 10.52175 7.269595 9.927982 10.68153 0.7632612   100
    ##  fun2alt(x)  1.00000  1.00000 1.000000 1.000000  1.00000 1.0000000   100

\#\#Question 4: bootstrap

``` r
library(parallel)
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE
  cl <- makePSOCKcluster(ncpus)
  
  # STEP 2: GOES HERE
  clusterSetRNGStream(cl, 123) # Equivalent to `set.seed(123)`  
  clusterExport(cl, c("stat", "dat", "idx"), envir = environment())
  
  # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl = cl, seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  stopCluster(cl)
  
  ans
  
}
```

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 5e3

x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), stat = my_stat, R = R, ncpus = 2L)

# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1395732 0.05291612
    ## x            4.8686527 5.04503468

Check the system

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##    0.20    0.05    7.83

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##    0.19    0.08    4.20
