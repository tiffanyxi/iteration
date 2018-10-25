Writing functions
================

``` r
x = rnorm(25, mean = 5, sd = 3)

(x - mean(x)) / sd(x)
##  [1]  1.41215047 -0.75577480  0.37911690 -0.32694762 -0.50641241
##  [6] -0.14594075 -1.41043334  1.03432198 -1.10120923  0.16780688
## [11] -1.50841829  0.29870957 -0.26056470 -0.85986660 -0.43288516
## [16] -1.34620758  1.17638959  0.51612562  0.07839909 -1.20774026
## [21]  0.53203444  1.40945821  1.00067864 -0.36060952  2.21781888
```

``` r
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  z
}

z_scores(x)
##  [1]  1.41215047 -0.75577480  0.37911690 -0.32694762 -0.50641241
##  [6] -0.14594075 -1.41043334  1.03432198 -1.10120923  0.16780688
## [11] -1.50841829  0.29870957 -0.26056470 -0.85986660 -0.43288516
## [16] -1.34620758  1.17638959  0.51612562  0.07839909 -1.20774026
## [21]  0.53203444  1.40945821  1.00067864 -0.36060952  2.21781888
```

``` r
z_scores(3)
## [1] NA
z_scores("my name is jeff")
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
## Error in x - mean(x): non-numeric argument to binary operator
z_scores(iris)
## Warning in mean.default(x): argument is not numeric or logical: returning
## NA
## Warning in Ops.factor(left, right): '-' not meaningful for factors
## Error in is.data.frame(x): (list) object cannot be coerced to type 'double'
z_scores(sample(c(TRUE, FALSE), 25, replace = TRUE))
##  [1] -1.2 -1.2  0.8 -1.2 -1.2  0.8  0.8 -1.2 -1.2 -1.2  0.8  0.8  0.8  0.8
## [15]  0.8  0.8  0.8 -1.2  0.8 -1.2 -1.2  0.8  0.8  0.8  0.8
```

Put in some checks on inputs

``` r
z_scores_warning = function(x) {
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  z = mean(x) / sd(x)
  z
}
```

### Mean and SD

tibble

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  tibble(
  mean_x = mean(x),
  sd_x = sd(x)
  )
}
mean_and_sd(x)
## # A tibble: 1 x 2
##   mean_x  sd_x
##    <dbl> <dbl>
## 1   4.06  3.66
```

list

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  list(mean = mean_x, 
       sd = sd_x)
}
```
