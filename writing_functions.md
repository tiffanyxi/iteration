Writing functions
================

``` r
x = rnorm(25, mean = 5, sd = 3)

(x - mean(x)) / sd(x)
##  [1] -0.3356917  0.6406980 -0.2184020  0.4177181 -0.9567470  0.8875528
##  [7]  1.9968879  0.5529631 -2.1334670  0.5422351  0.6243386  0.6878367
## [13] -1.4257840  0.8824691 -0.4965451  0.7253665 -0.8264896 -0.4493507
## [19] -1.2040972 -1.4094602 -0.5671326  1.3554362  0.2031259  1.0642609
## [25] -0.5577219
```

``` r
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  z
}

z_scores(x)
##  [1] -0.3356917  0.6406980 -0.2184020  0.4177181 -0.9567470  0.8875528
##  [7]  1.9968879  0.5529631 -2.1334670  0.5422351  0.6243386  0.6878367
## [13] -1.4257840  0.8824691 -0.4965451  0.7253665 -0.8264896 -0.4493507
## [19] -1.2040972 -1.4094602 -0.5671326  1.3554362  0.2031259  1.0642609
## [25] -0.5577219
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
##  [1]  1.2 -0.8  1.2 -0.8 -0.8  1.2 -0.8 -0.8  1.2  1.2 -0.8 -0.8  1.2  1.2
## [15]  1.2 -0.8 -0.8 -0.8  1.2  1.2 -0.8 -0.8 -0.8 -0.8 -0.8
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
## 1   5.60  2.38
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

### Multiple inputs

simple linear regression

``` r
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)

ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
beta1_hat = coef(ls_fit)[2]
```

change the sample size and regression parameters

``` r
sim_regression = function(n, beta0, beta1) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, 0, 1)
  )
  
  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
}
```

``` r
sim_regression(n = 3000, beta0 = 0, beta1 = -1)
## # A tibble: 1 x 2
##   beta0_hat beta1_hat
##       <dbl>     <dbl>
## 1   -0.0218    -0.986
```
