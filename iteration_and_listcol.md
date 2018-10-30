Iteration and list column
================

``` r
vec_numeric = 5:8
vec_char = c("My", "name", "is", "Jeff")
vec_logical = c(TRUE, TRUE, TRUE, FALSE)
```

``` r
l = list(vec_numeric = 5:8,
         mat         = matrix(1:8, 2, 4),
         vec_logical = c(TRUE, FALSE),
         summary     = summary(rnorm(1000)))
l$vec_numeric[2]
## [1] 6

l$vec_numeric
## [1] 5 6 7 8

l[[1]]
## [1] 5 6 7 8

l[[1]][1:3]
## [1] 5 6 7
```

### for loops

``` r
df = data_frame(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

is.list(df)
## [1] TRUE
df[[2]]
##  [1] -1.63244797  3.87002606  3.92503200  3.81623040  1.47404380
##  [6] -6.26177962 -5.04751876  3.75695597 -6.54176756  2.63770049
## [11] -2.66769787 -1.99188007 -3.94784725 -1.15070568  4.38592421
## [16]  2.26866589 -1.16232074  4.35002762  8.28001867 -0.03184464
```

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```
