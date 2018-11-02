Writing functions
================

``` r
sim_regression = function(n, beta0 = 2, beta1 = 3) {
  
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
output = vector("list", 100)

for (i in 1:100) {
  output[[i]] = sim_regression(30)
}

sim_results = bind_rows(output)
```

``` r
sim_results = 
  rerun(1000, sim_regression(30, 2, 3)) %>% 
  bind_rows()
```

``` r
sim_results %>% 
  ggplot(aes(x = beta0_hat, y = beta1_hat)) + 
  geom_point()
```

<img src="simulation_files/figure-markdown_github/unnamed-chunk-5-1.png" width="90%" />
