Writing functions
================

``` r
x = rnorm(25, mean = 5, sd = 3)

(x - mean(x)) / sd(x)
##  [1] -0.73506490  0.68725519 -0.84931512 -1.78380311 -0.44320873
##  [6]  0.56981539 -1.77899600  1.05178095  1.21171710  0.19776898
## [11] -1.33242445  0.10422223  0.41925538 -0.70810741  0.52624003
## [16]  0.07209541  2.04576026  0.23580265  1.19006656  0.37340589
## [21]  0.68255912 -1.15027404 -1.44352264  0.39265272  0.46431856
```

``` r
z_scores = function(x) {
  z = (x - mean(x)) / sd(x)
  z
}

z_scores(x)
##  [1] -0.73506490  0.68725519 -0.84931512 -1.78380311 -0.44320873
##  [6]  0.56981539 -1.77899600  1.05178095  1.21171710  0.19776898
## [11] -1.33242445  0.10422223  0.41925538 -0.70810741  0.52624003
## [16]  0.07209541  2.04576026  0.23580265  1.19006656  0.37340589
## [21]  0.68255912 -1.15027404 -1.44352264  0.39265272  0.46431856
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
##  [1]  1.0198039 -0.9413574 -0.9413574  1.0198039  1.0198039 -0.9413574
##  [7] -0.9413574 -0.9413574  1.0198039  1.0198039 -0.9413574 -0.9413574
## [13]  1.0198039  1.0198039  1.0198039 -0.9413574 -0.9413574 -0.9413574
## [19]  1.0198039 -0.9413574  1.0198039  1.0198039 -0.9413574 -0.9413574
## [25]  1.0198039
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
## 1   4.53  3.23
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
## 1    0.0803     -1.03
```

### Scraping Amazon

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = dynamite_html %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  review_titles = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  review_stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  review_text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(urls[1]),
  read_page_reviews(urls[2]),
  read_page_reviews(urls[3]),
  read_page_reviews(urls[4]),
  read_page_reviews(urls[5])
)

dynamite_reviews
## # A tibble: 50 x 3
##    title                     stars text                                   
##    <chr>                     <dbl> <chr>                                  
##  1 "Great \"Odd Ball\" movi…     5 The dance scene was worth the time spe…
##  2 Nostalgic Stupidity           4 This movie is dumb. I won't lie and sa…
##  3 Happy                         5 Don't know why I lov this movie but ido
##  4 Go watch THE ROCK or dum…     2 This movie is horrible. How do so many…
##  5 My mom loves it               5 Got this for my mom for mother's day, …
##  6 Nothing Quite Like It.        5 So much fun watching these listless pe…
##  7 Has pretty sweet bow ski…     5 Well, things are getting pretty seriou…
##  8 Great                         5 Great                                  
##  9 Fast delivery                 5 Bought as gift                         
## 10 Lol                           5 Funny                                  
## # ... with 40 more rows
```

### LoTR data

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) %>%
  janitor::clean_names() %>%
  gather(key = sex, value = words, female:male) %>%
  mutate(race = tolower(race)) %>% 
  select(movie, everything()) 
```

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  df = readxl::read_excel(path, range = range) %>%
    janitor::clean_names() %>%
    gather(key = sex, value = words, female:male) %>%
    mutate(race = tolower(race),
           movie = movie_name)
  
  df
  
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "J3:L6", "return_king")) %>%
  select(movie, everything()) 
```

``` r
x = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x, sd)
## [1] 1.080833

my_summary(x, IQR)
## [1] 1.549539

my_summary(x, var)
## [1] 1.168199
```

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
## [1] 4
```
