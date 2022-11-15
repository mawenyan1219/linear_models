cross_validation
================

## step 1

``` r
nonlin_df = 
  tibble(
    id = 1:100,
    x = runif(100, 0, 1),
    y = 1 - 10 * (x - .3) ^ 2 + rnorm(100, 0, .3)
  )

nonlin_df %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point()
```

![](cross_validation_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Let’s get this by hand

``` r
train_df = sample_n(nonlin_df, 80)
test_df = anti_join(nonlin_df, train_df, by = "id")

ggplot(train_df, aes(x = x, y = y)) + 
  geom_point() + 
  geom_point(data = test_df, color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Try to fit 3 models

``` r
linear_mod = lm(y ~ x, data = train_df)
smooth_mod = mgcv::gam(y ~ s(x), data = train_df)
wiggly_mod = mgcv::gam(y ~ s(x, k = 30), sp = 10e-6, data = train_df)
```

See the results

``` r
train_df %>% 
  add_predictions(linear_mod) %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
train_df %>% 
  add_predictions(smooth_mod) %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
train_df %>% 
  add_predictions(wiggly_mod) %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  geom_line(aes(y = pred), color = "red")
```

![](cross_validation_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

Make predictions and compute RMSEs

``` r
rmse(linear_mod, test_df)
```

    ## [1] 0.9497881

``` r
rmse(smooth_mod, test_df)
```

    ## [1] 0.2764706

``` r
rmse(wiggly_mod, test_df)
```

    ## [1] 0.3276233

``` r
#lower is better
```

## can we iterate…?

``` r
cv_df = 
  crossv_mc(nonlin_df, 100) 

cv_df %>% pull(train) %>% .[[1]] %>% as_tibble
```

    ## # A tibble: 79 × 3
    ##       id       x       y
    ##    <int>   <dbl>   <dbl>
    ##  1     1 0.369    0.832 
    ##  2     2 0.891   -2.33  
    ##  3     3 0.217    0.693 
    ##  4     4 0.00610  0.277 
    ##  5     5 0.127    0.844 
    ##  6     6 0.0227  -0.244 
    ##  7     7 0.775   -1.42  
    ##  8     8 0.696   -1.08  
    ##  9     9 0.778   -1.26  
    ## 10    11 0.596    0.0641
    ## # … with 69 more rows

``` r
cv_df =
  cv_df %>% 
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble))

cv_df = 
  cv_df %>% 
  mutate(
    linear_mod  = map(train, ~lm(y ~ x, data = .x)),
    smooth_mod  = map(train, ~mgcv::gam(y ~ s(x), data = .x)),
    wiggly_mod  = map(train, ~gam(y ~ s(x, k = 30), sp = 10e-6, data = .x))) %>% 
  mutate(
    rmse_linear = map2_dbl(linear_mod, test, ~rmse(model = .x, data = .y)),
    rmse_smooth = map2_dbl(smooth_mod, test, ~rmse(model = .x, data = .y)),
    rmse_wiggly = map2_dbl(wiggly_mod, test, ~rmse(model = .x, data = .y)))
```
