Data Exploration
================

``` r
lawsuit_df = read_csv("./data/Lawsuit.csv") %>% janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   ID = col_double(),
    ##   Dept = col_double(),
    ##   Gender = col_double(),
    ##   Clin = col_double(),
    ##   Cert = col_double(),
    ##   Prate = col_double(),
    ##   Exper = col_double(),
    ##   Rank = col_double(),
    ##   Sal94 = col_double(),
    ##   Sal95 = col_double()
    ## )

``` r
lawsuit_df = lawsuit_df %>% 
  mutate(sum_sal = sal94 + sal95)
```

``` r
lawsuit_df %>% 
  describeBy(., group = "gender")
```

    ## 
    ##  Descriptive statistics by group 
    ## gender: 0
    ##         vars   n      mean        sd    median   trimmed       mad     min
    ## id         1 106    129.30     67.52    123.50    127.45    103.78    31.0
    ## dept       2 106      3.33      1.61      4.00      3.35      1.48     1.0
    ## gender     3 106      0.00      0.00      0.00      0.00      0.00     0.0
    ## clin       4 106      0.57      0.50      1.00      0.58      0.00     0.0
    ## cert       5 106      0.66      0.48      1.00      0.70      0.00     0.0
    ## prate      6 106      5.35      1.89      5.25      5.32      2.52     2.4
    ## exper      7 106      7.49      4.17      7.00      7.19      4.45     1.0
    ## rank       8 106      1.50      0.75      1.00      1.38      0.00     1.0
    ## sal94      9 106 118871.27  56168.01 108457.00 111788.83  51002.92 34514.0
    ## sal95     10 106 130876.92  62034.51 119135.00 123086.12  53945.14 38675.0
    ## sum_sal   11 106 249748.19 118179.24 227412.00 234874.94 103693.79 73189.0
    ##              max    range  skew kurtosis       se
    ## id         261.0    230.0  0.26    -1.20     6.56
    ## dept         6.0      5.0 -0.13    -1.41     0.16
    ## gender       0.0      0.0   NaN      NaN     0.00
    ## clin         1.0      1.0 -0.26    -1.95     0.05
    ## cert         1.0      1.0 -0.67    -1.57     0.05
    ## prate        8.7      6.3  0.19    -1.42     0.18
    ## exper       23.0     22.0  0.81     0.54     0.40
    ## rank         3.0      2.0  1.09    -0.37     0.07
    ## sal94   308081.0 273567.0  1.17     1.29  5455.52
    ## sal95   339664.0 300989.0  1.16     1.22  6025.32
    ## sum_sal 647745.0 574556.0  1.16     1.25 11478.58
    ## -------------------------------------------------------- 
    ## gender: 1
    ##         vars   n      mean        sd median   trimmed       mad      min
    ## id         1 155    132.16     80.68    149    133.04    117.13      1.0
    ## dept       2 155      3.87      1.89      5      3.96      1.48      1.0
    ## gender     3 155      1.00      0.00      1      1.00      0.00      1.0
    ## clin       4 155      0.65      0.48      1      0.68      0.00      0.0
    ## cert       5 155      0.76      0.43      1      0.82      0.00      0.0
    ## prate      6 155      4.65      1.94      4      4.55      1.93      1.3
    ## exper      7 155     12.10      6.70     10     11.26      5.93      2.0
    ## rank       8 155      2.17      0.84      2      2.21      1.48      1.0
    ## sal94      9 155 177338.76  85930.54 155006 168768.04  80610.44  52582.0
    ## sal95     10 155 194914.09  94902.73 170967 185352.20  87922.63  58923.0
    ## sum_sal   11 155 372252.85 180794.22 325974 354117.69 168252.86 111505.0
    ##              max    range  skew kurtosis       se
    ## id         256.0    255.0 -0.12    -1.29     6.48
    ## dept         6.0      5.0 -0.42    -1.42     0.15
    ## gender       1.0      0.0   NaN      NaN     0.00
    ## clin         1.0      1.0 -0.60    -1.65     0.04
    ## cert         1.0      1.0 -1.21    -0.53     0.03
    ## prate        8.6      7.3  0.43    -1.18     0.16
    ## exper       37.0     35.0  1.22     1.65     0.54
    ## rank         3.0      2.0 -0.32    -1.51     0.07
    ## sal94   428876.0 376294.0  0.82    -0.01  6902.11
    ## sal95   472589.0 413666.0  0.83     0.02  7622.77
    ## sum_sal 891718.0 780213.0  0.82     0.00 14521.74

``` r
# lawsuit_df %>% 
#   gather(-id, -sum_sal, key = "var", value = "value") %>% 
#   ggplot(aes(x = value, y = sum_sal)) + 
#   geom_point() +
#   facet_wrap(~var)


lawsuit_df %>% 
  ggplot(aes(x = prate, y = sum_sal)) + 
  geom_point()
```

![](data_analysis_files/figure-gfm/descriptive%20stats-1.png)<!-- -->

``` r
lawsuit_df %>% 
  ggplot(aes(x = exper, y = sum_sal)) + 
  geom_point()
```

![](data_analysis_files/figure-gfm/descriptive%20stats-2.png)<!-- -->

``` r
lawsuit_df %>% 
  ggplot(aes(x = rank, y = sum_sal)) + 
  geom_violin() + 
  facet_wrap(~rank)
```

![](data_analysis_files/figure-gfm/descriptive%20stats-3.png)<!-- -->

``` r
lawsuit_df %>% 
  ggplot(aes(x = gender, y = sum_sal)) + 
  geom_violin() + 
  facet_wrap(~gender)
```

![](data_analysis_files/figure-gfm/descriptive%20stats-4.png)<!-- -->

``` r
# not normal
lawsuit_df %>% 
  ggplot(aes(sample = sum_sal)) + 
  stat_qq() + 
  stat_qq_line()
```

![](data_analysis_files/figure-gfm/descriptive%20stats-5.png)<!-- -->

``` r
# log transformation
lawsuit_df %>% 
  mutate(log_sal = log(sum_sal)) %>% 
  ggplot(aes(sample = log_sal)) + 
  stat_qq() + 
  stat_qq_line()
```

![](data_analysis_files/figure-gfm/descriptive%20stats-6.png)<!-- -->

``` r
# sqrt transformation
lawsuit_df %>% 
  mutate(rt_sal = sqrt(sum_sal)) %>% 
  ggplot(aes(sample = rt_sal)) + 
  stat_qq() + 
  stat_qq_line()
```

![](data_analysis_files/figure-gfm/descriptive%20stats-7.png)<!-- -->

``` r
# outcome: settle for sum_sal for now. 
# main predictor: gender

# fit model

model = lm(sum_sal~gender + prate + exper + rank + dept, data = lawsuit_df)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = sum_sal ~ gender + prate + exper + rank + dept, 
    ##     data = lawsuit_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -159069  -59988   -6160   44557  334345 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    47484      46964   1.011 0.312943    
    ## gender         19891      11534   1.724 0.085834 .  
    ## prate         -18607       5182  -3.591 0.000395 ***
    ## exper           8369       1072   7.803 1.56e-13 ***
    ## rank           28503       7874   3.620 0.000355 ***
    ## dept           58968       5620  10.492  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 80560 on 255 degrees of freedom
    ## Multiple R-squared:  0.7777, Adjusted R-squared:  0.7734 
    ## F-statistic: 178.5 on 5 and 255 DF,  p-value: < 2.2e-16
