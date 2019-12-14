Final Project
================

# Data exploration

``` r
law = read_csv("./data/Lawsuit.csv") %>%
  mutate(Sal = (Sal94 + Sal95)/2) %>%
  dplyr::select(-ID, -Sal94, -Sal95)
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
law_df = law %>%
  mutate(Gender = factor(Gender, levels = c(0,1), labels = c("Female", "Male")),
         Dept = factor(Dept, levels = c(1,2,3,4,5,6), 
                       labels = c("Biochemistry/Molecular Biology", "Physiology", "Genetics", "Pediatrics", "Medicine", "Surgery")),
         Clin = factor(Clin, levels = c(0,1), labels = c("Primarily research emphasis", "Primarily clinical emphasis")),
         Cert = factor(Cert, levels = c(0,1), labels = c("not certified", "Board certified")),
         Rank = factor(Rank, levels = c(1,2,3), labels = c("Assistant", "Associate","Full professor")))

my_labels <- list(Dept = "Department", Gender = "Gender", Clin = "Research/Clinical emphasis", Cert = "Not/Board Certificated", Prate = "Publication rate", Exper = "Experience(years)", Rank = "Title", Sal = "Average Salary in 1994-1995")

my_controls <- tableby.control(
  total = T,
  test = F, 
  digits = 2,
  numeric.stats = c("meansd", "medianq1q3", "range"),
  cat.stats = c("countpct"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    countpct = "N (%)"))

tab1 = tableby(Gender ~ Dept + Clin + Cert + Prate + Exper + Rank + Sal, data = law_df, control = my_controls) %>%
  summary(title = "Summary by Gender", 
        labelTranslations = my_labels, text = T)

tab1 %>% knitr::kable(format = "html")
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

Female (N=106)

</th>

<th style="text-align:left;">

Male (N=155)

</th>

<th style="text-align:left;">

Total (N=261)

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Department

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

  - Biochemistry/Molecular Biology
    </td>
    <td style="text-align:left;">
    20 (18.9%)
    </td>
    <td style="text-align:left;">
    30 (19.4%)
    </td>
    <td style="text-align:left;">
    50 (19.2%)
    </td>
    </tr>
    <tr>
    <td style="text-align:left;">
      - Physiology
        </td>
        <td style="text-align:left;">
        20 (18.9%)
        </td>
        <td style="text-align:left;">
        20 (12.9%)
        </td>
        <td style="text-align:left;">
        40 (15.3%)
        </td>
        </tr>
        <tr>
        <td style="text-align:left;">
          - Genetics
            </td>
            <td style="text-align:left;">
            11 (10.4%)
            </td>
            <td style="text-align:left;">
            10 (6.5%)
            </td>
            <td style="text-align:left;">
            21 (8.0%)
            </td>
            </tr>
            <tr>
            <td style="text-align:left;">
              - Pediatrics
                </td>
                <td style="text-align:left;">
                20 (18.9%)
                </td>
                <td style="text-align:left;">
                10 (6.5%)
                </td>
                <td style="text-align:left;">
                30 (11.5%)
                </td>
                </tr>
                <tr>
                <td style="text-align:left;">
                  - Medicine
                    </td>
                    <td style="text-align:left;">
                    30 (28.3%)
                    </td>
                    <td style="text-align:left;">
                    50 (32.3%)
                    </td>
                    <td style="text-align:left;">
                    80 (30.7%)
                    </td>
                    </tr>
                    <tr>
                    <td style="text-align:left;">
                      - Surgery
                        </td>
                        <td style="text-align:left;">
                        5 (4.7%)
                        </td>
                        <td style="text-align:left;">
                        35 (22.6%)
                        </td>
                        <td style="text-align:left;">
                        40 (15.3%)
                        </td>
                        </tr>
                        <tr>
                        <td style="text-align:left;">
                        Research/Clinical emphasis
                        </td>
                        <td style="text-align:left;">
                        </td>
                        <td style="text-align:left;">
                        </td>
                        <td style="text-align:left;">
                        </td>
                        </tr>
                        <tr>
                        <td style="text-align:left;">
                          - Primarily research emphasis
                            </td>
                            <td style="text-align:left;">
                            46 (43.4%)
                            </td>
                            <td style="text-align:left;">
                            55 (35.5%)
                            </td>
                            <td style="text-align:left;">
                            101 (38.7%)
                            </td>
                            </tr>
                            <tr>
                            <td style="text-align:left;">
                              - Primarily clinical emphasis
                                </td>
                                <td style="text-align:left;">
                                60 (56.6%)
                                </td>
                                <td style="text-align:left;">
                                100 (64.5%)
                                </td>
                                <td style="text-align:left;">
                                160 (61.3%)
                                </td>
                                </tr>
                                <tr>
                                <td style="text-align:left;">
                                Not/Board Certificated
                                </td>
                                <td style="text-align:left;">
                                </td>
                                <td style="text-align:left;">
                                </td>
                                <td style="text-align:left;">
                                </td>
                                </tr>
                                <tr>
                                <td style="text-align:left;">
                                  - not certified
                                    </td>
                                    <td style="text-align:left;">
                                    36 (34.0%)
                                    </td>
                                    <td style="text-align:left;">
                                    37 (23.9%)
                                    </td>
                                    <td style="text-align:left;">
                                    73 (28.0%)
                                    </td>
                                    </tr>
                                    <tr>
                                    <td style="text-align:left;">
                                      - Board certified
                                        </td>
                                        <td style="text-align:left;">
                                        70 (66.0%)
                                        </td>
                                        <td style="text-align:left;">
                                        118 (76.1%)
                                        </td>
                                        <td style="text-align:left;">
                                        188 (72.0%)
                                        </td>
                                        </tr>
                                        <tr>
                                        <td style="text-align:left;">
                                        Publication rate
                                        </td>
                                        <td style="text-align:left;">
                                        </td>
                                        <td style="text-align:left;">
                                        </td>
                                        <td style="text-align:left;">
                                        </td>
                                        </tr>
                                        <tr>
                                        <td style="text-align:left;">
                                          - Mean
                                            (SD)
                                            </td>
                                            <td style="text-align:left;">
                                            5.35
                                            (1.89)
                                            </td>
                                            <td style="text-align:left;">
                                            4.65
                                            (1.94)
                                            </td>
                                            <td style="text-align:left;">
                                            4.93
                                            (1.94)
                                            </td>
                                            </tr>
                                            <tr>
                                            <td style="text-align:left;">
                                              - Median (Q1,
                                                Q3)
                                                </td>
                                                <td style="text-align:left;">
                                                5.25 (3.73,
                                                7.27)
                                                </td>
                                                <td style="text-align:left;">
                                                4.00 (3.10,
                                                6.70)
                                                </td>
                                                <td style="text-align:left;">
                                                4.40 (3.20,
                                                6.90)
                                                </td>
                                                </tr>
                                                <tr>
                                                <td style="text-align:left;">
                                                  - Min -
                                                    Max
                                                    </td>
                                                    <td style="text-align:left;">
                                                    2.40 -
                                                    8.70
                                                    </td>
                                                    <td style="text-align:left;">
                                                    1.30 -
                                                    8.60
                                                    </td>
                                                    <td style="text-align:left;">
                                                    1.30 -
                                                    8.70
                                                    </td>
                                                    </tr>
                                                    <tr>
                                                    <td style="text-align:left;">
                                                    Experience(years)
                                                    </td>
                                                    <td style="text-align:left;">
                                                    </td>
                                                    <td style="text-align:left;">
                                                    </td>
                                                    <td style="text-align:left;">
                                                    </td>
                                                    </tr>
                                                    <tr>
                                                    <td style="text-align:left;">
                                                      - Mean
                                                        (SD)
                                                        </td>
                                                        <td style="text-align:left;">
                                                        7.49
                                                        (4.17)
                                                        </td>
                                                        <td style="text-align:left;">
                                                        12.10
                                                        (6.70)
                                                        </td>
                                                        <td style="text-align:left;">
                                                        10.23
                                                        (6.23)
                                                        </td>
                                                        </tr>
                                                        <tr>
                                                        <td style="text-align:left;">
                                                          - Median (Q1,
                                                            Q3)
                                                            </td>
                                                            <td style="text-align:left;">
                                                            7.00 (5.00,
                                                            10.00)
                                                            </td>
                                                            <td style="text-align:left;">
                                                            10.00 (7.00,
                                                            15.00)
                                                            </td>
                                                            <td style="text-align:left;">
                                                            9.00 (6.00,
                                                            14.00)
                                                            </td>
                                                            </tr>
                                                            <tr>
                                                            <td style="text-align:left;">
                                                              - Min -
                                                                Max
                                                                </td>
                                                                <td style="text-align:left;">
                                                                1.00 -
                                                                23.00
                                                                </td>
                                                                <td style="text-align:left;">
                                                                2.00 -
                                                                37.00
                                                                </td>
                                                                <td style="text-align:left;">
                                                                1.00 -
                                                                37.00
                                                                </td>
                                                                </tr>
                                                                <tr>
                                                                <td style="text-align:left;">
                                                                Title
                                                                </td>
                                                                <td style="text-align:left;">
                                                                </td>
                                                                <td style="text-align:left;">
                                                                </td>
                                                                <td style="text-align:left;">
                                                                </td>
                                                                </tr>
                                                                <tr>
                                                                <td style="text-align:left;">
                                                                  - Assistant
                                                                    </td>
                                                                    <td style="text-align:left;">
                                                                    69
                                                                    (65.1%)
                                                                    </td>
                                                                    <td style="text-align:left;">
                                                                    43
                                                                    (27.7%)
                                                                    </td>
                                                                    <td style="text-align:left;">
                                                                    112
                                                                    (42.9%)
                                                                    </td>
                                                                    </tr>
                                                                    <tr>
                                                                    <td style="text-align:left;">
                                                                      - Associate
                                                                        </td>
                                                                        <td style="text-align:left;">
                                                                        21
                                                                        (19.8%)
                                                                        </td>
                                                                        <td style="text-align:left;">
                                                                        43
                                                                        (27.7%)
                                                                        </td>
                                                                        <td style="text-align:left;">
                                                                        64
                                                                        (24.5%)
                                                                        </td>
                                                                        </tr>
                                                                        <tr>
                                                                        <td style="text-align:left;">
                                                                          - Full
                                                                            professor
                                                                            </td>
                                                                            <td style="text-align:left;">
                                                                            16
                                                                            (15.1%)
                                                                            </td>
                                                                            <td style="text-align:left;">
                                                                            69
                                                                            (44.5%)
                                                                            </td>
                                                                            <td style="text-align:left;">
                                                                            85
                                                                            (32.6%)
                                                                            </td>
                                                                            </tr>
                                                                            <tr>
                                                                            <td style="text-align:left;">
                                                                            Average
                                                                            Salary
                                                                            in
                                                                            1994-1995
                                                                            </td>
                                                                            <td style="text-align:left;">
                                                                            </td>
                                                                            <td style="text-align:left;">
                                                                            </td>
                                                                            <td style="text-align:left;">
                                                                            </td>
                                                                            </tr>
                                                                            <tr>
                                                                            <td style="text-align:left;">
                                                                              - Mean
                                                                                (SD)
                                                                                </td>
                                                                                <td style="text-align:left;">
                                                                                124874.09
                                                                                (59089.62)
                                                                                </td>
                                                                                <td style="text-align:left;">
                                                                                186126.43
                                                                                (90397.11)
                                                                                </td>
                                                                                <td style="text-align:left;">
                                                                                161250.00
                                                                                (84608.33)
                                                                                </td>
                                                                                </tr>
                                                                                <tr>
                                                                                <td style="text-align:left;">
                                                                                  - Median
                                                                                    (Q1,
                                                                                    Q3)
                                                                                    </td>
                                                                                    <td style="text-align:left;">
                                                                                    113706.00
                                                                                    (79059.88,
                                                                                    148401.12)
                                                                                    </td>
                                                                                    <td style="text-align:left;">
                                                                                    162987.00
                                                                                    (114612.50,
                                                                                    244332.25)
                                                                                    </td>
                                                                                    <td style="text-align:left;">
                                                                                    141628.00
                                                                                    (95176.50,
                                                                                    210012.50)
                                                                                    </td>
                                                                                    </tr>
                                                                                    <tr>
                                                                                    <td style="text-align:left;">
                                                                                      - Min
                                                                                        -
                                                                                        Max
                                                                                        </td>
                                                                                        <td style="text-align:left;">
                                                                                        36594.50
                                                                                        -
                                                                                        323872.50
                                                                                        </td>
                                                                                        <td style="text-align:left;">
                                                                                        55752.50
                                                                                        -
                                                                                        445859.00
                                                                                        </td>
                                                                                        <td style="text-align:left;">
                                                                                        36594.50
                                                                                        -
                                                                                        445859.00
                                                                                        </td>
                                                                                        </tr>
                                                                                        </tbody>
                                                                                        </table>
                                                                                        # Transformation

<!-- end list -->

``` r
attach(law)
pairs(law)
```

<img src="m_model_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
cor_mat = round(cor(law),2)
cor_mat %>% knitr::kable(format = "html")
```

<table>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:right;">

Dept

</th>

<th style="text-align:right;">

Gender

</th>

<th style="text-align:right;">

Clin

</th>

<th style="text-align:right;">

Cert

</th>

<th style="text-align:right;">

Prate

</th>

<th style="text-align:right;">

Exper

</th>

<th style="text-align:right;">

Rank

</th>

<th style="text-align:right;">

Sal

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Dept

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.15

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.37

</td>

<td style="text-align:right;">

\-0.87

</td>

<td style="text-align:right;">

\-0.13

</td>

<td style="text-align:right;">

\-0.17

</td>

<td style="text-align:right;">

0.76

</td>

</tr>

<tr>

<td style="text-align:left;">

Gender

</td>

<td style="text-align:right;">

0.15

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.08

</td>

<td style="text-align:right;">

0.11

</td>

<td style="text-align:right;">

\-0.18

</td>

<td style="text-align:right;">

0.36

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

0.36

</td>

</tr>

<tr>

<td style="text-align:left;">

Clin

</td>

<td style="text-align:right;">

0.61

</td>

<td style="text-align:right;">

0.08

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

\-0.84

</td>

<td style="text-align:right;">

\-0.06

</td>

<td style="text-align:right;">

\-0.10

</td>

<td style="text-align:right;">

0.54

</td>

</tr>

<tr>

<td style="text-align:left;">

Cert

</td>

<td style="text-align:right;">

0.37

</td>

<td style="text-align:right;">

0.11

</td>

<td style="text-align:right;">

0.33

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

\-0.39

</td>

<td style="text-align:right;">

0.10

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

0.43

</td>

</tr>

<tr>

<td style="text-align:left;">

Prate

</td>

<td style="text-align:right;">

\-0.87

</td>

<td style="text-align:right;">

\-0.18

</td>

<td style="text-align:right;">

\-0.84

</td>

<td style="text-align:right;">

\-0.39

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.11

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

\-0.71

</td>

</tr>

<tr>

<td style="text-align:left;">

Exper

</td>

<td style="text-align:right;">

\-0.13

</td>

<td style="text-align:right;">

0.36

</td>

<td style="text-align:right;">

\-0.06

</td>

<td style="text-align:right;">

0.10

</td>

<td style="text-align:right;">

0.11

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

0.32

</td>

</tr>

<tr>

<td style="text-align:left;">

Rank

</td>

<td style="text-align:right;">

\-0.17

</td>

<td style="text-align:right;">

0.38

</td>

<td style="text-align:right;">

\-0.10

</td>

<td style="text-align:right;">

0.01

</td>

<td style="text-align:right;">

0.13

</td>

<td style="text-align:right;">

0.65

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

0.23

</td>

</tr>

<tr>

<td style="text-align:left;">

Sal

</td>

<td style="text-align:right;">

0.76

</td>

<td style="text-align:right;">

0.36

</td>

<td style="text-align:right;">

0.54

</td>

<td style="text-align:right;">

0.43

</td>

<td style="text-align:right;">

\-0.71

</td>

<td style="text-align:right;">

0.32

</td>

<td style="text-align:right;">

0.23

</td>

<td style="text-align:right;">

1.00

</td>

</tr>

</tbody>

</table>

``` r
#There is collinearity between prate and dept, prate and clin, we can remove prate.

sal_log = log(law$Sal)

par(mfrow = c(1,2))

hist(law$Sal, xlab = "Salary", freq = T, col = 2)

hist(sal_log, xlab = "lm(Salary)", freq = T, col = 2)
```

<img src="m_model_files/figure-gfm/unnamed-chunk-2-2.png" width="90%" />

# Model

``` r
law_df = read_csv("./data/Lawsuit.csv") %>% 
  janitor::clean_names() %>% 
  mutate(mean_sal = log((sal94 + sal95)/2)) %>%
  mutate(gender = factor(gender, levels = c(0,1), labels = c("Female", "Male")),
         dept = factor(dept, levels = c(1,2,3,4,5,6), 
                       labels = c("Biochemistry/Molecular Biology", "Physiology", "Genetics", "Pediatrics", "Medicine", "Surgery")),
         clin = factor(clin, levels = c(0,1), labels = c("Primarily research emphasis", "Primarily clinical emphasis")),
         cert = factor(cert, levels = c(0,1), labels = c("not certified", "Board certified")),
         rank = factor(rank, levels = c(1,2,3), labels = c("Assistant", "Associate","Full professor")))
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

## First, fit only gender in the model

``` r
model_0 = lm(mean_sal~gender, data = law_df)

summary(model_0)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender, data = law_df)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.1255 -0.3659 -0.0078  0.3342  1.0549 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.63319    0.04595 253.162  < 2e-16 ***
    ## genderMale   0.38530    0.05963   6.462  5.1e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4731 on 259 degrees of freedom
    ## Multiple R-squared:  0.1388, Adjusted R-squared:  0.1355 
    ## F-statistic: 41.75 on 1 and 259 DF,  p-value: 5.103e-10

As a single variable, gender is significant. Let’s check confounder and
interaction

## Next see if a variable is a confounder or interaction

``` r
model_dept1 = lm(mean_sal~gender + dept, data = law_df)

model_dept2 = lm(mean_sal~gender*dept, data = law_df)

summary(model_dept1)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender + dept, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.70544 -0.19433 -0.02501  0.16341  0.60779 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    11.34504    0.04110 276.036  < 2e-16 ***
    ## genderMale      0.20521    0.03355   6.116 3.60e-09 ***
    ## deptPhysiology -0.13194    0.05385  -2.450  0.01496 *  
    ## deptGenetics    0.20092    0.06602   3.044  0.00258 ** 
    ## deptPediatrics  0.30535    0.05919   5.158 5.02e-07 ***
    ## deptMedicine    0.64943    0.04569  14.215  < 2e-16 ***
    ## deptSurgery     1.07668    0.05453  19.743  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2534 on 254 degrees of freedom
    ## Multiple R-squared:  0.7578, Adjusted R-squared:  0.752 
    ## F-statistic: 132.4 on 6 and 254 DF,  p-value: < 2.2e-16

``` r
summary(model_dept2)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender * dept, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.64982 -0.19117 -0.03961  0.16822  0.65953 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               11.367761   0.056555 201.002  < 2e-16 ***
    ## genderMale                 0.167339   0.073013   2.292 0.022746 *  
    ## deptPhysiology            -0.210289   0.079981  -2.629 0.009090 ** 
    ## deptGenetics               0.097590   0.094942   1.028 0.305000    
    ## deptPediatrics             0.305925   0.079981   3.825 0.000165 ***
    ## deptMedicine               0.652114   0.073013   8.932  < 2e-16 ***
    ## deptSurgery                1.117268   0.126462   8.835  < 2e-16 ***
    ## genderMale:deptPhysiology  0.149119   0.108295   1.377 0.169760    
    ## genderMale:deptGenetics    0.207153   0.132451   1.564 0.119089    
    ## genderMale:deptPediatrics -0.032026   0.122174  -0.262 0.793436    
    ## genderMale:deptMedicine   -0.002786   0.093502  -0.030 0.976250    
    ## genderMale:deptSurgery    -0.034484   0.141254  -0.244 0.807330    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2529 on 249 degrees of freedom
    ## Multiple R-squared:  0.7634, Adjusted R-squared:  0.7529 
    ## F-statistic: 73.03 on 11 and 249 DF,  p-value: < 2.2e-16

``` r
# department is a confounder, not a modifier, and should be included in the model
```

``` r
model_rank1 = lm(mean_sal~gender + rank, data = law_df)

model_rank2 = lm(mean_sal~gender*rank, data = law_df)

summary(model_rank1)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender + rank, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.10798 -0.37872  0.01491  0.35397  1.03532 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        11.615638   0.050964 227.919  < 2e-16 ***
    ## genderMale          0.349481   0.064420   5.425 1.34e-07 ***
    ## rankAssociate      -0.005225   0.076187  -0.069   0.9454    
    ## rankFull professor  0.123166   0.073223   1.682   0.0938 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4716 on 257 degrees of freedom
    ## Multiple R-squared:  0.151,  Adjusted R-squared:  0.1411 
    ## F-statistic: 15.23 on 3 and 257 DF,  p-value: 3.747e-09

``` r
summary(model_rank2)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender * rank, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.04274 -0.37104  0.01751  0.35145  0.96675 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   11.55040    0.05617 205.621  < 2e-16 ***
    ## genderMale                     0.51942    0.09066   5.729 2.83e-08 ***
    ## rankAssociate                  0.12859    0.11629   1.106  0.26985    
    ## rankFull professor             0.37976    0.12947   2.933  0.00366 ** 
    ## genderMale:rankAssociate      -0.27200    0.15379  -1.769  0.07814 .  
    ## genderMale:rankFull professor -0.40566    0.15806  -2.567  0.01084 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4666 on 255 degrees of freedom
    ## Multiple R-squared:  0.1752, Adjusted R-squared:  0.1591 
    ## F-statistic: 10.84 on 5 and 255 DF,  p-value: 1.829e-09

``` r
# rank is a confounder, and a modifier.
```

``` r
model_exper1 = lm(mean_sal~gender + exper, data = law_df)

model_exper2 = lm(mean_sal~gender*exper, data = law_df)

summary(model_exper1)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender + exper, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.03299 -0.37372  0.04036  0.33276  1.02947 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 11.50693    0.05839 197.079  < 2e-16 ***
    ## genderMale   0.30755    0.06277   4.900  1.7e-06 ***
    ## exper        0.01686    0.00496   3.399 0.000784 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4637 on 258 degrees of freedom
    ## Multiple R-squared:  0.1757, Adjusted R-squared:  0.1693 
    ## F-statistic:  27.5 on 2 and 258 DF,  p-value: 1.488e-11

``` r
summary(model_exper2)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender * exper, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.99432 -0.35888  0.01122  0.35331  1.00991 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      11.40986    0.09286 122.870  < 2e-16 ***
    ## genderMale        0.44592    0.12060   3.698 0.000266 ***
    ## exper             0.02982    0.01085   2.749 0.006402 ** 
    ## genderMale:exper -0.01637    0.01219  -1.343 0.180482    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.463 on 257 degrees of freedom
    ## Multiple R-squared:  0.1815, Adjusted R-squared:  0.1719 
    ## F-statistic: 18.99 on 3 and 257 DF,  p-value: 3.709e-11

``` r
# exper is a confounder, not a modifier, and should be included in the model
```

``` r
model_clin1 = lm(mean_sal~gender + clin, data = law_df)

model_clin2 = lm(mean_sal~gender*clin, data = law_df)

summary(model_clin1)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender + clin, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.00911 -0.25023 -0.01895  0.24564  0.99426 
    ## 
    ## Coefficients:
    ##                                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     11.28894    0.04470 252.526  < 2e-16 ***
    ## genderMale                       0.33718    0.04671   7.218 5.89e-12 ***
    ## clinPrimarily clinical emphasis  0.60819    0.04710  12.912  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3694 on 258 degrees of freedom
    ## Multiple R-squared:  0.4769, Adjusted R-squared:  0.4728 
    ## F-statistic: 117.6 on 2 and 258 DF,  p-value: < 2.2e-16

``` r
summary(model_clin2)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender * clin, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.02449 -0.25395 -0.01342  0.25344  0.96159 
    ## 
    ## Coefficients:
    ##                                            Estimate Std. Error t value
    ## (Intercept)                                11.32237    0.05446 207.921
    ## genderMale                                  0.27578    0.07379   3.737
    ## clinPrimarily clinical emphasis             0.54912    0.07238   7.587
    ## genderMale:clinPrimarily clinical emphasis  0.10241    0.09530   1.075
    ##                                            Pr(>|t|)    
    ## (Intercept)                                 < 2e-16 ***
    ## genderMale                                 0.000229 ***
    ## clinPrimarily clinical emphasis            6.03e-13 ***
    ## genderMale:clinPrimarily clinical emphasis 0.283568    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3693 on 257 degrees of freedom
    ## Multiple R-squared:  0.4792, Adjusted R-squared:  0.4731 
    ## F-statistic: 78.83 on 3 and 257 DF,  p-value: < 2.2e-16

``` r
# clin is a confounder, not a modifier, and should be included in the model
```

``` r
model_cert1 = lm(mean_sal~gender + cert, data = law_df)

model_cert2 = lm(mean_sal~gender*cert, data = law_df)

summary(model_cert1)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender + cert, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.11866 -0.27669 -0.01921  0.31081  1.01942 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         11.28992    0.05510 204.888  < 2e-16 ***
    ## genderMale           0.33285    0.05233   6.360 9.11e-10 ***
    ## certBoard certified  0.51981    0.05726   9.078  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4127 on 258 degrees of freedom
    ## Multiple R-squared:  0.3473, Adjusted R-squared:  0.3423 
    ## F-statistic: 68.65 on 2 and 258 DF,  p-value: < 2.2e-16

``` r
summary(model_cert2)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender * cert, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.12871 -0.28089 -0.00744  0.30056  1.05149 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                    11.32289    0.06883 164.515  < 2e-16 ***
    ## genderMale                      0.26781    0.09667   2.770  0.00601 ** 
    ## certBoard certified             0.46989    0.08469   5.548 7.17e-08 ***
    ## genderMale:certBoard certified  0.09205    0.11501   0.800  0.42423    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.413 on 257 degrees of freedom
    ## Multiple R-squared:  0.3489, Adjusted R-squared:  0.3413 
    ## F-statistic: 45.91 on 3 and 257 DF,  p-value: < 2.2e-16

``` r
# cert is a confounder, not a modifier, and should be included in the model
```

## Finally fit new model and diagnostic

``` r
mult_fit = lm(mean_sal ~ gender*rank + cert + exper + clin + dept, data = law_df)

summary(mult_fit)
```

    ## 
    ## Call:
    ## lm(formula = mean_sal ~ gender * rank + cert + exper + clin + 
    ##     dept, data = law_df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.32667 -0.08080 -0.01075  0.07646  0.86686 
    ## 
    ## Coefficients:
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     10.959335   0.027936 392.307  < 2e-16 ***
    ## genderMale                       0.074479   0.027568   2.702  0.00738 ** 
    ## rankAssociate                    0.173142   0.033904   5.107 6.55e-07 ***
    ## rankFull professor               0.282281   0.039594   7.129 1.11e-11 ***
    ## certBoard certified              0.191213   0.021363   8.951  < 2e-16 ***
    ## exper                            0.018171   0.001806  10.064  < 2e-16 ***
    ## clinPrimarily clinical emphasis  0.197031   0.022175   8.885  < 2e-16 ***
    ## deptPhysiology                  -0.175544   0.028871  -6.080 4.53e-09 ***
    ## deptGenetics                     0.184572   0.036206   5.098 6.84e-07 ***
    ## deptPediatrics                   0.208468   0.035528   5.868 1.41e-08 ***
    ## deptMedicine                     0.543204   0.029364  18.499  < 2e-16 ***
    ## deptSurgery                      0.931388   0.035267  26.409  < 2e-16 ***
    ## genderMale:rankAssociate        -0.082943   0.044750  -1.853  0.06501 .  
    ## genderMale:rankFull professor   -0.105271   0.046654  -2.256  0.02492 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1325 on 247 degrees of freedom
    ## Multiple R-squared:  0.9355, Adjusted R-squared:  0.9322 
    ## F-statistic: 275.8 on 13 and 247 DF,  p-value: < 2.2e-16

``` r
HH::vif(mult_fit)
```

    ##                      genderMale                   rankAssociate 
    ##                        2.723525                        3.161073 
    ##              rankFull professor             certBoard certified 
    ##                        5.115323                        1.366064 
    ##                           exper clinPrimarily clinical emphasis 
    ##                        1.871214                        1.733192 
    ##                  deptPhysiology                    deptGenetics 
    ##                        1.607201                        1.441025 
    ##                  deptPediatrics                    deptMedicine 
    ##                        1.907952                        2.723163 
    ##                     deptSurgery        genderMale:rankAssociate 
    ##                        2.398163                        4.094401 
    ##   genderMale:rankFull professor 
    ##                        6.289418

# Outliers/influential points

``` r
state_res = rstandard(mult_fit)
state_hat = hatvalues(mult_fit)
outliers_x = state_hat[state_hat > 0.2]
outliers_x
```

    ## named numeric(0)

``` r
outliers_y = state_res[abs(state_res) > 2.5]
outliers_y
```

    ##       122       184       208 
    ## -2.519036  6.780977  2.585976

``` r
# there is no outlier in X, outliers in Y are obs 122, 184, 208

par(mfrow = c(2,2))
plot(mult_fit)
```

<img src="m_model_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
# obs 184 is influencial point

#Remove observations 184
law_df_no184 = law_df[-184,]
model_no184 = lm(mean_sal~gender + cert + exper + dept + clin, data = law_df_no184)
summary(law_df_no184)
```

    ##        id                                     dept       gender   
    ##  Min.   :  1.00   Biochemistry/Molecular Biology:50   Female:106  
    ##  1st Qu.: 65.75   Physiology                    :40   Male  :154  
    ##  Median :130.50   Genetics                      :21               
    ##  Mean   :130.80   Pediatrics                    :30               
    ##  3rd Qu.:196.25   Medicine                      :79               
    ##  Max.   :261.00   Surgery                       :40               
    ##                           clin                  cert         prate      
    ##  Primarily research emphasis:100   not certified  : 72   Min.   :1.300  
    ##  Primarily clinical emphasis:160   Board certified:188   1st Qu.:3.200  
    ##                                                          Median :4.400  
    ##                                                          Mean   :4.932  
    ##                                                          3rd Qu.:6.925  
    ##                                                          Max.   :8.700  
    ##      exper                   rank         sal94            sal95       
    ##  Min.   : 1.00   Assistant     :111   Min.   : 34514   Min.   : 38675  
    ##  1st Qu.: 6.00   Associate     : 64   1st Qu.: 90736   1st Qu.: 99855  
    ##  Median : 9.00   Full professor: 85   Median :133191   Median :147722  
    ##  Mean   :10.26                        Mean   :153223   Mean   :168494  
    ##  3rd Qu.:14.00                        3rd Qu.:197664   3rd Qu.:217847  
    ##  Max.   :37.00                        Max.   :428876   Max.   :472589  
    ##     mean_sal    
    ##  Min.   :10.51  
    ##  1st Qu.:11.46  
    ##  Median :11.86  
    ##  Mean   :11.86  
    ##  3rd Qu.:12.25  
    ##  Max.   :13.01

``` r
#Check the model assumptions
par(mfrow = c(2,2))
plot(model_no184)
```

<img src="m_model_files/figure-gfm/unnamed-chunk-6-2.png" width="90%" />
