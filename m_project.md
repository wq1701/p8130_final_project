Final Project
================
Junhui Mi (jm4998)

``` r
law = read_csv("./data/Lawsuit.csv") %>%
  dplyr::select(-ID)
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
law$Dept <- ifelse(law$Dept == 6, 0, ifelse(law$Dept != 6, law$Dept, NA))
law$Rank <- ifelse(law$Rank == 3, 0, ifelse(law$Rank != 3, law$Rank, NA))

law_1 = law %>%
  mutate(
    Gender = as.factor(Gender),
    Dept = as.factor(Dept),
    Clin = as.factor(Clin),
    Cert = as.factor(Cert),
    Rank = as.factor(Rank)
  )

law_df = law %>%
  mutate(Gender = factor(Gender, levels = c(0,1), labels = c("Female", "Male")),
         Dept = factor(Dept, levels = c(0,1,2,3,4,5), 
                       labels = c("Surgery", "Biochemistry/Molecular Biology", "Physiology", "Genetics", "Pediatrics", "Medicine")),
         Clin = factor(Clin, levels = c(0,1), labels = c("Primarily research emphasis", "Primarily clinical emphasis")),
         Cert = factor(Cert, levels = c(0,1), labels = c("not certified", "Board certified")),
         Rank = factor(Rank, levels = c(0,1,2), labels = c("Full professor", "Assistant", "Associate")))

my_labels <- list(Dept = "Department", Gender = "Gender", Clin = "Research/Clinical emphasis", Cert = "Not/Board Certificated", Prate = "Publication rate", Exper = "Experience(years)", Rank = "Title", Sal94 = "Salary in 1994", Sal95 = "Salary in 1995")

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

tab1 <- tableby(Gender ~ Dept + Clin + Cert + Prate + Exper + Rank + Sal94 + Sal95, data = law_df, control = my_controls)

summary(tab1, title = "Summary by Gender", 
        labelTranslations = my_labels, text = T)
```

|                                   |         Female (N=106)          |           Male (N=155)           |          Total (N=261)          |
| :-------------------------------- | :-----------------------------: | :------------------------------: | :-----------------------------: |
| Department                        |                                 |                                  |                                 |
| \- Surgery                        |            5 (4.7%)             |            35 (22.6%)            |           40 (15.3%)            |
| \- Biochemistry/Molecular Biology |           20 (18.9%)            |            30 (19.4%)            |           50 (19.2%)            |
| \- Physiology                     |           20 (18.9%)            |            20 (12.9%)            |           40 (15.3%)            |
| \- Genetics                       |           11 (10.4%)            |            10 (6.5%)             |            21 (8.0%)            |
| \- Pediatrics                     |           20 (18.9%)            |            10 (6.5%)             |           30 (11.5%)            |
| \- Medicine                       |           30 (28.3%)            |            50 (32.3%)            |           80 (30.7%)            |
| Research/Clinical emphasis        |                                 |                                  |                                 |
| \- Primarily research emphasis    |           46 (43.4%)            |            55 (35.5%)            |           101 (38.7%)           |
| \- Primarily clinical emphasis    |           60 (56.6%)            |           100 (64.5%)            |           160 (61.3%)           |
| Not/Board Certificated            |                                 |                                  |                                 |
| \- not certified                  |           36 (34.0%)            |            37 (23.9%)            |           73 (28.0%)            |
| \- Board certified                |           70 (66.0%)            |           118 (76.1%)            |           188 (72.0%)           |
| Publication rate                  |                                 |                                  |                                 |
| \- Mean (SD)                      |           5.35 (1.89)           |           4.65 (1.94)            |           4.93 (1.94)           |
| \- Median (Q1, Q3)                |        5.25 (3.73, 7.27)        |        4.00 (3.10, 6.70)         |        4.40 (3.20, 6.90)        |
| \- Min - Max                      |           2.40 - 8.70           |           1.30 - 8.60            |           1.30 - 8.70           |
| Experience(years)                 |                                 |                                  |                                 |
| \- Mean (SD)                      |           7.49 (4.17)           |           12.10 (6.70)           |          10.23 (6.23)           |
| \- Median (Q1, Q3)                |       7.00 (5.00, 10.00)        |       10.00 (7.00, 15.00)        |       9.00 (6.00, 14.00)        |
| \- Min - Max                      |          1.00 - 23.00           |           2.00 - 37.00           |          1.00 - 37.00           |
| Title                             |                                 |                                  |                                 |
| \- Full professor                 |           16 (15.1%)            |            69 (44.5%)            |           85 (32.6%)            |
| \- Assistant                      |           69 (65.1%)            |            43 (27.7%)            |           112 (42.9%)           |
| \- Associate                      |           21 (19.8%)            |            43 (27.7%)            |           64 (24.5%)            |
| Salary in 1994                    |                                 |                                  |                                 |
| \- Mean (SD)                      |      118871.27 (56168.01)       |       177338.76 (85930.54)       |      153593.34 (80469.67)       |
| \- Median (Q1, Q3)                | 108457.00 (75774.50, 143096.00) | 155006.00 (109687.00, 231501.50) | 133284.00 (90771.00, 200543.00) |
| \- Min - Max                      |      34514.00 - 308081.00       |       52582.00 - 428876.00       |      34514.00 - 428876.00       |
| Salary in 1995                    |                                 |                                  |                                 |
| \- Mean (SD)                      |      130876.92 (62034.51)       |       194914.09 (94902.73)       |      168906.66 (88778.43)       |
| \- Median (Q1, Q3)                | 119135.00 (82345.25, 154170.50) | 170967.00 (119952.50, 257163.00) | 148117.00 (99972.00, 218955.00) |
| \- Min - Max                      |      38675.00 - 339664.00       |       58923.00 - 472589.00       |      38675.00 - 472589.00       |

Summary by Gender

``` r
attach(law)
pairs(law)
```

<img src="m_project_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />

``` r
cor_mat = round(cor(law),2)


par(mar = c(4,5,1,1))
par(mfrow = c(1,2))
cor(law_df[ ,c(5,6,8)]) %>% 
  corrplot(method = "circle", type = "upper", diag = FALSE)
cor(law_df[ ,c(5,6,9)]) %>% 
  corrplot(method = "circle", type = "upper", diag = FALSE)
```

<img src="m_project_files/figure-gfm/unnamed-chunk-2-2.png" width="90%" />

``` r
par(mfrow = c(2,2))
hist(law$Prate, xlab = "Publication rate", freq = T, col = 2)
hist(law$Exper, xlab = "Experience years", freq = T, col = 2)
hist(law$Sal94, xlab = "Salary in 1994", freq = T, col = 2)
hist(law$Sal95, xlab = "Salary in 1995", freq = T, col = 2)
```

<img src="m_project_files/figure-gfm/unnamed-chunk-2-3.png" width="90%" />

``` r
exper_log = log(law$Exper)
sal94_log = log(law$Sal94)
sal95_log = log(law$Sal95)
par(mfrow = c(2,2))
hist(law$Prate, xlab = "Publication rate", freq = T, col = 2)
hist(exper_log, xlab = "lm(Experience years)", freq = T, col = 2)
hist(sal94_log, xlab = "lm(Salary in 1994)", freq = T, col = 2)
hist(sal95_log, xlab = "lm(Salary in 1995)", freq = T, col = 2)
```

<img src="m_project_files/figure-gfm/unnamed-chunk-2-4.png" width="90%" />

``` r
law_trans = law_df %>%
  mutate(
    exper_log = log(law$Exper),
    sal94_log = log(law$Sal94),
    sal95_log = log(law$Sal95)
  ) %>% 
  dplyr::select(-Exper, -Sal94, -Sal95)


mult_fit94 = lm(sal94_log ~ Dept + Gender + Clin + Cert + Prate + Rank + exper_log, data = law_trans)
step(mult_fit94, direction = 'backward')
```

Start: AIC=-1026.52 sal94\_log \~ Dept + Gender + Clin + Cert + Prate +
Rank + exper\_log

``` 
        Df Sum of Sq     RSS      AIC
```

  - Gender 1 0.0169 4.6437 -1027.57
  - Prate 1 0.0351 4.6619 -1026.55 <none> 4.6268 -1026.52
  - Clin 1 0.2884 4.9152 -1012.74
  - Rank 2 1.0164 5.6432 -978.69
  - Cert 1 1.2120 5.8387 -967.80
  - exper\_log 1 1.5458 6.1726 -953.29
  - Dept 5 8.7226 13.3494 -759.97

Step: AIC=-1027.57 sal94\_log \~ Dept + Clin + Cert + Prate + Rank +
exper\_log

``` 
        Df Sum of Sq     RSS      AIC
```

<none> 4.6437 -1027.57 - Prate 1 0.0516 4.6953 -1026.69 - Clin 1 0.2718
4.9155 -1014.72 - Rank 2 1.0969 5.7406 -976.23 - Cert 1 1.2189 5.8626
-968.74 - exper\_log 1 1.6576 6.3013 -949.90 - Dept 5 8.8681 13.5118
-758.81

Call: lm(formula = sal94\_log \~ Dept + Clin + Cert + Prate + Rank +
exper\_log, data = law\_trans)

Coefficients: (Intercept) DeptBiochemistry/Molecular Biology  
12.0229 -0.8548  
DeptPhysiology DeptGenetics  
\-1.0226 -0.7053  
DeptPediatrics DeptMedicine  
\-0.7106 -0.3632  
ClinPrimarily clinical emphasis CertBoard certified  
0.1570 0.1783  
Prate RankAssistant  
\-0.0287 -0.2122  
RankAssociate exper\_log  
\-0.1053
0.1844

``` r
mult_fit94 = lm(sal94_log ~ Dept + Clin + Cert + Prate + Rank + exper_log, data = law_trans)
summary(mult_fit94)
```

Call: lm(formula = sal94\_log \~ Dept + Clin + Cert + Prate + Rank +
exper\_log, data = law\_trans)

Residuals: Min 1Q Median 3Q Max -0.31867 -0.08688 -0.01049 0.07913
1.00026

Coefficients: Estimate Std. Error t value Pr(\>|t|) (Intercept) 12.02293
0.09459 127.110 \< 2e-16 DeptBiochemistry/Molecular Biology -0.85480
0.06279 -13.614 \< 2e-16 DeptPhysiology -1.02258 0.06253 -16.354 \<
2e-16 DeptGenetics -0.70526 0.05522 -12.772 \< 2e-16 DeptPediatrics
-0.71058 0.03632 -19.565 \< 2e-16 DeptMedicine -0.36321 0.03182 -11.414
\< 2e-16 ClinPrimarily clinical emphasis 0.15696 0.04111 3.818 0.00017
CertBoard certified 0.17826 0.02205 8.085 2.72e-14 Prate -0.02870
0.01725 -1.664 0.09737 RankAssistant -0.21221 0.02772 -7.654 4.28e-13
RankAssociate -0.10530 0.02379 -4.426 1.43e-05 exper\_log 0.18439
0.01956 9.428 \< 2e-16

(Intercept) *** DeptBiochemistry/Molecular Biology *** DeptPhysiology
*** DeptGenetics *** DeptPediatrics *** DeptMedicine *** ClinPrimarily
clinical emphasis *** CertBoard certified *** Prate .  
RankAssistant *** RankAssociate *** exper\_log \*\*\* — Signif. codes: 0
‘***’ 0.001 ’**’ 0.01 ’*’ 0.05 ‘.’ 0.1 ’ ’ 1

Residual standard error: 0.1366 on 249 degrees of freedom Multiple
R-squared: 0.9309, Adjusted R-squared: 0.9278 F-statistic: 304.8 on 11
and 249 DF, p-value: \<
2.2e-16

``` r
mult_fit95 = lm(sal95_log ~ Dept + Gender + Clin + Cert + Prate + Rank + exper_log, data = law_trans)
step(mult_fit95, direction = 'backward')
```

Start: AIC=-1026.23 sal95\_log \~ Dept + Gender + Clin + Cert + Prate +
Rank + exper\_log

``` 
        Df Sum of Sq     RSS      AIC
```

  - Gender 1 0.0128 4.6448 -1027.51
  - Prate 1 0.0333 4.6653 -1026.36 <none> 4.6320 -1026.23
  - Clin 1 0.3163 4.9483 -1010.99
  - Rank 2 1.0310 5.6630 -977.78
  - Cert 1 1.1547 5.7867 -970.14
  - exper\_log 1 1.5583 6.1903 -952.54
  - Dept 5 8.6500 13.2820 -761.29

Step: AIC=-1027.51 sal95\_log \~ Dept + Clin + Cert + Prate + Rank +
exper\_log

``` 
        Df Sum of Sq     RSS      AIC
```

<none> 4.6448 -1027.51 - Prate 1 0.0474 4.6922 -1026.86 - Clin 1 0.3036
4.9484 -1012.99 - Rank 2 1.1063 5.7511 -975.75 - Cert 1 1.1607 5.8055
-971.29 - exper\_log 1 1.6628 6.3076 -949.64 - Dept 5 8.7813 13.4261
-760.47

Call: lm(formula = sal95\_log \~ Dept + Clin + Cert + Prate + Rank +
exper\_log, data = law\_trans)

Coefficients: (Intercept) DeptBiochemistry/Molecular Biology  
12.11256 -0.86173  
DeptPhysiology DeptGenetics  
\-1.02812 -0.71327  
DeptPediatrics DeptMedicine  
\-0.70687 -0.36861  
ClinPrimarily clinical emphasis CertBoard certified  
0.16587 0.17395  
Prate RankAssistant  
\-0.02749 -0.21325  
RankAssociate exper\_log  
\-0.10409
0.18468

``` r
mult_fit95 = lm(sal95_log ~ Dept + Clin + Cert + Prate + Rank + exper_log, data = law_trans)
summary(mult_fit95)
```

Call: lm(formula = sal95\_log \~ Dept + Clin + Cert + Prate + Rank +
exper\_log, data = law\_trans)

Residuals: Min 1Q Median 3Q Max -0.32369 -0.08028 -0.00854 0.07091
1.01026

Coefficients: Estimate Std. Error t value Pr(\>|t|) (Intercept) 12.11256
0.09460 128.042 \< 2e-16 DeptBiochemistry/Molecular Biology -0.86173
0.06280 -13.723 \< 2e-16 DeptPhysiology -1.02812 0.06253 -16.441 \<
2e-16 DeptGenetics -0.71327 0.05523 -12.915 \< 2e-16 DeptPediatrics
-0.70687 0.03632 -19.460 \< 2e-16 DeptMedicine -0.36861 0.03183 -11.582
\< 2e-16 ClinPrimarily clinical emphasis 0.16587 0.04112 4.034 7.30e-05
CertBoard certified 0.17395 0.02205 7.888 9.68e-14 Prate -0.02749
0.01725 -1.594 0.112 RankAssistant -0.21325 0.02773 -7.691 3.40e-13
RankAssociate -0.10409 0.02379 -4.375 1.79e-05 exper\_log 0.18468
0.01956 9.441 \< 2e-16

(Intercept) *** DeptBiochemistry/Molecular Biology *** DeptPhysiology
*** DeptGenetics *** DeptPediatrics *** DeptMedicine *** ClinPrimarily
clinical emphasis *** CertBoard certified *** Prate  
RankAssistant *** RankAssociate *** exper\_log \*\*\* — Signif. codes: 0
‘***’ 0.001 ’**’ 0.01 ’*’ 0.05 ‘.’ 0.1 ’ ’ 1

Residual standard error: 0.1366 on 249 degrees of freedom Multiple
R-squared: 0.9312, Adjusted R-squared: 0.9282 F-statistic: 306.3 on 11
and 249 DF, p-value: \< 2.2e-16
