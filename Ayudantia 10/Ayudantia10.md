Ayudantia 10
================

## Librerias

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(e1071)
#install.packages('caret')
library(caret)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
#install.packages('rstanarm')
library(rstanarm)
```

    ## Loading required package: Rcpp

    ## This is rstanarm version 2.21.1

    ## - See https://mc-stan.org/rstanarm/articles/priors for changes to default priors!

    ## - Default priors may change, so it's safest to specify priors, even if equivalent to the defaults.

    ## - For execution on a local, multicore CPU with excess RAM we recommend calling

    ##   options(mc.cores = parallel::detectCores())

    ## 
    ## Attaching package: 'rstanarm'

    ## The following objects are masked from 'package:caret':
    ## 
    ##     compare_models, R2

``` r
#library(titanic)
```

## Cargar Datos

``` r
setwd('/Users/amara/Documents/GitHub/Actividades-Ayudantias/Ayudantia 10')
credit <- read.csv("UCI_Credit_Card.csv")

glimpse(credit)
```

    ## Rows: 30,000
    ## Columns: 25
    ## $ ID                         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, …
    ## $ LIMIT_BAL                  <dbl> 20000, 120000, 90000, 50000, 50000, 50000, …
    ## $ SEX                        <int> 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1…
    ## $ EDUCATION                  <int> 2, 2, 2, 2, 2, 1, 1, 2, 3, 3, 3, 1, 2, 2, 1…
    ## $ MARRIAGE                   <int> 1, 2, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2…
    ## $ AGE                        <int> 24, 26, 34, 37, 57, 37, 29, 23, 28, 35, 34,…
    ## $ PAY_0                      <int> 2, -1, 0, 0, -1, 0, 0, 0, 0, -2, 0, -1, -1,…
    ## $ PAY_2                      <int> 2, 2, 0, 0, 0, 0, 0, -1, 0, -2, 0, -1, 0, 2…
    ## $ PAY_3                      <int> -1, 0, 0, 0, -1, 0, 0, -1, 2, -2, 2, -1, -1…
    ## $ PAY_4                      <int> -1, 0, 0, 0, 0, 0, 0, 0, 0, -2, 0, -1, -1, …
    ## $ PAY_5                      <int> -2, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, -1, …
    ## $ PAY_6                      <int> -2, 2, 0, 0, 0, 0, 0, -1, 0, -1, -1, 2, -1,…
    ## $ BILL_AMT1                  <dbl> 3913, 2682, 29239, 46990, 8617, 64400, 3679…
    ## $ BILL_AMT2                  <dbl> 3102, 1725, 14027, 48233, 5670, 57069, 4120…
    ## $ BILL_AMT3                  <dbl> 689, 2682, 13559, 49291, 35835, 57608, 4450…
    ## $ BILL_AMT4                  <dbl> 0, 3272, 14331, 28314, 20940, 19394, 542653…
    ## $ BILL_AMT5                  <dbl> 0, 3455, 14948, 28959, 19146, 19619, 483003…
    ## $ BILL_AMT6                  <dbl> 0, 3261, 15549, 29547, 19131, 20024, 473944…
    ## $ PAY_AMT1                   <dbl> 0, 0, 1518, 2000, 2000, 2500, 55000, 380, 3…
    ## $ PAY_AMT2                   <dbl> 689, 1000, 1500, 2019, 36681, 1815, 40000, …
    ## $ PAY_AMT3                   <dbl> 0, 1000, 1000, 1200, 10000, 657, 38000, 0, …
    ## $ PAY_AMT4                   <dbl> 0, 1000, 1000, 1100, 9000, 1000, 20239, 581…
    ## $ PAY_AMT5                   <dbl> 0, 0, 1000, 1069, 689, 1000, 13750, 1687, 1…
    ## $ PAY_AMT6                   <dbl> 0, 2000, 5000, 1000, 679, 800, 13770, 1542,…
    ## $ default.payment.next.month <int> 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0…

``` r
credit$EDUCATION <- str_extract(credit$EDUCATION, "[0-9]")
credit$SEX <- (credit$SEX == "female") %>% as.numeric()

credit <- credit[c(4,3,5:10,2)]

str(credit)
```

    ## 'data.frame':    30000 obs. of  9 variables:
    ##  $ EDUCATION: chr  "2" "2" "2" "2" ...
    ##  $ SEX      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ MARRIAGE : int  1 2 2 1 1 2 2 2 1 2 ...
    ##  $ AGE      : int  24 26 34 37 57 37 29 23 28 35 ...
    ##  $ PAY_0    : int  2 -1 0 0 -1 0 0 0 0 -2 ...
    ##  $ PAY_2    : int  2 2 0 0 0 0 0 -1 0 -2 ...
    ##  $ PAY_3    : int  -1 0 0 0 -1 0 0 -1 2 -2 ...
    ##  $ PAY_4    : int  -1 0 0 0 0 0 0 0 0 -2 ...
    ##  $ LIMIT_BAL: num  20000 120000 90000 50000 50000 50000 500000 100000 140000 20000 ...

``` r
credit$EDUCATION <- NULL
credit$SEX <- NULL
credit$MARRIAGE <- NULL
credit$AGE <- NULL


str(credit)
```

    ## 'data.frame':    30000 obs. of  5 variables:
    ##  $ PAY_0    : int  2 -1 0 0 -1 0 0 0 0 -2 ...
    ##  $ PAY_2    : int  2 2 0 0 0 0 0 -1 0 -2 ...
    ##  $ PAY_3    : int  -1 0 0 0 -1 0 0 -1 2 -2 ...
    ##  $ PAY_4    : int  -1 0 0 0 0 0 0 0 0 -2 ...
    ##  $ LIMIT_BAL: num  20000 120000 90000 50000 50000 50000 500000 100000 140000 20000 ...

``` r
library(e1071)

#credit_linear <- stan_glm(Survived ~ LIMIT_BAL + as.factor(default.payment.next.month),data = credit, family = gaussian)
```
