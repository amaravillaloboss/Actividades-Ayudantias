Actividad Ayudantia 3
================

## Actividad en Clases

``` r
setwd('/Users/amara/Documents/GitHub/Actividades-Ayudantias/Ayudantia 3')
Titanic <- read.csv("titanic.csv")
#Titanic
```

``` r
boxplot(Titanic$AGE, horizontal = TRUE )
```

![](Ejercicio-Ayudantia3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
boxplot.stats(Titanic$AGE)
```

    ## $stats
    ## [1] 17 28 37 48 78
    ## 
    ## $n
    ## [1] 29286
    ## 
    ## $conf
    ## [1] 36.81535 37.18465
    ## 
    ## $out
    ##   [1] 90 90 90 82 80 84 90 90 90 90 83 80 84 90 84 81 83 90 82 80 82 81 79 84 84
    ##  [26] 84 80 81 79 90 80 85 81 82 90 80 79 79 81 90 81 83 79 80 79 79 90 90 81 82
    ##  [51] 80 84 87 90 82 82 88 80 90 79 90 90 90 90 80 90 80 80 90 81 90 90 80 90 81
    ##  [76] 79 81 79 82 90 90 81 80 86 82 90 79 90 82 80 90 90 90 79 90 88 90 79 80 79
    ## [101] 90 84 80 85 83 81 79 90 80 90 80 84 90 80 82 81 79 82 83 85 90 79 80 79 79
    ## [126] 90

``` r
Titanic_1 <- Titanic$AGE[Titanic$AGE< 79]
length(Titanic$AGE) - length(Titanic_1)
```

    ## [1] 126

``` r
boxplot.stats(Titanic_1)
```

    ## $stats
    ## [1] 17 28 37 47 75
    ## 
    ## $n
    ## [1] 29160
    ## 
    ## $conf
    ## [1] 36.8242 37.1758
    ## 
    ## $out
    ##  [1] 78 78 76 78 76 76 76 76 77 77 77 78 76 76 77 77 76 78 77 78 77 77 76 76 77
    ## [26] 76 76 76 76 76 77 76 78 76 76 77 76 76 76 77 78 76 77 77 77 76 78 76 76 76
    ## [51] 77 76 76 77 78 78 77 76 78 77 76 77 78 78 77 77 77 77 76 76 76 76 76 77 76
    ## [76] 76 78 76 78 76 76 77 76 78 76 78 78 76

``` r
boxplot(Titanic_1, horizontal = TRUE)
```

![](Ejercicio-Ayudantia3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
Titanic_2 <- Titanic$AGE[Titanic$AGE< 76]
length(Titanic$AGE) - length(Titanic_2)
```

    ## [1] 214

``` r
boxplot.stats(Titanic_2)
```

    ## $stats
    ## [1] 17 28 37 47 75
    ## 
    ## $n
    ## [1] 29072
    ## 
    ## $conf
    ## [1] 36.82393 37.17607
    ## 
    ## $out
    ## integer(0)

``` r
boxplot(Titanic_2, horizontal = TRUE)
```

![](Ejercicio-Ayudantia3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
