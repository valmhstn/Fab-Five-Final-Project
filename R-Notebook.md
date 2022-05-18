Wine Quality Feature Selection & Prediction Project
================
Autumn Heyman, Bethel Ikejiofor, Erin Weaver, Georgia Miller, Valerie
Huston

Here we are taking a look at the quality of Vinho Verde wines within a
region and select the more relevant physiochemical features that
contribute to wine quality and in which ways. This will be achieved
through the use of stepwise binary logistic regression.

# Importing Libraries

First we begin by *importing* our libraries.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.6     ✓ dplyr   1.0.8
    ## ✓ tidyr   1.2.0     ✓ stringr 1.4.0
    ## ✓ readr   2.1.2     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library("caret")
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library("lmtest")
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library("magrittr")
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
library("dplyr")
library("tidyr")
library("popbio")
```

    ## 
    ## Attaching package: 'popbio'

    ## The following object is masked from 'package:caret':
    ## 
    ##     sensitivity

``` r
library("e1071")
```

# Importing our Dataset

Next we *import* our dataset.

``` r
setwd('/Users/bethelikejiofor/Documents/GitHub/Fab-Five-Final-Project')
wine <- read.csv("./Data/WineQT.csv")

head(wine)
```

    ##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
    ## 1           7.4             0.70        0.00            1.9     0.076
    ## 2           7.8             0.88        0.00            2.6     0.098
    ## 3           7.8             0.76        0.04            2.3     0.092
    ## 4          11.2             0.28        0.56            1.9     0.075
    ## 5           7.4             0.70        0.00            1.9     0.076
    ## 6           7.4             0.66        0.00            1.8     0.075
    ##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
    ## 1                  11                   34  0.9978 3.51      0.56     9.4
    ## 2                  25                   67  0.9968 3.20      0.68     9.8
    ## 3                  15                   54  0.9970 3.26      0.65     9.8
    ## 4                  17                   60  0.9980 3.16      0.58     9.8
    ## 5                  11                   34  0.9978 3.51      0.56     9.4
    ## 6                  13                   40  0.9978 3.51      0.56     9.4
    ##   quality Id
    ## 1       5  0
    ## 2       5  1
    ## 3       5  2
    ## 4       6  3
    ## 5       5  4
    ## 6       5  5

# Some Data Wrangling

We beging by reformatting column names so there are no spaces.

``` r
names(wine) <- str_replace_all(names(wine), c(" "="."))
```

Next, we proceed to drop the ID column since it will not be used in our
analysis. We will also take a look again at the head of the dataframe to
make sure the wrangling changes took effect.

``` r
wine = subset(wine, select = -c(Id))
head(wine)
```

    ##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
    ## 1           7.4             0.70        0.00            1.9     0.076
    ## 2           7.8             0.88        0.00            2.6     0.098
    ## 3           7.8             0.76        0.04            2.3     0.092
    ## 4          11.2             0.28        0.56            1.9     0.075
    ## 5           7.4             0.70        0.00            1.9     0.076
    ## 6           7.4             0.66        0.00            1.8     0.075
    ##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
    ## 1                  11                   34  0.9978 3.51      0.56     9.4
    ## 2                  25                   67  0.9968 3.20      0.68     9.8
    ## 3                  15                   54  0.9970 3.26      0.65     9.8
    ## 4                  17                   60  0.9980 3.16      0.58     9.8
    ## 5                  11                   34  0.9978 3.51      0.56     9.4
    ## 6                  13                   40  0.9978 3.51      0.56     9.4
    ##   quality
    ## 1       5
    ## 2       5
    ## 3       5
    ## 4       6
    ## 5       5
    ## 6       5

# Assumptions Testing

For this project, rather than taking each of the individual quality
levels and doing a logistic regression against them, we will recode the
levels so wines either have either ‘good’ or ‘poor’ quality. Wines with
a quality between 3 and 5 will fall into the ‘poor’ quality level and
those between 6 and 8 will fall into the ‘good’ quality level.

## Recoding Wine Quality

``` r
wine$qualityR <- NA
wine$qualityR[wine$quality==3] <- 0
wine$qualityR[wine$quality==4] <- 0
wine$qualityR[wine$quality==5] <- 0
wine$qualityR[wine$quality==6] <- 1
wine$qualityR[wine$quality==7] <- 1
wine$qualityR[wine$quality==8] <- 1
head(wine)
```

    ##   fixed.acidity volatile.acidity citric.acid residual.sugar chlorides
    ## 1           7.4             0.70        0.00            1.9     0.076
    ## 2           7.8             0.88        0.00            2.6     0.098
    ## 3           7.8             0.76        0.04            2.3     0.092
    ## 4          11.2             0.28        0.56            1.9     0.075
    ## 5           7.4             0.70        0.00            1.9     0.076
    ## 6           7.4             0.66        0.00            1.8     0.075
    ##   free.sulfur.dioxide total.sulfur.dioxide density   pH sulphates alcohol
    ## 1                  11                   34  0.9978 3.51      0.56     9.4
    ## 2                  25                   67  0.9968 3.20      0.68     9.8
    ## 3                  15                   54  0.9970 3.26      0.65     9.8
    ## 4                  17                   60  0.9980 3.16      0.58     9.8
    ## 5                  11                   34  0.9978 3.51      0.56     9.4
    ## 6                  13                   40  0.9978 3.51      0.56     9.4
    ##   quality qualityR
    ## 1       5        0
    ## 2       5        0
    ## 3       5        0
    ## 4       6        1
    ## 5       5        0
    ## 6       5        0
