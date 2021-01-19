
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multiple Imputation by Super Learning (MISL)

[![Travis-CI Build
Status](https://travis-ci.com/carpenitoThomas/misl.svg?token=u9TyfsxVjq6xxvc5h473&branch=master)](https://travis-ci.com/carpenitoThomas/misl)
[![Coverage
Status](https://codecov.io/gh/carpenitoThomas/misl/branch/master/graph/badge.svg?token=C157LCBBJI)](https://codecov.io/gh/carpenitoThomas/misl)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

The goal of MISL (Multiple Imputation by Super Learning) is to create
multiply imputed datasets using the super learning framework. This
package builds heavily off of the `sl3` and `mice` packages.

## Installation

You can install the released version of misl from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("misl")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("carpenitoThomas/misl")
```

## Example

Here’s an example with the nhanes data in which we use `misl()`
imputation and then pool the results:

``` r
library(misl)

misl_imp <- misl(nhanes, maxit = 1, m = 3, quiet = TRUE)

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(TotChol ~ Age + Weight + Height + Smoke100 + Education, data = y)
})

summary(mice::pool(misl_modeling))
#>                      term     estimate    std.error  statistic        df
#> 1             (Intercept)  5.796203599 0.2834701153 20.4473180  208.7238
#> 2                     Age  0.007709200 0.0008908660  8.6536019 3686.0959
#> 3                  Weight  0.000632672 0.0008285656  0.7635751  254.7670
#> 4                  Height -0.008138830 0.0018082439 -4.5009583  124.3602
#> 5                Smoke100  0.012844816 0.0317406548  0.4046803 2960.4836
#> 6 Education9 - 11th Grade  0.189701125 0.0687421717  2.7596033 3890.5501
#> 7   EducationCollege Grad  0.233800587 0.0641049529  3.6471532 1087.7208
#> 8    EducationHigh School  0.194493393 0.0643960759  3.0202678 2412.1133
#> 9   EducationSome College  0.216553820 0.0619527697  3.4954663 3402.6398
#>        p.value
#> 1 0.0000000000
#> 2 0.0000000000
#> 3 0.4458270077
#> 4 0.0000153465
#> 5 0.6857417906
#> 6 0.0058142480
#> 7 0.0002777204
#> 8 0.0025520097
#> 9 0.0004792783
```
