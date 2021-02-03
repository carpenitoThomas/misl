
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

misl_imp <- misl(nhanes, maxit = 2, m = 2, quiet = TRUE)
#> Warning: UNRELIABLE VALUE: Future ('future_lapply-1') unexpectedly generated
#> random numbers without specifying argument 'future.seed'. There is a risk that
#> those random numbers are not statistically sound and the overall results might
#> be invalid. To fix this, specify 'future.seed=TRUE'. This ensures that proper,
#> parallel-safe random numbers are produced via the L'Ecuyer-CMRG method. To
#> disable this check, use 'future.seed=NULL', or set option 'future.rng.onMisuse'
#> to "ignore".

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(TotChol ~ Age + Weight + Height + Smoke100 + Education, data = y)
})

summary(mice::pool(misl_modeling))
#>                      term     estimate    std.error  statistic         df
#> 1             (Intercept)  4.494030741 0.1102412195 40.7654302  124.13756
#> 2                     Age  0.015269357 0.0006330859 24.1189323 4280.52218
#> 3                  Weight  0.003079035 0.0006194373  4.9706965 6567.65850
#> 4                  Height -0.003891016 0.0009442594 -4.1207062 1259.11873
#> 5                Smoke100 -0.008525189 0.0258431270 -0.3298822  314.59578
#> 6 Education9 - 11th Grade  0.228853391 0.0595294598  3.8443720   47.82588
#> 7   EducationCollege Grad  0.241475686 0.0520346945  4.6406669  120.18450
#> 8    EducationHigh School  0.225179863 0.0508468337  4.4285916 2031.19361
#> 9   EducationSome College  0.081420453 0.0425275507  1.9145343 6667.01926
#>        p.value
#> 1 0.000000e+00
#> 2 0.000000e+00
#> 3 6.840363e-07
#> 4 4.023961e-05
#> 5 7.417085e-01
#> 6 3.564632e-04
#> 7 8.925686e-06
#> 8 9.988893e-06
#> 9 5.559468e-02
```

This package also supports paralellization with the `future` package.
One can choose to paralellize either the outside creation of datasets or
the learners in the super learner library (or both\!). The following
snippet explains how this can be accomplished with four test-case
scenarios (with an assumption that our computer has 8 cores):

``` r
library(future)

# Sequential dataset processessing, Sequential super learning  (default)
plan(list(sequential,sequential))
seq_seq <- misl(nhanes)

# Sequential dataset processessing, paralell super learning (8) 
plan(list(sequential,tweak(multisession, workers = 8)))
seq_par <- misl(nhanes)

# Paralelle dataset processessing (8), sequential super learning 
plan(list(tweak(multisession, workers = 5), sequential))
par_seq <- misl(nhanes)

# paralell dataset processessing (4), paralell super learning (2) 
plan(list(tweak(multisession, workers = 4),tweak(multisession, workers = 2)))
par_par <- misl(nhanes)

# paralell dataset processing to ensure you don't overload your computer
plan(list(tweak(multisession, workers = availableCores() %/% 4),tweak(multisession, workers = 4)))
par_safe <- misl(nhanes)
```

Reminder, paralellizing code is not a silver bullet to automate making
runtime processes faster. Make sure you have an understanding of the
capacity of your computer. Further information about the topology of
running code in paralell can be found in the future package.
