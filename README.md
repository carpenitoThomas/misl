
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
#> 
#> Attaching package: 'misl'
#> The following object is masked from 'package:graphics':
#> 
#>     plot
#> The following object is masked from 'package:base':
#> 
#>     plot
set.seed(123)

misl_imp <- misl(abalone, maxit = 5, m = 5, quiet = TRUE,
                  con_method = c("Lrnr_glm_fast", "Lrnr_earth", "Lrnr_ranger"),
                  bin_method = c("Lrnr_earth", "Lrnr_glm_fast", "Lrnr_ranger"),
                  cat_method = c("Lrnr_independent_binomial", "Lrnr_ranger"))

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(Whole_Weight ~ Sex + Length + Diameter + Height + Older_12, data = y$datasets)
})

summary(mice::pool(misl_modeling), conf.int = TRUE)
#>          term    estimate   std.error  statistic         df      p.value
#> 1 (Intercept) -1.02932002 0.016927014 -60.809307 1310.67176 0.000000e+00
#> 2        SexI -0.02767941 0.008537256  -3.242190  338.20348 1.304257e-03
#> 3        SexM  0.01815814 0.007203195   2.520845  144.60212 1.279237e-02
#> 4      Length  1.81451491 0.146948804  12.347939  246.29487 0.000000e+00
#> 5    Diameter  1.67524674 0.191253346   8.759307   94.16393 7.860379e-14
#> 6      Height  1.49665468 0.158197280   9.460685   15.49830 7.786931e-08
#> 7    Older_12  0.05020394 0.006728222   7.461696  339.90279 7.212009e-13
#>          2.5 %      97.5 %
#> 1 -1.062527025 -0.99611302
#> 2 -0.044472213 -0.01088660
#> 3  0.003920984  0.03239529
#> 4  1.525078301  2.10395153
#> 5  1.295517359  2.05497613
#> 6  1.160406950  1.83290241
#> 7  0.036969748  0.06343814
```

We can also look at the traceplot of the imputations as
well:

``` r
plot(misl_imp)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />

This package also supports paralellization with the `future` package.
One can choose to paralellize either the outside creation of datasets or
the learners in the super learner library (or both\!). The following
snippet explains how this can be accomplished with four test-case
scenarios (with an assumption that our computer has 8 cores):

``` r
library(future)

# Sequential dataset processessing, Sequential super learning  (default)
plan(list(sequential,sequential))
seq_seq <- misl(abalone)

# Sequential dataset processessing, paralell super learning (8) 
plan(list(sequential,tweak(multisession, workers = 8)))
seq_par <- misl(abalone)

# Paralelle dataset processessing (8), sequential super learning 
plan(list(tweak(multisession, workers = 5), sequential))
par_seq <- misl(abalone)

# paralell dataset processessing (4), paralell super learning (2) 
plan(list(tweak(multisession, workers = 4),tweak(multisession, workers = 2)))
par_par <- misl(abalone)

# paralell dataset processing to ensure you don't overload your computer
plan(list(tweak(multisession, workers = availableCores() %/% 4),tweak(multisession, workers = 4)))
par_safe <- misl(abalone)
```

Reminder, paralellizing code is not a silver bullet to automate making
runtime processes faster. Make sure you have an understanding of the
capacity of your computer. Further information about the topology of
running code in paralell can be found in the future package.
