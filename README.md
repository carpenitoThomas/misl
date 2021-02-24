
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

misl_imp <- misl(abalone, maxit = 5, m = 5, quiet = TRUE,
                  con_method = c("Lrnr_mean", "Lrnr_glm_fast", "Lrnr_earth", "Lrnr_glmnet", "Lrnr_polspline"),
                  bin_method = c("Lrnr_mean", "Lrnr_earth", "Lrnr_glm_fast"),
                  cat_method = c("Lrnr_independent_binomial", "Lrnr_mean"))
#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

#> Warning in if (length(cat_method == 1) & cat_method == "Lrnr_mean") {: the
#> condition has length > 1 and only the first element will be used

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(Whole_Weight ~ Sex + Length + Diameter + Height + Older_12, data = y$datasets)
})

summary(mice::pool(misl_modeling), conf.int = TRUE)
#>          term    estimate   std.error  statistic          df      p.value
#> 1 (Intercept) -1.03257632 0.016700130 -61.830436 2249.171260 0.000000e+00
#> 2        SexI -0.02797162 0.008661970  -3.229244  231.724751 1.421043e-03
#> 3        SexM  0.01739208 0.006720929   2.587749 1534.027044 9.751724e-03
#> 4      Length  1.93903177 0.189889013  10.211395   18.684182 4.446959e-09
#> 5    Diameter  1.44656728 0.197547685   7.322623   72.204451 2.759397e-10
#> 6      Height  1.73062838 0.498933179   3.468658    4.370206 2.221507e-02
#> 7    Older_12  0.04824471 0.007342443   6.570662   66.576509 9.028441e-09
#>          2.5 %      97.5 %
#> 1 -1.065325602 -0.99982705
#> 2 -0.045037899 -0.01090533
#> 3  0.004208899  0.03057526
#> 4  1.541134323  2.33692922
#> 5  1.052782213  1.84035234
#> 6  0.390442299  3.07081445
#> 7  0.033587422  0.06290201
```

We can also look at the traceplot of the imputations as well:

``` r
traceplot(misl_imp)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

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
