
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multiple Imputation by Super Learning (MISL)

[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

The goal of MISL (Multiple Imputation by Super Learning) is to create
multiply imputed datasets using the super learning framework. This
package builds heavily off of the `sl3` and `mice` packages.

This method has been submitted for publication and is currently in
review.

## Installation

The MISL algorithm is not yet available on CRAN; instead, please use the
development version available here on Github. To download the
development version use:

``` r
# install.packages("devtools")
devtools::install_github("carpenitoThomas/misl")
```

## Example

Here’s an example with abalone data in which we use `misl()` imputation
and then pool the results:

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
#>          term     estimate   std.error   statistic         df      p.value
#> 1 (Intercept) -0.987047919 0.021019630 -46.9583879  26.057703 0.000000e+00
#> 2        SexI -0.043276012 0.009544576  -4.5340949  38.854378 5.414110e-05
#> 3        SexM  0.000770776 0.006959510   0.1107515 161.308054 9.119511e-01
#> 4      Length  1.349568205 0.179038602   7.5378616  25.961408 5.350155e-08
#> 5    Diameter  2.225230763 0.200859265  11.0785567  57.582699 6.661338e-16
#> 6      Height  1.388076073 0.268363687   5.1723692   5.787563 2.306323e-03
#> 7    Older_12  0.050900904 0.007995520   6.3661784  23.288962 1.603561e-06
#>         2.5 %      97.5 %
#> 1 -1.03024973 -0.94384611
#> 2 -0.06258405 -0.02396797
#> 3 -0.01297272  0.01451427
#> 4  0.98152246  1.71761395
#> 5  1.82310521  2.62735631
#> 6  0.72552659  2.05062555
#> 7  0.03437226  0.06742955
```

We can also look at the traceplot of the imputations as
well:

``` r
plot(misl_imp)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-3-2.png" width="100%" />

This package also supports parallel processing with the `future`
package. One can choose to parallelize either the outside creation of
datasets or the learners in the super learner library (or both\!). The
following snippet explains how this can be accomplished with four
test-case scenarios (with an assumption that our computer has 8 cores):

``` r
library(future)

# Sequential dataset processessing, Sequential super learning  (default)
plan(list(sequential,sequential))
seq_seq <- misl(abalone)

# Sequential dataset processessing, parallel super learning (8) 
plan(list(sequential,tweak(multisession, workers = 8)))
seq_par <- misl(abalone)

# Parallel dataset processessing (8), sequential super learning 
plan(list(tweak(multisession, workers = 5), sequential))
par_seq <- misl(abalone)

# Parallel dataset processessing (4), parallel super learning (2) 
plan(list(tweak(multisession, workers = 4),tweak(multisession, workers = 2)))
par_par <- misl(abalone)

# Parallel dataset processing to ensure you don't overload your computer
plan(list(tweak(multisession, workers = availableCores() %/% 4),tweak(multisession, workers = 4)))
par_safe <- misl(abalone)
```

Reminder, parallel code is not a silver bullet to automate making
runtime processes faster. Make sure you have an understanding of the
capacity of your computer. Further information about the topology of
running code in parallel can be found in the future package.
