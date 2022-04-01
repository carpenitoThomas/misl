
<!-- README.md is generated from README.Rmd. Please edit that file -->

# misl <a href='/'><img src='misl_hex.png' align="right" height="139" /></a>

[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Multiple Imputation by Super Learning

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
install.packages("devtools")
devtools::install_github("carpenitoThomas/misl")
```

## Example

Here’s an example with abalone data in which we use `misl()` imputation
and then pool the results:

``` r
library(misl)
set.seed(123)

misl_imp <- misl(abalone, maxit = 20, m = 10, quiet = TRUE,
    con_method = c("Lrnr_glm_fast", "Lrnr_earth", "Lrnr_ranger"),
    bin_method = c("Lrnr_earth", "Lrnr_glm_fast", "Lrnr_ranger"),
    cat_method = c("Lrnr_independent_binomial", "Lrnr_ranger"))

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(Whole_Weight ~ Sex + Length + Diameter + Height + Older_12, data = y$datasets)
})

summary(mice::pool(misl_modeling), conf.int = TRUE)
#>          term     estimate   std.error   statistic        df      p.value
#> 1 (Intercept) -0.992171743 0.018252802 -54.3572290 208.63020 0.000000e+00
#> 2        SexI -0.044382542 0.008882474  -4.9966417 188.38235 1.327084e-06
#> 3        SexM -0.002187947 0.006761454  -0.3235912 692.98769 7.463452e-01
#> 4      Length  1.478661263 0.186511138   7.9280051  47.93463 2.855538e-10
#> 5    Diameter  2.070730580 0.243349864   8.5092736  37.05361 3.042553e-10
#> 6      Height  1.408162289 0.204656664   6.8806080  18.04273 1.929282e-06
#> 7    Older_12  0.052249600 0.007550405   6.9201057  77.10638 1.170662e-09
#>         2.5 %      97.5 %
#> 1 -1.02815531 -0.95618817
#> 2 -0.06190444 -0.02686065
#> 3 -0.01546334  0.01108744
#> 4  1.10364227  1.85368025
#> 5  1.57768100  2.56378016
#> 6  0.97826758  1.83805700
#> 7  0.03721515  0.06728405
```

We can also look at the traceplot of the imputations as
well:

``` r
misl::trace_plot(misl_imp)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />
