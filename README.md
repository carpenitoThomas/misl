
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

misl_imp <- misl(abalone, maxit = 10, m = 10, quiet = TRUE,
    con_method = c("Lrnr_glm_fast", "Lrnr_earth", "Lrnr_ranger"),
    bin_method = c("Lrnr_earth", "Lrnr_glm_fast", "Lrnr_ranger"),
    cat_method = c("Lrnr_independent_binomial", "Lrnr_ranger"))
#> Growing trees.. Progress: 13%. Estimated remaining time: 1 hour, 45 minutes, 1 seconds.

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(Whole_Weight ~ Sex + Length + Diameter + Height + Older_12, data = y$datasets)
})

summary(mice::pool(misl_modeling), conf.int = TRUE)
#>          term      estimate   std.error    statistic        df      p.value
#> 1 (Intercept) -0.9917138520 0.017134443 -57.87838125 889.35930 0.000000e+00
#> 2        SexI -0.0426686559 0.009137655  -4.66954137 135.62160 7.174625e-06
#> 3        SexM  0.0002759165 0.007257143   0.03801998 174.92055 9.697151e-01
#> 4      Length  1.3620546956 0.206252463   6.60382269  30.35273 2.469290e-07
#> 5    Diameter  2.2108707044 0.250137186   8.83863267  32.66127 3.547722e-10
#> 6      Height  1.4354390562 0.138282429  10.38048769 101.74196 0.000000e+00
#> 7    Older_12  0.0474856116 0.008259594   5.74914621  45.01115 7.343897e-07
#>         2.5 %      97.5 %
#> 1 -1.02534251 -0.95808519
#> 2 -0.06073938 -0.02459794
#> 3 -0.01404692  0.01459875
#> 4  0.94103615  1.78307325
#> 5  1.70176219  2.71997922
#> 6  1.16114816  1.70972995
#> 7  0.03085005  0.06412117
```

We can also look at the traceplot of the imputations as
well:

``` r
misl::trace_plot(misl_imp)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />
