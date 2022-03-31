
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
                  con_method = c("Lrnr_glm_fast", "Lrnr_mean"),
                  bin_method = c("Lrnr_mean", "Lrnr_glm_fast"),
                  cat_method = c("Lrnr_independent_binomial", "Lrnr_mean"))

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(Whole_Weight ~ Sex + Length + Diameter + Height + Older_12, data = y$datasets)
})

summary(mice::pool(misl_modeling), conf.int = TRUE)
#>          term     estimate   std.error   statistic        df      p.value
#> 1 (Intercept) -0.947623384 0.027847374 -34.0291823 10.771964 2.597922e-12
#> 2        SexI -0.045806881 0.010350211  -4.4256955 36.635632 8.312807e-05
#> 3        SexM -0.001394636 0.007976192  -0.1748498 60.701997 8.617795e-01
#> 4      Length  1.029861122 0.216725520   4.7519144 15.100531 2.525098e-04
#> 5    Diameter  2.044891961 0.277137724   7.3786128 14.357777 2.982464e-06
#> 6      Height  2.846376758 0.231297999  12.3061019  7.201350 4.269258e-06
#> 7    Older_12  0.068027399 0.012868046   5.2865367  7.349247 9.749150e-04
#>         2.5 %      97.5 %
#> 1 -1.00907372 -0.88617305
#> 2 -0.06678544 -0.02482832
#> 3 -0.01734560  0.01455633
#> 4  0.56818932  1.49153292
#> 5  1.45187724  2.63790668
#> 6  2.30252871  3.39022481
#> 7  0.03788999  0.09816481
```

We can also look at the traceplot of the imputations as well:

``` r
#misl::plot(misl_imp)
```
