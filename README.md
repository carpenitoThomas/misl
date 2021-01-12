
<!-- README.md is generated from README.Rmd. Please edit that file -->

# misl

<!-- badges: start -->

<!-- badges: end -->

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
devtools::install_github("carpenitoThomas/MISL")
```

## Example

Here’s an example with the nhanes data in which we use MISL imputation
and then pool the results:

``` r
library(misl)
library(mice)
#> 
#> Attaching package: 'mice'
#> The following objects are masked from 'package:base':
#> 
#>     cbind, rbind

misl_imp <- misl(mice::nhanes, maxit = 2, m = 2)
#> [1] "Imputing dataset: 1"
#> [1] "Imputing iteration: 1"
#> [1] "Imputing: age"
#> [1] "Imputing: hyp"
#> [1] "Imputing: bmi"
#> [1] "Imputing: chl"
#> [1] "Imputing iteration: 2"
#> [1] "Imputing: age"
#> [1] "Imputing: hyp"
#> [1] "Imputing: bmi"
#> [1] "Imputing: chl"
#> [1] "Imputing dataset: 2"
#> [1] "Imputing iteration: 1"
#> [1] "Imputing: age"
#> [1] "Imputing: hyp"
#> [1] "Imputing: bmi"
#> [1] "Imputing: chl"
#> [1] "Imputing iteration: 2"
#> [1] "Imputing: age"
#> [1] "Imputing: hyp"
#> [1] "Imputing: bmi"
#> [1] "Imputing: chl"

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(bmi ~ hyp + chl, data = y)
})

summary(mice::pool(misl_modeling))
#>          term    estimate  std.error  statistic       df     p.value
#> 1 (Intercept) 20.84723093 4.32108249  4.8245390 14.56379 0.000241836
#> 2         hyp -0.52786921 1.92793610 -0.2738002 16.83378 0.787569392
#> 3         chl  0.03156849 0.02245913  1.4055977 19.61482 0.175486020
```
