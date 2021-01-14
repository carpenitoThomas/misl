
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

misl_imp <- misl(nhanes, maxit = 2, m = 2, quiet = TRUE)

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(TotChol ~ Age + Weight + Height + Smoke100 + Education, data = y)
})

summary(mice::pool(misl_modeling))
#>                      term     estimate    std.error  statistic         df
#> 1             (Intercept)  4.565795636 0.1149755505 39.7110135 101.400312
#> 2                     Age  0.015125596 0.0006390698 23.6681459 819.118545
#> 3                  Weight  0.003125892 0.0006397174  4.8863643 221.247354
#> 4                  Height -0.004391819 0.0010196632 -4.3071276 127.385581
#> 5                Smoke100  0.011292815 0.0316854873  0.3564034   7.499389
#> 6 Education9 - 11th Grade  0.231236488 0.0600430448  3.8511786  38.353163
#> 7   EducationCollege Grad  0.263022444 0.0535395971  4.9126713  58.777697
#> 8    EducationHigh School  0.234936239 0.0554150897  4.2395716  33.353266
#> 9   EducationSome College  0.081504768 0.0453715024  1.7963868  66.029501
#>        p.value
#> 1 0.000000e+00
#> 2 0.000000e+00
#> 3 1.970862e-06
#> 4 3.273947e-05
#> 5 7.313563e-01
#> 6 4.337178e-04
#> 7 7.527704e-06
#> 8 1.668987e-04
#> 9 7.700736e-02
```
