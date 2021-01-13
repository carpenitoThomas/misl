
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
#>                      term      estimate    std.error  statistic        df
#> 1             (Intercept)  5.6328707806 0.2301070949 24.4793442  143.2236
#> 2                     Age  0.0085934170 0.0007332633 11.7194158 4555.9498
#> 3                  Weight  0.0002587501 0.0006491807  0.3985794 2818.2259
#> 4                  Height -0.0073416898 0.0014506829 -5.0608507  132.2360
#> 5                Smoke100  0.0029648092 0.0254164772  0.1166491 1520.9742
#> 6 Education9 - 11th Grade  0.2084123451 0.0601127263  3.4670253 2362.2259
#> 7   EducationCollege Grad  0.2829696978 0.0543703801  5.2044826 6892.4541
#> 8    EducationHigh School  0.2594646318 0.0554576045  4.6786123 6771.6069
#> 9   EducationSome College  0.2596451972 0.0537724508  4.8285915 6178.3221
#>        p.value
#> 1 0.000000e+00
#> 2 0.000000e+00
#> 3 6.902333e-01
#> 4 1.368272e-06
#> 5 9.071535e-01
#> 6 5.356581e-04
#> 7 2.001571e-07
#> 8 2.944332e-06
#> 9 1.408132e-06
```
