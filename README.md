
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
#>                      term      estimate    std.error   statistic         df
#> 1             (Intercept)  4.6564006230 0.1000874436 46.52332457  412.90653
#> 2                     Age  0.0157687084 0.0005385166 29.28175083  608.77488
#> 3                  Weight  0.0025877835 0.0005472410  4.72878195   96.90356
#> 4                  Height -0.0050954553 0.0008713365 -5.84786129  737.29116
#> 5                Smoke100 -0.0006840408 0.0212317424 -0.03221784  402.92259
#> 6 Education9 - 11th Grade  0.2666215163 0.0482433162  5.52660010 5268.06243
#> 7   EducationCollege Grad  0.3365688500 0.0431663330  7.79702205 4690.95283
#> 8    EducationHigh School  0.3228523534 0.0437720961  7.37575721 5725.36852
#> 9   EducationSome College  0.1635224142 0.0378514746  4.32010684 5617.47501
#>        p.value
#> 1 0.000000e+00
#> 2 0.000000e+00
#> 3 7.669874e-06
#> 4 7.481124e-09
#> 5 9.743143e-01
#> 6 3.421919e-08
#> 7 7.771561e-15
#> 8 1.865175e-13
#> 9 1.586426e-05
```
