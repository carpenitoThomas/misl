
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multiple Imputation by Super Learning (MISL)

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
devtools::install_github("carpenitoThomas/misl")
```

## Example

Here’s an example with the nhanes data in which we use `misl()`
imputation and then pool the results:

``` r
library(misl)

misl_imp <- misl(nhanes, maxit = 2, m = 2, quiet = TRUE)

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(TotChol ~ Age + Weight + Height + Smoke100 + Education, data = y)
})

summary(mice::pool(misl_modeling))
#>                      term     estimate    std.error  statistic         df
#> 1             (Intercept)  4.560484400 0.1099404608 41.4814016 5640.49527
#> 2                     Age  0.015079860 0.0006320319 23.8593349 2896.66633
#> 3                  Weight  0.003201187 0.0006235016  5.1342077 3172.42226
#> 4                  Height -0.004502931 0.0009800801 -4.5944521 6089.20706
#> 5                Smoke100  0.018157167 0.0273859752  0.6630097   45.68105
#> 6 Education9 - 11th Grade  0.243570874 0.0556668340  4.3755115 1259.79710
#> 7   EducationCollege Grad  0.278377007 0.0519941733  5.3540039  167.89103
#> 8    EducationHigh School  0.253036015 0.0519685203  4.8690248  262.05890
#> 9   EducationSome College  0.101243631 0.0444235646  2.2790524  139.08709
#>        p.value
#> 1 0.000000e+00
#> 2 0.000000e+00
#> 3 3.004145e-07
#> 4 4.426319e-06
#> 5 5.106562e-01
#> 6 1.311608e-05
#> 7 2.793126e-07
#> 8 1.940894e-06
#> 9 2.418660e-02
```
