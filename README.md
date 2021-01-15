
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

misl_imp <- misl(nhanes, maxit = 2, m = 2, quiet = TRUE)

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(TotChol ~ Age + Weight + Height + Smoke100 + Education, data = y)
})

summary(mice::pool(misl_modeling))
#>                      term     estimate    std.error  statistic          df
#> 1             (Intercept)  4.538036722 0.1100107675 41.2508414 6766.866150
#> 2                     Age  0.015071298 0.0006294279 23.9444401 5655.218861
#> 3                  Weight  0.003012577 0.0006246664  4.8226974 1347.834327
#> 4                  Height -0.004199229 0.0010352553 -4.0562254   87.446555
#> 5                Smoke100  0.013687362 0.0260768931  0.5248847  261.827194
#> 6 Education9 - 11th Grade  0.240174045 0.0748621356  3.2082179    4.732437
#> 7   EducationCollege Grad  0.268423116 0.0691709244  3.8805773    4.368908
#> 8    EducationHigh School  0.241742271 0.0696531638  3.4706574    4.392088
#> 9   EducationSome College  0.087841421 0.0718903513  1.2218805    2.370217
#>        p.value
#> 1 0.000000e+00
#> 2 0.000000e+00
#> 3 1.577216e-06
#> 4 1.080386e-04
#> 5 6.001075e-01
#> 6 2.570262e-02
#> 7 1.508099e-02
#> 8 2.199388e-02
#> 9 3.292862e-01
```
