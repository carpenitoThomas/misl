
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

misl_imp <- misl(nhanes, maxit = 2, m = 2)

misl_modeling <- lapply(misl_imp, function(y){
  stats::lm(bmi ~ hyp + chl, data = y)
})

summary(mice::pool(misl_modeling))
```
