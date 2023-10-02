
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simults

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/leonfernandes/simults/branch/master/graph/badge.svg)](https://app.codecov.io/gh/leonfernandes/simults?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/simults)](https://CRAN.R-project.org/package=simults)
[![BioC
status](http://www.bioconductor.org/shields/build/release/bioc/simults.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/simults)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of simults is to simulate time series from various time series
models.

## Installation

You can install the development version of simults like so:

``` r
pak::pkg_install("leonfernandes/simults")
```

## Simulate Data

Currently this package supports ARIMA and GARCH time series models from
the *smpspltools* package. Burn-in is done implicitly where sufficient
innovations are assumed to be supplied.

``` r
library(simults)
library(smpspltools)
# simulate from an AR(1) model
mdl <- make_arima(phi = 0.3, theta = 0, delta = 0)
simults(mdl, nsim = 100, innov = rnorm(200))
#> # A tsibble: 100 x 2 [1D]
#>    date         value
#>    <date>       <dbl>
#>  1 2023-10-03  1.87  
#>  2 2023-10-04 -0.253 
#>  3 2023-10-05 -0.208 
#>  4 2023-10-06 -1.60  
#>  5 2023-10-07 -0.722 
#>  6 2023-10-08  1.05  
#>  7 2023-10-09  1.22  
#>  8 2023-10-10 -0.0600
#>  9 2023-10-11 -0.349 
#> 10 2023-10-12  0.987 
#> # ℹ 90 more rows
```
