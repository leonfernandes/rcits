
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rcits

<!-- badges: start -->

<!-- badges: end -->

The goal of this package, given a causal and invertible time series
model, is two-fold: (i) given innovations, simulate observations via
`inn2ts`, (ii) given observations, return innovations via `ts2inn`. The
first is natural in simulating time series. The second is useful for
getting fitted residuals where the parameter could have been estimated
on a different dataset compared to the dataset on which residuals are
computed on.

This package implements the two methods for ARIMA and GARCH models.

## Installation

You can install the development version of simults like so:

``` r
pak::pkg_install("leonfernandes/rcits")
```

## Simulate Data

We demonstrate the steps to simulate an AR(1) below.

``` r
library(rcits)

mdl <- make_arima(phi=0.3, theta=0, delta=0)
z <- rnorm(200)
x <- inn2ts(mdl, inn=z)
plot(x, type='l', main="Simulated AR(1)")
```

<img src="man/figures/README-sim-ar1-1.png" width="100%" />

## Exact Residuals

If we apply `obs2inn` using the true model parameters, we should recover
the original innovations. This is verified below.

``` r
z0 <- ts2inn(mdl, x) # Exact residuals
print(tail(z))
#> [1]  0.4112346 -0.8136649 -0.4220300 -0.2034632  0.9574177  1.2681553
print(tail(z0$inn))
#> [1]  0.4112346 -0.8136649 -0.4220300 -0.2034632  0.9574177  1.2681553
```

## Fitted Residuals

Fit an AR(1) model on first half of the data and compute residuals on
all the data.

``` r
phi_hat <- stats::ar(x$ts[1:50], order.max=1)$ar
fit_mdl <- make_arima(phi=phi_hat, theta=0, delta=0)
z_hat <- ts2inn(fit_mdl, x)
print(tail(z_hat$inn))
#> [1]  0.3480185 -0.8582482 -0.3847166 -0.1659782  0.9813383  1.2156877
plot(z_hat, type='l', main="Fitted residuals from AR(1)")
```

<img src="man/figures/README-fitted-plot-1.png" width="100%" />
