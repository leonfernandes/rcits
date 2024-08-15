
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simults

<!-- badges: start -->

<!-- badges: end -->

The goal of this package, given a causal and invertible time series
model, is two-fold: (i) given innovations, simulate observations via
`simults`, (ii) given observations, return innovations via
`fitted_resid`. The first is natural in simulating time series. The
second is useful for getting fitted residuals where the parameter could
have been estimated on a different dataset compared to the dataset on
which residuals are computed on.

This package implements the two methods for ARIMA and GARCH models.

## Installation

You can install the development version of simults like so:

``` r
pak::pkg_install("leonfernandes/simults")
```

## Simulate Data

We demonstrate the steps to simulate an AR(1) below.

``` r
library(simults)

mdl <- make_arima(phi=0.3, theta=0, delta=0)
z <- rnorm(200)
x <- simults(mdl, nsim=100, innov=z)
plot(x, type='l', main="Simulated AR(1)")
```

<img src="man/figures/README-sim-ar1-1.png" width="100%" />

Note the difference in lengths of `z` and `x`. The burn-in phase is
performed implicitly: all the values of `z` have been used in the
recursions for the AR(1) but only the last 100 observations are returned
to `x`.

## Exact Residuals

If we apply `fitted_resid` using the true model parameters, we should
recover the original innovations. This is verified below.

``` r
z0 <- fitted_resid(mdl, x) # Exact residuals
print(tail(z))
#> [1]  0.2219408 -0.4043768  0.1484931 -1.5217871 -0.6926848 -0.5946657
print(tail(z0$.resid))
#> [1]  0.2219408 -0.4043768  0.1484931 -1.5217871 -0.6926848 -0.5946657
```

## Fitted Residuals

Fit an AR(1) model on first half of the data and compute residuals on
all the data.

``` r
phi_hat <- stats::ar(x$value[1:50], order.max=1)$ar
fit_mdl <- make_arima(phi=phi_hat, theta=0, delta=0)
z_hat <- fitted_resid(fit_mdl, x)
print(tail(z_hat$.resid))
#> [1]  0.10417930 -0.37312297  0.03655622 -1.51082023 -1.14593083 -0.93844498
plot(z_hat, type='l', main="Fitted residuals from AR(1)")
```

<img src="man/figures/README-fitted-plot-1.png" width="100%" />
