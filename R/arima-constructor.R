#' Constructor for arima object
#'
#' @inheritParams stats::makeARIMA
#' @param delta vector of differencing coefficients, so an ARMA model is fitted
#'      to `y[t] - delta[1]*y[t-1] - ...`.
#' @export
arima <-
    function(phi, theta, delta) {
        ret <- stats::makeARIMA(phi = phi, theta = theta, Delta = delta)
        class(ret) <- "arima"
        ret
    }