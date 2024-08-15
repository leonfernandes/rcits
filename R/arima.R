#' Constructor for arima object
#'
#' @inheritParams stats::makeARIMA
#' @param delta vector of differencing coefficients, so an ARMA model is fitted
#'      to `y[t] - delta[1]*y[t-1] - ...`.
#' @export
make_arima <- function(phi, theta, delta) {
    ret <- stats::makeARIMA(phi = phi, theta = theta, Delta = delta)
    class(ret) <- "arima"
    ret
}

#' Apply ARIMA recursions
#'
#' This implementation is based on `stats::arima.sim`.
#' @param object an `arima` object
#' @inheritParams inn2ts
#' @param ... not used.
#' @return a [tsibble][tsibble::tsibble-package] of class "arima_ts". Column
#'      "ts" corresponds to the simulated time series and column "date" has
#'      dummy dates for each observation.
#' @export
#' @examples
#' # simulate from an AR(1) model
#' mdl <- make_arima(phi = 0.3, theta = 0, delta = 0)
#' inn2ts(mdl, inn = rnorm(200))
inn2ts.arima <- function(object, inn, ...) {
    len_inn <- length(inn)
    phi <- object$phi
    theta <- object$theta
    delta <- object$delta
    len_delta <- length(delta)
    len_phi <- length(phi)
    if (!is.numeric(inn)) rlang::abort("`inn` is not numeric")
    ret <- inn
    # apply theta
    if (length(theta)) {
        ret <- stats::filter(ret, c(1, theta), sides = 1L) |>
            as.numeric()
        ret[1:length(theta)] <- 0
    }
    # apply phi
    if (len_phi) {
        ret <- stats::filter(ret, phi, method = "recursive") |>
            as.numeric()
    }
    # apply delta
    if (len_delta) {
        ret <- stats::filter(ret, delta, method = "recursive") |>
            as.numeric()
    }
    tsibble::tsibble(
        date = Sys.Date() + 1:len_inn,
        ts = ret,
        index = date
    ) |>
        tsibble::new_tsibble("arima_ts")
}


#' Fitted residuals for ARIMA
#'
#' @inheritParams ts2inn
#' @param ... not used.
#' @return a [tsibble][tsibble::tsibble-package] where column `inn` contains the
#'      calculated innovations.
#' @export
ts2inn.arima <- function(object, ts, ...) {
    if (!inherits(object, "arima")) rlang::abort("Not arima object.")
    phi <- object$phi
    theta <- object$theta
    delta <- object$delta
    len_delta <- length(delta)
    len_phi <- length(phi)
    # Extract response column
    ts_response_var <- tsibble::measured_vars(ts)
    ret <- ts[[ts_response_var]]
    ret <- c(rep(0, len_delta + len_phi), ret)
    # Differencing
    if (len_delta) {
        ret <- stats::filter(ret, c(1, -delta), sides = 1) |>
            stats::na.omit()
    }
    # Apply Phi
    if (len_phi) {
        ret <- stats::filter(ret, c(1, -phi), sides = 1) |>
            stats::na.omit()
    }
    # Apply Theta
    if (length(theta)) {
        ret <- stats::filter(ret, -theta, method = "recursive")
    }
    ts |>
        dplyr::mutate("inn" = ret) |>
        dplyr::select(-!!ts_response_var)
}