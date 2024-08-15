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

#' Simulate from arima models
#'
#' The innovations `innov` are assumed to include the burn-in phase. Only the
#' last `nsim` simulations are returned.
#' The implementation is based on `stats::arima.sim`.
#' @param object a fitted arima model
#' @param nsim positive integer. Length of time series to be generated.
#' @param innov vector of innovations.
#' @param ... not used.
#' @return a [tsibble][tsibble::tsibble-package] of class "arima_ts". Column
#'      "value" corresponds to the simulated time series and column "date" has
#'      dummy dates for each observation.
#' @export
#' @examples
#' # simulate from an AR(1) model
#' mdl <- make_arima(phi = 0.3, theta = 0, delta = 0)
#' simults(mdl, nsim = 100, innov = rnorm(200))
simults.arima <- function(object, nsim, innov, ...) {
    phi <- object$phi
    theta <- object$theta
    delta <- object$delta
    len_delta <- length(delta)
    len_phi <- length(phi)
    if (!is.numeric(innov)) rlang::abort("`innov` is not numeric")
    if (vctrs::vec_size(innov) <= nsim) {
        rlang::abort(
            "Length of `innov` should be more than `nsim`."
        )
    }
    ret <- innov
    # apply theta
    if (length(theta)) {
        ret <- stats::filter(ret, c(1, theta), sides = 1L) |>
            stats::na.omit() |>
            as.numeric()
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
        date = Sys.Date() + 1:nsim,
        value = utils::tail(ret, nsim),
        index = date
    ) |>
        tsibble::new_tsibble("arima_ts")
}


#' Fitted residuals for ARIMA
#'
#' @inheritParams fitted_resid
#' @param new_data a [tsibble][tsibble::tsibble-package] of univariate
#'      time series on which residuals are to be calculated.
#' @param ... not used
#' @return a [tsibble][tsibble::tsibble-package] of fitted residuals of class
#'      `fitted_resid_ts`.
#' @export
fitted_resid.arima <- function(object, new_data, ...) {
    if (!inherits(object, "arima")) rlang::abort("Not arima object.")
    phi <- object$phi
    theta <- object$theta
    delta <- object$delta
    len_delta <- length(delta)
    len_phi <- length(phi)
    # Extract response column
    new_data_response_var <- tsibble::measured_vars(new_data)
    ret <- new_data[[new_data_response_var]]
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
    new_data |>
        dplyr::mutate(".resid" = ret) |>
        dplyr::select(-!!new_data_response_var) |>
        tsibble::new_tsibble(class = "fitted_resid_ts")
}