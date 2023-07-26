#' Simulate from arima models
#'
#' The innovations `innov` are assumed to include the burn-in phase. Only the
#' last `nsim` simulations are returned.
#' The implementation is based on `stats::arima.sim`.
#' @param object either `arima`, `Arima` or `_Arima_fit_impl` object.
#' @param nsim positive integer. Length of time series to be generated.
#' @param innov vector of innovations.
#' @param ... not used.
#' @return a [tsibble][tsibble::tsibble-package] of class "arima_ts". Column
#'      "value" corresponds to the simulated time series and column "date" has
#'      dummy dates for each observation.
#' @rdname simults_arima
#' @export
#' @examples
#' # simulate from an AR(1) model
#' mdl <- smpspltools::make_arima(phi = 0.3, theta = 0, delta = 0)
#' simults(mdl, nsim = 100, innov = rnorm(200))
simults.arima <-
    function(object, nsim, innov, ...) {
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
            ret <-
                stats::filter(ret, c(1, theta), sides = 1L) |>
                stats::na.omit() |>
                as.numeric()
        }
        # apply phi
        if (len_phi) {
            ret <-
                stats::filter(ret, phi, method = "recursive") |>
                as.numeric()
        }
        # apply delta
        if (len_delta) {
            ret <-
                stats::filter(ret, delta, method = "recursive") |>
                as.numeric()
        }
        tsibble::tsibble(
            date = Sys.Date() + 1:nsim,
            value = utils::tail(ret, nsim),
            index = date
        ) |>
            tsibble::new_tsibble("arima_ts")
    }