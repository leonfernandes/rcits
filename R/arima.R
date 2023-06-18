#' Simulate from arima models
#'
#' The innovations `innov` are assumed to include the burn-in phase. Only the
#' last `nsim` simulations are returned.
#' @param object Arima object
#' @param nsim positive integer. Length of time series to be generated.
#' @param innov vector of innovations.
#' @param ... not used.
#' @return a [tibble][tibble::tibble-package] of class "arima_tbl".
#' @rdname simts_arima
#' @export
simts.arima <-
    function(object, nsim, innov, ...) {
        simts_arima_impl(
            object$phi, object$theta, object$delta, nsim, innov, ...
        )
    }

#' @rdname simts_arima
#' @export
simts._Arima_fit_impl <-
    function(object, nsim, innov, ...) {
        object <- extract_arima(object)
        simts(object, nsim, innov, ...)
    }

#' @rdname simts_arima
#' @export
simts.Arima <-
    function(object, nsim, innov, ...) {
        object <- extract_arima(object)
        simts(object, nsim, innov, ...)
    }

simts_arima_impl <-
    function(phi, theta, delta, nsim, innov, ...) {
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
        tibble::tibble(
            x = utils::tail(ret, nsim), date = Sys.Date() + 1:nsim
        ) |>
            tibble::new_tibble("arima_tbl")
    }