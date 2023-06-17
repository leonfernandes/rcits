#' Simulate from fitted ARIMA model
#'
#' The innovations `innov` are assumed to include the burn-in phase. Only the
#' last `nsim` simulations are returned.
#' @param object Arima object
#' @param nsim positive integer. Length of time series to be generated.
#' @param innov vector of innovations.
#' @param ... not used.
#' @return a [tibble][tibble::tibble-package] of class "arima_tbl".
#' @export
sim_arima <-
    function(object, ...) {
        UseMethod("sim_arima")
    }

#' @rdname sim_arima
#' @export
sim_arima._Arima_fit_impl <-
    function(object, nsim, innov, ...) {
        object <- extract_arima(object)
        sim_arima(object, nsim, innov, ...)
    }

#' @rdname sim_arima
#' @export
sim_arima.Arima <-
    function(object, nsim, innov, ...) {
        object <- extract_arima(object)
        sim_arima(object, nsim, innov, ...)
    }

#' @rdname sim_arima
#' @export
sim_arima.arima <-
    function(object, nsim, innov, ...) {
        sim_arima_impl(object$phi, object$theta, object$delta, nsim, innov, ...)
    }

sim_arima_impl <-
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
        # apply Theta
        if (length(theta)) {
            ret <-
                stats::filter(ret, c(1, theta), sides = 1L) |>
                stats::na.omit() |>
                as.numeric()
        }
        # apply Phi
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