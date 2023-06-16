#' Simulate from fitted ARIMA model
#'
#' The innovations `innov` are assumed to include the burn-in phase. Only the
#' last `nsim` simulations are returned.
#' @param object Arima object
#' @param nsim positive integer. Length of time series to be generated.
#' @param innov vector of innovations.
#' @param ... not used.
#' @export
sim_arima <-
    function(object, ...) {
        UseMethod("sim_arima")
    }

#' @rdname sim_arima
#' @export
sim_arima._Arima_fit_impl <-
    function(object, nsim, innov, ...) {
        object <- object$fit$models$model_1
        sim_arima(object, nsim, innov, ...)
    }

#' @rdname sim_arima
#' @export
sim_arima.Arima <-
    function(object, nsim, innov, ...) {
        model <- object$model
        len_delta <- length(model$Delta)
        len_phi <- length(model$phi)
        if (!is.numeric(innov)) rlang::abort("`innov` is not numeric")
        if (vctrs::vec_size(innov) <= nsim) rlang::abort(
            "Length of `innov` should be more than `nsim`."
        )
        ret <- innov
        # apply Theta
        if (length(model$theta)) {
            ret <-
                stats::filter(ret, c(1, model$theta), sides = 1L) |>
                stats::na.omit() |>
                as.numeric()
        }
        # apply Phi
        if (len_phi) {
            ret <-
                stats::filter(ret, model$phi, method = "recursive") |>
                as.numeric()
        }
        # apply Delta
        if (len_delta) {
            ret <-
                stats::filter(ret, model$Delta, method = "recursive") |>
                as.numeric()
        }
        ret |>
            utils::tail(nsim)
    }