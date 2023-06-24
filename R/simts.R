#' Simulate from registered models
#'
#' @param object model from which to simulate. Currently supports objects of
#'      class `arima` and `garch`. Also allowed are fitted models from
#'      `stats::arima` and `modeltime::arima_reg` for `arima` class.
#' @param ... not used.
#' @export
simts <-
    function(object, ...) {
        UseMethod("simts")
    }