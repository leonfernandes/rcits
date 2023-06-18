#' Simulate from registered models
#'
#' @param object model from which to simulate. Currently supports objects of
#'      class `arima` or fitted using `stats::arima` and `modeltime::arima_reg`.
#' @param ... not used.
#' @export
simts <-
    function(object, ...) {
        UseMethod("simts")
    }