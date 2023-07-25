#' Simulate from registered models
#'
#' @param object model from which to simulate. Currently supports objects of
#'      class `arima` and `garch`.
#' @param ... not used.
#' @export
simts <-
    function(object, ...) {
        UseMethod("simts")
    }

simts.default <-
    function(object, ...) {
        object <- smpspltools::extract_model(object)
        if (!inherits(object, "arima") && !inherits(object, "garch")) {
            rlang::abort(
                glue::glue("Object of class {class(object)} is not registered.")
            )
        }
        simts(object, ...)
    }