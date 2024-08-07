#' Simulate from registered models
#'
#' @param object model from which to simulate. Currently supports objects of
#'      class `arima` and `garch`.
#' @param ... not used.
#' @export
simults <-
    function(object, ...) {
        UseMethod("simults")
    }

#' @rdname simults
#' @export
simults.default <-
    function(object, ...) {
        rlang::abort(
            glue::glue("Object of class {class(object)} is not registered.")
        )
    }