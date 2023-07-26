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
        object <- smpspltools::extract_model(object)
        if (!smpspltools::is_smpspl_reg(object)) {
            rlang::abort(
                glue::glue("Object of class {class(object)} is not registered.")
            )
        }
        simults(object, ...)
    }