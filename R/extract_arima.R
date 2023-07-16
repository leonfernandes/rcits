#' Extract arima object
#'
#' Extract underlying `arima` object from fitted ARIMA model.
#' @param object Object of class "Arima" or "_Arima_fit_impl".
#' @param ... not used.
#' @return "arima" object.
#' @export
extract_arima <-
    function(object, ...) {
        UseMethod("extract_arima")
    }

#' @rdname extract_arima
#' @export
extract_arima._Arima_fit_impl <-
    function(object, ...) {
        object <- object$fit
        extract_arima(object, ...)
    }

#' @rdname extract_arima
#' @export
extract_arima.Arima_fit_impl <-
    function(object, ...) {
        object <- object$models$model_1
        extract_arima(object, ...)
    }

#' @rdname extract_arima
#' @export
extract_arima.Arima <-
    function(object, ...) {
        ret <- object$model
        class(ret) <- "arima"
        ret
    }