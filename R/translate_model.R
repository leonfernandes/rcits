#' Extracts available models
#'
#' Generic function to extract and return objects that have been registered with
#' `rcits` package. Currently only `arima` and `garch` objects are registered.
#' This function translates functions of class `Arima`, `Arima_fit_impl`, `AR`,
#' `ARIMA` and `fGARCH` to the corresponding registered methods. This function
#' also extracts the fitted models from `model_fit`, `workflow` and `mdl_ts`
#' objects.
#' @param object An object.
#' @param ... unused.
#' @export
translate_model <- function(object, ...) UseMethod("translate_model")

#' @rdname translate_model
#' @export
translate_model.default <-
    function(object, ...) {
        rlang::abort(glue::glue("Model class {class(object)} is unregistered."))
    }

## arima ----
#' @rdname translate_model
#' @export
translate_model.arima <- function(object, ...) object

## Arima ----
#' @rdname translate_model
#' @export
translate_model.Arima <-
    function(object, ...) {
        ret <- object$model
        class(ret) <- "arima"
        ret
    }

## tidymodels ----
#' @rdname translate_model
#' @export
translate_model.model_fit <-
    function(object, ...) translate_model(hardhat::extract_fit_engine(object))

#' @rdname translate_model
#' @export
translate_model.workflow <-
    function(object, ...) translate_model(hardhat::extract_fit_engine(object))

## modeltime ----
#' @rdname translate_model
#' @export
translate_model.Arima_fit_impl <-
    # manually use .Arima method
    function(object, ...) translate_model.Arima(object$models$model_1, ...)

## fable ----
#' @rdname translate_model
#' @export
translate_model.mdl_ts <- function(object, ...) translate_model(object$fit, ...)

#' @rdname translate_model
#' @export
translate_model.ARIMA <- function(object, ...) translate_model(object$model, ...)

#' @rdname translate_model
#' @export
translate_model.AR <-
    function(object, ...) rcits::make_arima(phi = object$coef, theta = 0, delta = 0)

## garch ----
#' @rdname translate_model
#' @export
translate_model.garch <- function(object, ...) object

#' @rdname translate_model
#' @export
translate_model.fGARCH <- function(object, ...) {
    par <- fGarch::coef(object)
    par_list <-
        c(omega = "omega", alpha = "alpha", beta = "beta") |>
        lapply(function(.) subset_from_name(par, .))
    do.call(rcits::make_garch, par_list)
}

subset_from_name <-
    function(object, str_to_detect) {
        idx <-
            object |>
            names() |>
            stringr::str_detect(str_to_detect) |>
            which()
        ret <- object[idx]
        if (!length(ret))
            ret <- 0
        ret
    }