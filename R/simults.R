#' Simulate from registered models
#'
#' @param object model from which to simulate. Currently supports objects of
#'      class `arima` and `garch`.
#' @param ... not used.
#' @export
simults <- function(object, ...) UseMethod("simults")

#' Fitted residuals from registered models
#'
#' @param object a registered model. Currently supports objects of class `arima`
#'      and `garch`.
#' @param ... not used
#' @export
fitted_resid <- function(object, ...) UseMethod("fitted_resid")