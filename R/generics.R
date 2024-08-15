#' Simulate observations
#'
#' This generic is used to obtain observations from a time series model given innovations.
#' @param object model from which to simulate. Currently supports objects of
#'      class `arima` and `garch`.
#' @param inn vector of innovations.
#' @param ... not used.
#' @export
inn2ts <- function(object, inn, ...) UseMethod("inn2ts")

#' Fitted residuals from registered models
#'
#' This generic is used to obtain innovations from a time series model given observations.
#' @inheritParams inn2ts
#' @param ts a [tsibble][tsibble::tsibble-package] of univariate
#'      time series observation
#' @param ... not used
#' @export
ts2inn <- function(object, ts, ...) UseMethod("ts2inn")