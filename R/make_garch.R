#' Constructor for garch object
#'
#' @param omega numeric constant coefficient of the variance equation.
#' @param alpha numeric vector of autoregressive coefficients.
#' @param beta numeric vector of variance coefficients.
#' @export
make_garch <-
    function(omega, alpha, beta) {
        ret <- list(omega = omega, alpha = alpha, beta = beta)
        class(ret) <- "garch"
        ret
    }