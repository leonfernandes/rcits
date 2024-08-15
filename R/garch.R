#' Constructor for garch object
#'
#' @param omega numeric constant coefficient of the variance equation.
#' @param alpha numeric vector of autoregressive coefficients.
#' @param beta numeric vector of variance coefficients.
#' @export
make_garch <- function(omega, alpha, beta) {
    if (!length(alpha)) {
        alpha <- 0
    }
    if (!length(beta)) {
        beta <- 0
    }
    ret <- list(omega = omega, alpha = alpha, beta = beta)
    class(ret) <- "garch"
    ret
}

#' Custom simulation of univariate GARCH(1,1) model
#'
#' Simulates a univariate time series according to \eqn{X_t  = \sigma_t Z_t},
#' where \eqn{\sigma_t} follows the recursion
#' \deqn{\sigma_{t|t-1} = \sum_{j=1}^p \beta_j \sigma_{t-j|t-j-1}+ \alpha_0+\sum_{j=1}^q \alpha_j X_{t-i}^2.}
#'
#' We fix \eqn{p=q=1} for the GARCH model. The implementation is based on `fGarch::garchSim`.
#' @param object A `garch` object.
#' @inheritParams inn2ts
#' @return A [tsibble][tsibble::tsibble-package] of class "garch_ts". Column
#'      "ts" corresponds to the simulated time series and column 
#'      "date" has dummy dates for each observation.
#' @export
#' @examples
#' # simulate from a GARCH(1, 1) model
#' mdl <- make_garch(omega = 0.5, alpha = 0.1, beta = 0.8)
#' inn2ts(
#'      mdl,
#'      inn = rnorm(200)
#' )
inn2ts.garch <- function(object, inn, ...) {
    omega <- object$omega
    alpha <- object$alpha
    beta <- object$beta
    # Set defaults for GARCH
    delta <- 2
    # mu <- ar <- ma <- 0

    len_alpha <- vctrs::vec_size(alpha)
    len_beta <- vctrs::vec_size(beta)
    len_inn <- vctrs::vec_size(inn)
    # len_ar <- vctrs::vec_size(ar)
    # len_ma <- vctrs::vec_size(ma)
    # m <- min(vctrs::vec_size(init_garch), vctrs::vec_size(init_sigma2)) # length of burn-in
    if (len_inn <=  1) {
        rlang::abort(
            "Length of `inn` insufficient for recursion"
        )
    }
    # initialize h and ts
    h <- rep(NA, times=len_inn)
    ts <- rep(NA, times=len_inn)
    eps <- rep(NA, times=len_inn)
    deltainv <- 1 / delta
    # Iterate GARCH / APARCH Model:
    h[1] <- omega/(1-alpha-beta)
    ts[1] <- omega/(1-alpha)
    eps[1] <- h[1]^deltainv * inn[1]
    for (i in 2:len_inn) {
        h[i] <- omega +
            sum(
                alpha * (
                    abs(eps[i - (1:len_alpha)])
                    # -gamma * (eps[i - (1:len_alpha)])
                )^delta
            ) +
            sum(beta * h[i - (1:len_beta)])
        eps[i] <- h[i]^deltainv * inn[i] # update garch
        ts[i] <-
            # mu +
            # sum(ar * ts[i - (1:len_ar)]) +
            # sum(ma * eps[i - (1:len_ma)]) +
            eps[i]
    }
    tsibble::tsibble(
        date = Sys.Date() + 1:len_inn,
        ts = ts,
        index = date
    ) |>
        tsibble::new_tsibble("garch_ts")
}


#' Fitted residuals for GARCH(1,1)
#'
#' @inheritParams ts2inn
#' @param ts a [tsibble][tsibble::tsibble-package] of univariate time series
#'      observations.
#' @param ... not used
#' @return a [tsibble][tsibble::tsibble-package] where column `inn`
#'      contains the calculated innovations.
#' @export
ts2inn.garch <- function(object, ts, ...) {
    if (!inherits(object, "garch")) rlang::abort("Not garch object.")
    omega <- object$omega
    alpha <- object$alpha
    beta <- object$beta
    delta <- 2
    deltainv <- 1 / delta

    ts_response_var <- tsibble::measured_vars(ts)
    # Extract outcome column
    y <- ts[[ts_response_var]]
    ydelta <- abs(y)^delta
    len_y <- vctrs::vec_size(y)
    vec_c <- ccoef(omega, alpha, beta, len_y)
    h <- vec_c[1] +
        sapply(
            2:len_y, \(i) sum(vec_c[2:i] * ydelta[i:2 - 1])
        )
    h <- c(vec_c[1], h)
    z <- y / (h^deltainv)
    ts |>
        dplyr::mutate(".resid" = z) |>
        dplyr::select(-!!ts_response_var)
}

#' Calculate Recursion Polynomial
#'
#' Refers to recursion in equation (6.1) in Davis and Wan (2020).
#' @inheritParams make_garch
#' @param order a positive integer. The order of the polynomial to be returned.
#' @returns numeric vector consisting of coefficients of the conditional
#'       variance recursion polynomial.
#' @noRd
ccoef <- function(omega, alpha, beta, order) {
    ret <- numeric(order)
    n_alpha <- vctrs::vec_size(alpha)
    if (order > n_alpha) {
        alpha <- c(alpha, numeric(order - n_alpha))
    }
    n_beta <- vctrs::vec_size(beta)
    if (order > n_beta + 1) {
        beta <- c(beta, numeric(order - n_beta - 1))
    }
    for (j in seq_along(ret)) {
        ret[j] <-
            alpha[j] +
            sum(
                vctrs::vec_slice(beta, 1:(j - 1)) *
                    vctrs::vec_slice(ret, (j - 1):1)
            )
    }
    ret <- c(omega / (1 - sum(beta)), ret)
    return(ret)
}