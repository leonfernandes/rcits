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

#' Custom simulation of univariate GARCH model
#'
#' Simulates a univariate time series according to \eqn{X_t  = \sigma_t Z_t},
#' where \eqn{\sigma_t} follows the recusrion
#' \deqn{\sigma_{t|t-1} = \sum_{j=1}^p \beta_j \sigma_{t-j|t-j-1}+ \alpha_0+\sum_{j=1}^q \alpha_j X_{t-i}^2.}
#' It is assumed that sufficient terms for burn-in period are provided in
#' `innov`, `init_garch` and `init_sigma2`. The number of terms used for burn-in
#' is \eqn{m}, where \eqn{m} is the minimum lengths oamong `init_garch` and
#' `init_sigma2`. In this case `innov` is assumed to have at least `nsim` +
#' \eqn{m}. Note that only the minimal required number of terms, counted from the
#' tail are used for the simulation.
#' The implementation is based on `fGarch::garchSim`.
#' @param object A `garch` object.
#' @param nsim positive integer. Length of time series to be generated.
#' @param innov vector of innovations.
#' @param init_garch numeric vector of starting values for garch process
#'      used only for burn-in period.
#' @param init_sigma2 numeric vector of starting values for variance process
#'      used only for burn-in period.
#' @inheritParams simults
#' @return A [tsibble][tsibble::tsibble-package] of class "garch_ts". Column
#'      "value" corresponds to the simulated time series, "sigma2" and column 
#'      "date" has dummy dates for each observation.
#' @export
#' @examples
#' # simulate from a GARCH(1, 1) model
#' mdl <- make_garch(omega = 0.5, alpha = 0.1, beta = 0.8)
#' simults(
#'      mdl,
#'      nsim = 100,
#'      innov = rnorm(200),
#'      init_garch = c(0, 0),
#'      init_sigma2 = c(1, 1)
#' )
simults.garch <- function(object, nsim, innov, init_garch, init_sigma2, ...) {
    omega <- object$omega
    alpha <- object$alpha
    beta <- object$beta
    # Set defaults for GARCH
    delta <- 2
    # mu <- ar <- ma <- 0

    len_alpha <- vctrs::vec_size(alpha)
    len_beta <- vctrs::vec_size(beta)
    len_innov <- vctrs::vec_size(innov)
    # len_ar <- vctrs::vec_size(ar)
    # len_ma <- vctrs::vec_size(ma)
    m <- min(vctrs::vec_size(init_garch), vctrs::vec_size(init_sigma2)) # length of burn-in
    if (len_innov <= nsim + m) {
        rlang::abort(
            "Length of `innov` insufficient for burn-in and simulation."
        )
    }
    if (m <= max(len_alpha, len_beta)) {
        rlang::abort(
            "Number of terms for burn-in is not larger than `alpha` or
                `beta`."
        )
    }
    # allocate memory for variance and garch
    y <- c(
        utils::tail(init_garch, m),
        rep(NA, times = nsim)
    )
    h <- c(
        utils::tail(init_sigma2, m),
        rep(NA, times = nsim)
    )
    z <- utils::tail(innov, m + nsim)
    deltainv <- 1 / delta
    # Iterate GARCH / APARCH Model:
    eps <- h^deltainv * z # calculate burn-in values
    for (i in (m + 1):(m + nsim)) {
        h[i] <- omega +
            sum(
                alpha * (
                    abs(eps[i - (1:len_alpha)])
                    # -gamma * (eps[i - (1:len_alpha)])
                )^delta
            ) +
            sum(beta * h[i - (1:len_beta)])
        eps[i] <- h[i]^deltainv * z[i] # update garch
        y[i] <-
            # mu +
            # sum(ar * y[i - (1:len_ar)]) +
            # sum(ma * eps[i - (1:len_ma)]) +
            eps[i]
    }
    # attr(ret, "eps") <- eps
    tsibble::tsibble(
        date = Sys.Date() + 1:nsim,
        value = utils::tail(y, nsim),
        # sigma2 = utils::tail(h, nsim),
        index = date
    ) |>
        tsibble::new_tsibble("garch_ts")
}


#' Fitted residuals for GARCH
#'
#' @inheritParams fitted_resid
#' @param new_data a [tsibble][tsibble::tsibble-package] of univariate
#'      time series on which residuals are to be calculated.
#' @param ... not used
#' @return a [tsibble][tsibble::tsibble-package] of fitted residuals of class
#'      `fitted_resid_ts`.
#' @export
fitted_resid.garch <- function(object, new_data, ...) {
    if (!inherits(object, "garch")) rlang::abort("Not garch object.")
    omega <- object$omega
    alpha <- object$alpha
    beta <- object$beta
    delta <- 2
    deltainv <- 1 / delta

    new_data_response_var <- tsibble::measured_vars(new_data)
    # Extract outcome column
    y <- new_data[[new_data_response_var]]
    ydelta <- abs(y)^delta
    len_y <- vctrs::vec_size(y)
    vec_c <- ccoef(omega, alpha, beta, len_y)
    h <- vec_c[1] +
        sapply(
            2:len_y, \(i) sum(vec_c[2:i] * ydelta[i:2 - 1])
        )
    h <- c(vec_c[1], h)
    z <- y / (h^deltainv)
    new_data |>
        dplyr::mutate(".resid" = z) |>
        dplyr::select(-!!new_data_response_var) |>
        tsibble::new_tsibble(class = "fitted_resid_ts")
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