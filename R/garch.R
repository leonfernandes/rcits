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
#' @param nsim positive integer. Number of simulations.
#' @param innov numeric vector of innovations.
#' @param init_garch numeric vector of starting values for garch process
#'      used only for burn-in period.
#' @param init_sigma2 numeric vector of starting values for variance process
#'      used only for burn-in period.
#' @param ... Not used.
#' @return A [tsibble][tsibble::tsibble-package] of class "garch_ts". Column
#'      "value" corresponds to the simulated time series, "sigma2" contains the
#'      simulated variance time series and column "date" has dummy dates per
#'      observation.
#' @rdname simts_garch
#' @export
#' @examples
#' # simulate from a GARCH(1, 1) model
#' mdl <- make_garch(omega = 0.5, alpha = 0.1, beta = 0.8)
#' simts(
#'      mdl,
#'      nsim = 100,
#'      innov = rnorm(200),
#'      init_garch = numeric(2),
#'      init_sigma = c(1, 1)
#' )
simts.garch <-
    function(object, nsim, innov, init_garch, init_sigma2, ...) {
        simts_garch_impl(
            omega = object$omega,
            alpha = object$alpha,
            beta = object$beta,
            nsim = nsim,
            innov = innov,
            init_garch = init_garch,
            init_sigma2 = init_sigma2
        )
    }

simts_garch_impl <-
    function(omega, alpha, beta, nsim, innov, init_garch, init_sigma2, ...) {
        # Set defaults for GARCH
        delta <- 2
        # mu <- ar <- ma <- 0

        len_alpha <- vctrs::vec_size(alpha)
        len_beta <- vctrs::vec_size(beta)
        len_innov <- vctrs::vec_size(innov)
        # len_ar <- vctrs::vec_size(ar)
        # len_ma <- vctrs::vec_size(ma)
        m <-
            # length of burn-in
            min(vctrs::vec_size(init_garch), vctrs::vec_size(init_sigma2))
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
        y <-
            c(
                utils::tail(init_garch, m),
                rep(NA, times = nsim)
            )
        h <-
            c(
                utils::tail(init_sigma2, m),
                rep(NA, times = nsim)
            )
        z <- utils::tail(innov, m + nsim)
        deltainv <- 1 / delta
        # Iterate GARCH / APARCH Model:
        eps <- h^deltainv * z # calculate burn-in values
        for (i in (m + 1):(m + nsim)) {
            h[i] <-
                omega +
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
        ret <-
            tsibble::tsibble(
                date = Sys.Date() + 1:nsim,
                value = utils::tail(y, nsim),
                sigma2 = utils::tail(h, nsim),
                index = date
            ) |>
            tsibble::new_tsibble("garch_ts")
        ret
    }