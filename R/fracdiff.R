
#' Generalized binomial coefficient
#'
#' Computes the generalized binomial coefficient used in the expansion of the
#' fractional differencing operator.
#'
#' @param r Numeric. Upper argument of the binomial coefficient.
#' @param k Non-negative integer. Lower argument of the binomial coefficient.
#'
#' @return Numeric value.
#'
#' @keywords internal
binomial_coefficient <- function(r, k) {
  b <- 1

  if (k > 0) {
    for (j in 1:k) {
      b <- b * r / j
      r <- r - 1
    }
  }

  b
}




#' Backshift a time series
#'
#' Shifts a vector by `k` time steps and inserts zeros at the beginning.
#'
#' @param x Numeric vector. Input time series.
#' @param k Non-negative integer. Number of time steps by which to shift.
#'
#' @return Numeric vector of the same length as `x`.
#'
#' @keywords internal
backshift <- function(x, k) {
  n <- length(x)

  if (k < n) {
    c(rep(0, k), x[1:(n - k)])
  } else {
    rep(0, n)
  }
}




#' Fractionally difference a time series
#'
#' Applies the fractional differencing operator \eqn{(1 - B)^d} to a
#' univariate time series.
#'
#' @param x Numeric vector or time series. Input time series.
#' @param d Numeric. Differencing order. Integer values reproduce ordinary
#'   differencing; fractional values allow fractional differencing.
#' @param max_lag Non-negative integer. Maximum lag used in the finite
#'   approximation of the infinite expansion.
#'
#' @return A time series object containing the fractionally differenced values.
#'
#' @details
#' Fractional differencing is defined by
#' \deqn{
#' (1 - B)^d x_t =
#' \sum_{k = 0}^{\infty} (-1)^k \binom{d}{k} x_{t-k}.
#' }
#'
#' This function approximates the infinite sum by truncating it after
#' `max_lag` lags. The first `max_lag` observations are removed because they
#' are affected by zero-padding in the backshift operation.
#'
#' Positive values of `d` correspond to differencing. Negative values correspond
#' to fractional summation.
#'
#' @examples
#' x <- rnorm(200)
#' y <- fractional_difference(x, d = 0.4)
#' plot(y)
#'
#' @export
fractional_difference <- function(x, d, max_lag = 30) {
  n <- length(x)
  y <- rep(0, n)

  for (k in 0:max_lag) {
    y <- y +
      binomial_coefficient(d, k) *
      (-1)^k *
      backshift(x, k)
  }

  y <- y[-(1:max_lag)]

  as.ts(y)
}




#' Plot fractional differencing coefficients
#'
#' Plots the coefficients of the fractional differencing operator
#' \eqn{(1 - B)^d}.
#'
#' @param d Numeric. Differencing order.
#' @param max_lag Non-negative integer. Maximum lag shown in the plot.
#'
#' @return No return value. Called for its side effect of producing a plot.
#'
#' @details
#' The plotted coefficients are
#' \deqn{
#' \beta_k = (-1)^k \binom{d}{k}.
#' }
#'
#' For example, `d = 0` gives the identity operator, `d = 1` gives the usual
#' first difference, and `d = 2` gives the usual second difference.
#'
#' Negative values of `d` correspond to inverse differencing or fractional
#' summation.
#'
#' @examples
#' plot_fractional_difference_coefficients(d = 0)
#' plot_fractional_difference_coefficients(d = 0.5)
#' plot_fractional_difference_coefficients(d = -0.5)
#'
#' @export
plot_fractional_difference_coefficients <- function(d, max_lag = 30) {
  ks <- 0:max_lag

  bs <- sapply(ks, function(k) {
    (-1)^k * binomial_coefficient(d, k)
  })

  plot(
    ks,
    bs,
    type = "h",
    lwd = 6,
    ylim = c(min(0, min(bs)), max(1, max(bs))),
    xlab = "Lag k",
    ylab = expression(beta[k]),
    main = paste("Coefficients of (1 - B)^", d, sep = "")
  )
}




#' Plot a function for nine parameter values
#'
#' Evaluates a plotting function for nine equally spaced parameter values.
#'
#' @param f Function. A plotting function that takes one numeric argument.
#' @param min Numeric. Lower bound of the parameter range.
#' @param max Numeric. Upper bound of the parameter range.
#'
#' @return No return value. Called for its side effect of producing a 3 by 3
#'   panel of plots.
#'
#' @details
#' This function is useful for comparing how a plot changes as a parameter
#' varies. In this exercise, it is used to compare different values of the
#' fractional differencing parameter `d`.
#'
#' @examples
#' plot_parameter_grid(plot_fractional_difference_coefficients, min = 0, max = 1)
#'
#' @export
plot_parameter_grid <- function(f, min = 0, max = 1) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(3, 3))

  for (d in seq(min, max, length.out = 9)) {
    f(d)
  }
}
