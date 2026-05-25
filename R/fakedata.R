

#' Add a linear trend to a time series
#'
#' Adds a deterministic linear trend to a univariate time series. The trend
#' increases linearly from 0 to a maximum value proportional to the standard
#' deviation of the input series.
#'
#' @param x Numeric vector. Input time series.
#' @param strength Numeric. Scaling factor controlling the strength of the
#'   trend relative to the standard deviation of `x`.
#'
#' @return Numeric vector. Time series with an added linear trend.
#'
#' @details
#' The added trend is defined as a linear sequence ranging from
#' \eqn{0} to \eqn{strength \times sd(x)}.
#'
#' Larger values of `strength` produce more pronounced trends.
#'
#' @examples
#' x <- rnorm(200)
#' y <- add_trend(x, strength = 2)
#' plot(y, type = "l")
#'
#' @export
add_trend <- function(x, strength = 1) {
  x + seq(
    from = 0,
    to = strength * sd(x),
    length.out = length(x)
  )
}


#' Add periodicity to a time series
#'
#' Adds a sinusoidal periodic component to a univariate time series.
#'
#' @param x Numeric vector. Input time series.
#' @param period Integer. Period length of the sinusoidal component.
#'
#' @return Numeric vector. Time series with an added periodic component.
#'
#' @details
#' The periodic component is generated using a sine function:
#'
#' \deqn{
#' \sin\left( \frac{2\pi t}{period} \right)
#' }
#'
#' where \eqn{t} denotes the time index.
#'
#' Smaller values of `period` produce more rapidly oscillating behaviour.
#'
#' @examples
#' x <- rnorm(200)
#' y <- add_periodicity(x, period = 20)
#' plot(y, type = "l")
#'
#' @export
add_periodicity <- function(x, period = floor(length(x) / 2)) {
  x + sapply(
    1:length(x),
    function(t) sin(2 * pi * t / period)
  )
}


#' Add random jumps to a time series
#'
#' Adds random level shifts ("jumps") to a univariate time series.
#'
#' @param x Numeric vector. Input time series.
#' @param strength Numeric. Scaling factor controlling the magnitude of jumps.
#' @param rate Numeric. Rate parameter controlling the frequency of jumps.
#'
#' @return Numeric vector. Time series with added random jumps.
#'
#' @details
#' Jump locations are generated from exponentially distributed waiting times.
#' Between jump points, the process remains at a constant random level.
#'
#' Larger values of `strength` increase the magnitude of jumps, while larger
#' values of `rate` increase their frequency.
#'
#' @examples
#' x <- rnorm(200)
#' y <- add_jumps(x, strength = 2)
#' plot(y, type = "l")
#'
#' @export
add_jumps <- function(x,
                      strength = 1,
                      rate = 2 / 3 * log(length(x))) {
  aha <- cumsum(floor(rexp(length(x), rate = rate))) + 1

  x + strength * (runif(max(aha)))[aha]
}
