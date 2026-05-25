
#' Create Gaussian white noise
#' @export
purely_random <- function(n) rnorm(n)

#' Add trend to a time series
#' @export
add_trend <- function(x, strength=1)
  x + seq(from=0, to=strength*sd(x), length=length(x))

#' Add periodicity to a time series
#' @export
add_periodicity <- function(x, period=floor(length(x)/2))
  x + sapply(1:length(x), function(t) sin(2 * pi * t / period))

#' Add jumps to a time series
#' @export
add_jumps <- function(x, strength=1, rate=2/3*log(length(x))) {
  aha <- cumsum(floor(rexp(length(x), rate=rate))) + 1
  x + strength * (runif(max(aha)))[aha]
}

