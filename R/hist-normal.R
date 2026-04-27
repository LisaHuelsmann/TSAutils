
#' Histogram with density and normal curve
#'
#' Plots a histogram of a numeric vector with an overlaid kernel density
#' estimate and a fitted normal distribution curve.
#'
#' @param x Numeric vector of observations.
#' @param n Integer. Number of bins used for the histogram. Defaults to 40.
#'
#' @return No return value. The function is called for its side effect of
#'   producing a plot.
#'
#' @details
#' The function creates a histogram with `n` bins spanning the range of the
#' data. It overlays:
#' \itemize{
#'   \item a kernel density estimate (red line), scaled to match histogram counts
#'   \item a normal distribution curve (blue line) based on the sample mean and variance
#' }
#'
#' The normal curve is computed using an internal helper function.
#'
#' @examples
#' x <- rnorm(1000)
#' hist.normal(x)
#'
#' @export
hist.normal <- function(x, n = 40) {
  low <- floor(min(x))
  high <- ceiling(max(x))
  rng <- 0:n * (high - low) / n + low
  rng2 <- (2 * low):(2 * high) / 2
  a = hist(x, breaks = rng, plot = F)
  hist(
    x,
    breaks = rng,
    main = paste("Histogram,", n, "bins"),
    xlab = "Observations",
    ylim = c(0, 1.1 * max(a$counts))
  )
  dens <- density(x, n = 2 * n)
  lines(dens$x,
        dens$y * length(x) * (high - low) / n,
        col = "red",
        lw = 2)
  lines(rng2,
        sapply(rng2, histnorm(x, (high - low) / n)),
        col = "blue",
        lw = 2)
  legend(
    "topright",
    legend = c("Frequency", "Density", "Normal"),
    lty = 1,
    lwd = 2,
    col = c("black", "red", "blue")
  )
}


#' Normal curve scaled to histogram counts (internal)
#'
#' Computes a normal (Gaussian) curve based on the mean and variance of a
#' numeric vector and returns it as a function. The resulting function can be
#' used to overlay a normal curve on a histogram.
#'
#' @param x Numeric vector of observations.
#' @param scale Numeric scaling factor. Defaults to 1. Can be adjusted to better
#'   match histogram scaling.
#'
#' @return A function of a single numeric argument `t` that evaluates the scaled
#'   normal density based on `x`.
#'
#' @details
#' The returned function represents a normal distribution with mean
#' `mean(x)` and variance `var(x)`, scaled by the sample size and the
#' `scale` parameter. It is primarily intended for visual comparison with
#' histograms.
#'
#' @examples
#' x <- rnorm(100)
#' h <- hist(x)
#' f <- histnorm(x)
#' curve(f, add = TRUE, col = "red")
#'
#' @keywords internal
histnorm <- function(x, scale = 1) {
  m <- mean(x)
  v <- var(x)
  n <- length(x)
  function(t) { n * scale / sqrt(2 * pi * v) * exp(-((t - m)^2 / (2 * v))) }
}

