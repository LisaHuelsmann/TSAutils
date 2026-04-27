
#' Plot original and deseasonalized time series
#'
#' Visualizes a time series before and after removal of its seasonal cycle.
#' The function produces four plots: the original data with a smooth trend,
#' the deseasonalized data with a smooth trend, and histograms (with density
#' and normal curve overlays) for both series.
#'
#' @param t Numeric vector of time values (e.g., decimal years).
#' @param x Numeric vector of observations.
#' @param d Integer. Number of seasonal bins per year (e.g., 365 for daily data).
#' @param n Integer. Number of bins for the histograms. Defaults to 40.
#'
#' @return No return value. The function is called for its side effect of
#'   producing plots.
#'
#' @details
#' The seasonal cycle is removed using [deseason()], which groups data into
#' `d` seasonal bins and subtracts the corresponding mean seasonal component.
#' A LOESS smoother is applied to both the original and deseasonalized series
#' to highlight trends.
#'
#' The resulting figure layout contains:
#' \itemize{
#'   \item Original time series with LOESS smooth
#'   \item Deseasonalized time series with LOESS smooth
#'   \item Histogram of original data with density and normal curve
#'   \item Histogram of deseasonalized data with density and normal curve
#' }
#'
#' @examples
#' x <- rnorm(1000)
#' t <- seq(2000, 2002, length.out = 1000)
#' plot.deseason(t, x, d = 365)
#'
#' @export
plot.deseason = function(t, x, d, n = 40) {
  p = d
  dummy = deseason(t, x, p)
  dummy.loess = loess(dummy ~ t, span = 0.3)
  x.loess = loess(x ~ t, span = 0.3)
  op = par(mfrow = c(2, 2))
  plot(
    t,
    x,
    type = 'l',
    ylab = 'Observations',
    xlab = 'Time',
    main = 'Original data'
  )
  lines(t,
        x.loess$fitted,
        type = 'l',
        col = 'red',
        lwd = 2)
  legend(
    'bottomleft',
    legend = c('Data', 'Smooth curve'),
    lty = 1,
    col = c('black', 'red')
  )
  plot(
    t,
    dummy,
    type = 'l',
    ylab = 'Observations',
    xlab = 'Time',
    main = 'Deseasonalised data'
  )
  lines(
    t,
    dummy.loess$fitted,
    type = 'l',
    col = 'red',
    lwd = 2
  )
  legend(
    'bottomleft',
    legend = c('Data', 'Smooth curve'),
    lty = 1,
    col = c('black', 'red')
  )
  hist.normal(x, n)
  hist.normal(dummy, n)
  par(op)
}





