

#' Linearly detrends a time series
#'
#' Fits a linear regression and returns the residuals
#'
#' @param x A numerical vector
#' @return Residuals, a vector of the same length as x
#' @export
detrend <- function(x) {
  t <- 1:length(x)
  lm(x ~ t)$residuals # + mean(x)
}

#' Plot a time series and its linear trend
#'
#' Fits a linear regression and visualizes the time series and the linear trend
#'
#' @param t A vector of time steps plotted on x axis
#' @param x A numerical vector plotted on y axis
#' @return No return value, called for side effects (a plot).
#' @export
trendplot <- function(t, x) {
  x.lab = ''
  y.lab = ''
  regr <- lm(x ~ t)
  plot(t,
       x,
       type = "l",
       xlab = x.lab,
       ylab = y.lab)
  abline(regr$coefficients, col = "red")
  legend(
    'topleft',
    legend = 'Best linear fit',
    cex = 0.7,
    col = 'red',
    lty = 1
  )
}
