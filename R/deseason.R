

#' Remove the seasonal cycle from a time series
#'
#' Removes the mean seasonal cycle from a numeric time series by grouping
#' observations into equally spaced bins within the year.
#'
#' @param t Numeric vector of time values expressed as decimal years, for
#'   example `2001.5`. The fractional part is used to determine the position
#'   within the year.
#' @param x Numeric vector of observations to be deseasonalized.
#' @param d Integer. Number of seasonal bins per year. Defaults to `365`, which
#'   is suitable for daily data.
#'
#' @return A numeric vector of the same length as `x`, with the estimated
#'   seasonal cycle removed and the overall mean added back.
#'
#' @details
#' For each observation, the function extracts the fractional part of `t` and
#' assigns the observation to one of `d` equally spaced seasonal bins. The mean
#' of each bin is used as the estimated seasonal component. The deseasonalized
#' value is calculated as:
#'
#' `x - seasonal_mean + overall_mean`
#'
#' This preserves the overall mean of the original series while removing the
#' average seasonal pattern.
#'
#' @examples
#' x_deseason <- deseason(meteodata$dec_year, meteodata$Tmean)
#' plot(meteodata$Date, meteodata$Tmean, type = "l")
#' lines(meteodata$Date, x_deseason, col = "red")
#'
#' @export
deseason <- function(t, x, d = 365) {
  p <- d
  frac <- 0:p / p
  bin <- rep(0, p)
  n <- length(x)
  diff <- rep(0, n)

  for (i in 1:p) {
    season <- t - floor(t)
    index <- which(frac[i] <= season & season < frac[i + 1])
    bin[i] <- mean(x[index], na.rm = TRUE)
    diff[index] <- bin[i]
  }

  x - diff + mean(x, na.rm = TRUE)
}


