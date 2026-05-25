# Internal helper functions -------------------------------------------------

#' Extract a slice from a vector
#'
#' Internal helper that extracts elements from position `first` to `last`.
#' If `last < first`, an empty numeric vector is returned.
#'
#' @param x A vector.
#' @param first Integer. First index to extract.
#' @param last Integer. Last index to extract.
#'
#' @return A vector slice.
#'
#' @keywords internal
.slice_vector <- function(x, first = 1, last = length(x)) {
  if (last < first) {
    numeric(0)
  } else {
    x[first:last]
  }
}


#' Z-standardize a numeric vector
#'
#' Internal helper that subtracts the sample mean and divides by the sample
#' standard deviation.
#'
#' @param x Numeric vector.
#'
#' @return Numeric vector with mean approximately 0 and standard deviation 1.
#'
#' @keywords internal
.z_standardize <- function(x) {
  s <- sd(x)

  if (is.na(s) || s == 0) {
    stop("Cannot z-standardize a constant or invalid series.")
  }

  (x - mean(x)) / s
}


#' Generate equally spaced values
#'
#' Internal helper that generates `n` equally spaced values between `min`
#' and `max`.
#'
#' @param n Integer. Number of values.
#' @param min Numeric. Lower bound.
#' @param max Numeric. Upper bound.
#'
#' @return Numeric vector of equally spaced values.
#'
#' @keywords internal
.seq_equal_space <- function(n, min = 0, max = 1) {
  if (n == 1) {
    (max + min) / 2
  } else {
    sapply(0:(n - 1), function(k) {
      k * (max - min) / (n - 1) + min
    })
  }
}


#' Generate logarithmically spaced values
#'
#' Internal helper that generates `n` logarithmically spaced values between
#' `min` and `max`.
#'
#' @param n Integer. Number of values.
#' @param min Numeric. Lower bound. Must be positive.
#' @param max Numeric. Upper bound. Must be positive.
#'
#' @return Numeric vector of logarithmically spaced values.
#'
#' @keywords internal
.seq_log_space <- function(n, min = 1, max = exp(1)) {
  if (min <= 0 || max <= 0) {
    stop("Arguments 'min' and 'max' must be positive.")
  }

  exp(.seq_equal_space(n, log(min), log(max)))
}


# Rescaled range method ----------------------------------------------------

#' Compute log-log data for rescaled range Hurst estimation
#'
#' Computes the data used for estimating the Hurst exponent by rescaled range
#' analysis. For several window sizes, the function computes the rescaled range
#' statistic and returns the corresponding values on a base-10 logarithmic scale.
#'
#' This function returns the regression data, not the Hurst estimate itself.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param windows Integer. Number of windows sampled for each window size.
#' @param sizes Integer. Number of different window sizes.
#' @param min.size Integer. Smallest window size considered.
#' @param logkmin Numeric. Minimum base-10 log window size included in the
#'   returned data.
#'
#' @return Numeric matrix with two columns:
#' \describe{
#'   \item{Column 1}{Base-10 logarithm of the window size.}
#'   \item{Column 2}{Base-10 logarithm of the rescaled range statistic.}
#' }
#'
#' @export
hurst_rs_data <- function(x, windows = 10, sizes = 50,
                          min.size = 10, logkmin = 0.5) {
  rs <- function(t, k) {
    win <- .slice_vector(x, t + 1, t + k)
    rng <- range(cumsum(.z_standardize(win)))
    rng[2] - rng[1]
  }

  n <- length(x)

  if (n < min.size) {
    stop("The time series is shorter than 'min.size'.")
  }

  ks <- round(.seq_log_space(sizes, min.size, floor(n / windows)))
  samp <- matrix(nrow = windows, ncol = sizes)

  for (j in 1:sizes) {
    k <- ks[j]
    ts <- round(.seq_equal_space(windows, 1, n - k + 1))

    for (i in 1:windows) {
      t <- ts[i]
      samp[i, j] <- rs(t - 1, k)
    }
  }

  result <- matrix(nrow = sizes * windows, ncol = 2)

  for (j in 1:sizes) {
    if (log(ks[j], base = 10) >= logkmin) {
      for (i in 1:windows) {
        result[(j - 1) * windows + i, ] <-
          log(c(ks[j], samp[i, j]), base = 10)
      }
    } else {
      result[(j - 1) * windows + (1:windows), ] <- NA
    }
  }

  colnames(result) <- c("log_window_size", "log_rescaled_range")
  result
}


#' Estimate the Hurst exponent using rescaled range analysis
#'
#' Estimates the Hurst exponent using rescaled range analysis. The Hurst
#' exponent is estimated as the slope in a log-log regression of the rescaled
#' range statistic on the window size.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param windows Integer. Number of windows sampled for each window size.
#' @param sizes Integer. Number of different window sizes.
#' @param min.size Integer. Smallest window size considered.
#' @param logkmin Numeric. Minimum base-10 log window size included in the
#'   regression.
#'
#' @return Numeric value. Estimated Hurst exponent.
#'
#' @export
estimate_hurst_rs <- function(x, windows = 10, sizes = 50,
                              min.size = 10, logkmin = 0.5) {
  rs_data <- hurst_rs_data(
    x = x,
    windows = windows,
    sizes = sizes,
    min.size = min.size,
    logkmin = logkmin
  )

  fit <- lm(rs_data[, 2] ~ rs_data[, 1])
  round(as.vector(fit$coefficients[2]), digits = 4)
}


#' Plot rescaled range Hurst estimation
#'
#' Creates a four-panel diagnostic plot for Hurst exponent estimation using
#' rescaled range analysis. The panels show:
#' \enumerate{
#'   \item the original time series,
#'   \item the autocorrelation function,
#'   \item the log-log regression used to estimate the Hurst exponent,
#'   \item residuals versus fitted values from the log-log regression.
#' }
#'
#' The additional reference lines in the log-log plot are theoretical comparison
#' lines and are not separate regression models.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param windows Integer. Number of windows sampled for each window size.
#' @param sizes Integer. Number of different window sizes.
#' @param min.size Integer. Smallest window size considered.
#' @param logkmin Numeric. Minimum base-10 log window size included in the
#'   regression.
#'
#' @return Invisibly returns the fitted linear model.
#'
#' @export
plot_hurst_rs <- function(x, windows = 10, sizes = 50,
                          min.size = 10, logkmin = 0.5) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(2, 2))

  plot(x, type = "l", lwd = 2, main = "Input Data", ylab = "Value")

  acf(x, lwd = 6, main = "Autocorrelation")

  rs_data <- hurst_rs_data(
    x = x,
    windows = windows,
    sizes = sizes,
    min.size = min.size,
    logkmin = logkmin
  )

  fit <- lm(rs_data[, 2] ~ rs_data[, 1])
  intercept <- fit$coefficients[1]
  hurst <- fit$coefficients[2]

  plot(
    rs_data,
    main = paste("R/S Estimation, H ~=", round(hurst, digits = 2)),
    xlab = "log(k)",
    ylab = "log(R/S)",
    ylim = range(c(intercept, na.omit(rs_data[, 2])))
  )

  abline(fit$coefficients, lwd = 2, col = "blue")
  abline(intercept, 1, lwd = 2, lty = 2, col = "red")
  abline(intercept, 0.5, lwd = 2, lty = 3, col = "green2")
  abline(h = intercept, lwd = 2, lty = 2, col = "magenta")

  legend(
    "topleft",
    lwd = 2,
    lty = c(1, 2, 3, 2),
    legend = c("estimated H", "H = 1", "H = 0.5", "H = 0"),
    col = c("blue", "red", "green2", "magenta"),
    cex = 0.8
  )

  plot(
    fit$fitted.values,
    fit$residuals,
    main = "Residuals vs Fitted",
    xlab = "Fitted values",
    ylab = "Residuals"
  )
  abline(h = 0, lwd = 2, col = "blue")

  invisible(fit)
}


# Aggregated variance method ----------------------------------------------

#' Compute log-log data for aggregated variance Hurst estimation
#'
#' Computes the data used for estimating the Hurst exponent by the aggregated
#' variance method. The time series is divided into blocks of different sizes,
#' block means are computed, and the variance of these block means is analysed
#' on a log-log scale.
#'
#' This function returns the regression data, not the Hurst estimate itself.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param sizes Integer. Number of different block sizes.
#' @param kmin Integer. Minimum number of blocks required for a block size to
#'   be included.
#'
#' @return Numeric matrix with two columns:
#' \describe{
#'   \item{Column 1}{Base-10 logarithm of the block size.}
#'   \item{Column 2}{Base-10 logarithm of the variance of block means.}
#' }
#'
#' @export
hurst_aggvar_data <- function(x, sizes = 50, kmin = 10) {
  n <- length(x)
  ms <- floor(.seq_log_space(sizes, 2, floor(n / 2)))
  result <- matrix(nrow = sizes, ncol = 2)

  for (j in 1:sizes) {
    m <- ms[j]
    windows <- floor(n / m)

    if (windows < kmin) {
      result[j, ] <- NA
    } else {
      samp <- sapply(1:windows, function(k) {
        mean(.slice_vector(x, (k - 1) * m + 1, k * m))
      })

      result[j, ] <- log(c(m, var(samp)), base = 10)
    }
  }

  colnames(result) <- c("log_block_size", "log_variance")
  result
}


#' Estimate the Hurst exponent using aggregated variances
#'
#' Estimates the Hurst exponent using the aggregated variance method. The slope
#' of the log-log regression is transformed into a Hurst exponent estimate.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param sizes Integer. Number of different block sizes.
#' @param kmin Integer. Minimum number of blocks required for a block size to
#'   be included.
#'
#' @return Numeric value. Estimated Hurst exponent.
#'
#' @export
estimate_hurst_aggvar <- function(x, sizes = 50, kmin = 10) {
  aggvar_data <- hurst_aggvar_data(
    x = x,
    sizes = sizes,
    kmin = kmin
  )

  fit <- lm(aggvar_data[, 2] ~ aggvar_data[, 1])
  round(as.vector((fit$coefficients[2] + 2) / 2), digits = 4)
}


#' Plot aggregated variance Hurst estimation
#'
#' Creates a four-panel diagnostic plot for Hurst exponent estimation using
#' the aggregated variance method. The panels show:
#' \enumerate{
#'   \item the original time series,
#'   \item the autocorrelation function,
#'   \item the log-log regression used to estimate the Hurst exponent,
#'   \item residuals versus fitted values from the log-log regression.
#' }
#'
#' The additional reference lines in the log-log plot are theoretical comparison
#' lines and are not separate regression models.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param sizes Integer. Number of different block sizes.
#' @param kmin Integer. Minimum number of blocks required for a block size to
#'   be included.
#'
#' @return Invisibly returns the fitted linear model.
#'
#' @export
plot_hurst_aggvar <- function(x, sizes = 50, kmin = 10) {
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfrow = c(2, 2))

  plot(x, type = "l", lwd = 2, main = "Input Data", ylab = "Value")

  acf(x, lwd = 6, main = "Autocorrelation")

  aggvar_data <- hurst_aggvar_data(
    x = x,
    sizes = sizes,
    kmin = kmin
  )

  fit <- lm(aggvar_data[, 2] ~ aggvar_data[, 1])
  intercept <- fit$coefficients[1]
  hurst <- (2 + fit$coefficients[2]) / 2

  plot(
    aggvar_data,
    main = paste("Aggregated Variance, H ~=", round(hurst, digits = 2)),
    xlab = "log(m)",
    ylab = expression(log(s[m]^2)),
    ylim = range(c(intercept, na.omit(aggvar_data[, 2])))
  )

  abline(fit$coefficients, lwd = 2, col = "blue")
  abline(intercept, 0, lwd = 2, lty = 2, col = "red")
  abline(intercept, -1, lwd = 2, lty = 3, col = "green2")
  abline(intercept, -2, lwd = 2, lty = 2, col = "magenta")

  legend(
    "bottomleft",
    lwd = 2,
    lty = c(1, 2, 3, 2),
    legend = c("estimated H", "H = 1", "H = 0.5", "H = 0"),
    col = c("blue", "red", "green2", "magenta"),
    cex = 0.8
  )

  plot(
    fit$fitted.values,
    fit$residuals,
    main = "Residuals vs Fitted",
    xlab = "Fitted values",
    ylab = "Residuals"
  )
  abline(h = 0, lwd = 2, col = "blue")

  invisible(fit)
}



# General plotting and estimation interface -------------------------------


#' Plot Hurst exponent estimation diagnostics
#'
#' Generic plotting interface for Hurst exponent estimation. The argument
#' `method` selects the estimation method.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param method Character string. Estimation method. One of
#'   `"rescaled_range"` or `"aggregated_variance"`.
#' @param ... Further arguments passed to the selected plotting function.
#'
#' @return Invisibly returns the fitted linear model from the selected method.
#'
#' @export
plot_hurst <- function(x, method = "rescaled_range", ...) {
  method <- match.arg(method, c("rescaled_range", "aggregated_variance"))

  if (method == "rescaled_range") {
    plot_hurst_rs(x, ...)
  } else {
    plot_hurst_aggvar(x, ...)
  }
}


#' Estimate the Hurst exponent
#'
#' General interface for estimating the Hurst exponent of a univariate time
#' series. The argument `method` selects the estimation method.
#'
#' @param x Numeric vector. The time series to analyse.
#' @param method Character string. Estimation method. One of
#'   `"rescaled_range"` or `"aggregated_variance"`.
#' @param ... Further arguments passed to the selected estimation function.
#'
#' @return Numeric value. Estimated Hurst exponent.
#'
#' @export
estimate_hurst <- function(x, method = "rescaled_range", ...) {
  method <- match.arg(method, c("rescaled_range", "aggregated_variance"))

  if (method == "rescaled_range") {
    estimate_hurst_rs(x, ...)
  } else {
    estimate_hurst_aggvar(x, ...)
  }
}
