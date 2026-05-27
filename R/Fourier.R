# Fourier orthogonality ----------------------------------------------------

#' Plot Fourier orthogonality
#'
#' Visualizes the orthogonality of sine and cosine functions for different
#' frequencies.
#'
#' @param f Function. First trigonometric function, usually `sin` or `cos`.
#' @param g Function. Second trigonometric function, usually `sin` or `cos`.
#' @param kmax Integer. Maximum frequency shown.
#'
#' @return No return value. Called for its side effect of producing a plot.
#'
#' @export
plot_fourier_orthogonality <- function(f, g, kmax = 3) {
  n <- round(1024 / kmax)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))

  par(mfcol = c(kmax, kmax), mar = c(4, 4, 1, 1))

  for (i in 1:kmax) {
    for (j in 1:kmax) {
      f1 <- function(t) f(2 * pi * i / n * t)
      g1 <- function(t) g(2 * pi * j / n * t)
      h <- function(t) f1(t) * g1(t)

      up <- function(t) max(h(t), 0)
      down <- function(t) min(h(t), 0)

      plot(c(1, n), c(1, -1), col = "white", xlab = "t", ylab = "")
      points(sapply(1:n, up), type = "h", col = "green")
      points(sapply(1:n, down), type = "h", col = "red")
      points(sapply(1:n, f1), type = "l", col = "cyan", lwd = 2)
      points(sapply(1:n, g1), type = "l", col = "magenta", lwd = 2)
    }
  }
}


# Fourier decomposition ----------------------------------------------------

#' Decompose a time series into Fourier coefficients
#'
#' Computes real-valued Fourier coefficients of a univariate time series using
#' sine and cosine basis functions.
#'
#' @param x Numeric vector or time series. Input series.
#'
#' @return A numeric matrix with columns:
#' \describe{
#'   \item{k}{Frequency index.}
#'   \item{a}{Cosine coefficient.}
#'   \item{b}{Sine coefficient.}
#' }
#'
#' @details
#' This function is intended as a teaching-oriented real Fourier decomposition.
#' It returns sine and cosine coefficients rather than the complex coefficients
#' returned by `fft()`.
#'
#' @export
fourier_decompose <- function(x) {
  n <- length(x)

  if (n %% 2 == 0) {
    kmax <- n / 2 - 1
    coeffs <- matrix(nrow = kmax + 2, ncol = 3)
  } else {
    kmax <- (n - 1) / 2
    coeffs <- matrix(nrow = kmax + 1, ncol = 3)
  }

  colnames(coeffs) <- c("k", "a", "b")

  for (k in 0:kmax) {
    s <- sapply(0:(n - 1), function(t) sin(2 * pi * k / n * t))
    c <- sapply(0:(n - 1), function(t) cos(2 * pi * k / n * t))

    coeffs[k + 1, 1] <- k
    coeffs[k + 1, 2] <- 2 / n * sum(x * c)
    coeffs[k + 1, 3] <- 2 / n * sum(x * s)
  }

  coeffs[1, 2] <- coeffs[1, 2] / 2

  if (n %% 2 == 0) {
    c <- sapply(0:(n - 1), function(t) cos(2 * pi * (kmax + 1) / n * t))

    coeffs[kmax + 2, 1] <- kmax + 1
    coeffs[kmax + 2, 2] <- 1 / n * sum(x * c)
    coeffs[kmax + 2, 3] <- 0
  }

  coeffs
}


# Fourier reconstruction ---------------------------------------------------

#' Reconstruct a time series from Fourier coefficients
#'
#' Reconstructs a time series from real-valued sine and cosine Fourier
#' coefficients.
#'
#' @param coeffs Numeric matrix. Fourier coefficients as returned by
#'   `fourier_decompose()`.
#' @param n Integer. Length of the reconstructed series.
#'
#' @return Numeric vector of length `n`.
#'
#' @export
fourier_reconstruct <- function(coeffs, n) {
  x <- rep(0, times = n)

  for (t in 0:(n - 1)) {
    s <- sapply(coeffs[, 1], function(k) sin(2 * pi * k / n * t))
    c <- sapply(coeffs[, 1], function(k) cos(2 * pi * k / n * t))

    x[t + 1] <- sum(coeffs[, 3] * s + coeffs[, 2] * c)
  }

  x
}


# Fourier term functions ---------------------------------------------------

#' Return a zero-valued function
#'
#' Internal helper that returns zero for any input.
#'
#' @param t Numeric input.
#'
#' @return Numeric value 0.
#'
#' @keywords internal
zero_function <- function(t) {
  0
}


#' Compute a Fourier sine term
#'
#' Computes the sine component for one Fourier frequency.
#'
#' @param coeffs Numeric matrix. Fourier coefficients as returned by
#'   `fourier_decompose()`.
#' @param k Integer. Frequency index.
#' @param n Integer. Length of the original series.
#' @param h Numeric. Resolution factor for plotting.
#'
#' @return Numeric vector containing the sine component.
#'
#' @export
fourier_sine_term <- function(coeffs, k, n, h = 1) {
  t <- 0:(h * (n - 1)) / h
  coeffs[k + 1, 3] * sin(2 * pi * k / n * t)
}


#' Compute a Fourier cosine term
#'
#' Computes the cosine component for one Fourier frequency.
#'
#' @param coeffs Numeric matrix. Fourier coefficients as returned by
#'   `fourier_decompose()`.
#' @param k Integer. Frequency index.
#' @param n Integer. Length of the original series.
#' @param h Numeric. Resolution factor for plotting.
#'
#' @return Numeric vector containing the cosine component.
#'
#' @export
fourier_cosine_term <- function(coeffs, k, n, h = 1) {
  t <- 0:(h * (n - 1)) / h
  coeffs[k + 1, 2] * cos(2 * pi * k / n * t)
}


# Fourier approximation plots ---------------------------------------------

#' Add Fourier terms to an existing plot
#'
#' Internal helper that adds the sum of up to five component functions to an
#' existing plot.
#'
#' @param x Numeric vector. Original series.
#' @param g1 Function. First component function.
#' @param g2 Function. Second component function.
#' @param g3 Function. Third component function.
#' @param g4 Function. Fourth component function.
#' @param g5 Function. Fifth component function.
#' @param h Numeric. Resolution factor for plotting.
#'
#' @return No return value. Called for its side effect of adding points to a
#'   plot.
#'
#' @keywords internal
add_fourier_terms <- function(x,
                              g1 = zero_function,
                              g2 = zero_function,
                              g3 = zero_function,
                              g4 = zero_function,
                              g5 = zero_function,
                              h = 1) {
  t <- 0:(h * (length(x) - 1)) / h

  points(
    t + 1,
    sapply(t, function(t) {
      g1(t) + g2(t) + g3(t) + g4(t) + g5(t)
    }),
    type = "b",
    col = "blue",
    lwd = 2
  )
}


#' Plot a Fourier approximation
#'
#' Plots a time series and overlays an approximation constructed from selected
#' Fourier component functions.
#'
#' @param x Numeric vector or time series. Original series.
#' @param g1 Function. First Fourier component function.
#' @param g2 Function. Second Fourier component function.
#' @param g3 Function. Third Fourier component function.
#' @param g4 Function. Fourth Fourier component function.
#' @param g5 Function. Fifth Fourier component function.
#' @param ... Further arguments passed to `plot()`.
#'
#' @return No return value. Called for its side effect of producing a plot.
#'
#' @export
plot_fourier_approximation <- function(x,
                                       g1 = zero_function,
                                       g2 = zero_function,
                                       g3 = zero_function,
                                       g4 = zero_function,
                                       g5 = zero_function,
                                       ...) {
  plot(x, type = "o", lwd = 2, ...)
  add_fourier_terms(x, g1, g2, g3, g4, g5)
}
