
#' Plot series
#' @export
plot.series <- function(sets) {
  n <- nrow(sets)
  r <- ceiling(sqrt(n))
  q <- ceiling(n/r)
  p <- par(mfcol=c(r, q))
  for (i in 1:n)
    plot(sets[i,1][[1]], sets[i,2][[1]], main=sets[i,3][[1]],
         type="l", ylab="x", xlab="t")
  par(p)
}

#' Plot periodogram
#' @export
plot.periodograms <- function(sets, fit=F,lwd=2,cex.axis=1.5,cex.lab=1.5) {
  n <- nrow(sets)
  r <- ceiling(sqrt(n))
  q <- ceiling(n/r)
  p <- par(mfcol=c(r, q))
  power <- function(z) Re(z)^2 + Im(z)^2
  for (i in 1:n) {
    x <- sets[i,2][[1]]
    n <- length(x)
    fourier <- sapply(fft(x)[-1], power)[1:floor(n/2)]
    plot(fourier, log="xy", type="l", xlab="Frequency (k)",
    ylab="Spectral Density", main=sets[i,3][[1]],lwd=lwd,cex.axis=cex.axis,
    cex.lab=cex.lab)
    if (fit) plot.logfit(fourier)
  }
  par(p)
}

#' Plot log fit
#' @export
plot.logfit <- function(fourier) {
  k <- length(fourier)
  q <- 1
  lf <- sapply(fourier[q:k], log)
  f <- sapply(q:k, log)
  lfm <- lm(lf ~ f)
  a <- lfm$coefficients
  lines(c(q,k),c(exp(a[1]+a[2]*log(q)),exp(a[1]+a[2]*log(k))),col="red")
  low <- min(fourier)
  high <- max(fourier)
  text(k, low*(high/low)^0.98, c(paste("beta ~=", round(-a[2], digits=2))), pos=2, col="red",cex=1.5)
}

#' Plot autocorrelations
#' @export
plot.autocorrelations <- function(sets) {
  n <- nrow(sets)
  r <- ceiling(sqrt(n))
  q <- ceiling(n/r)
  p <- par(mfcol=c(r, q))
  for (i in 1:n) {
    x <- sets[i,2][[1]]
    a <- acf(x, lag.max=length(x)-1, plot=F)
    plot(a$acf, main=sets[i,3][[1]], type="l", ylab="r", xlab="Lag")
  }
  par(p)
}

#' Plot powerspectra
#' @export
plot.powerspectra <- function(sets, fit=F,lwd=2,cex.axis=1.5,cex.lab=1.5) {
  n <- nrow(sets)
  r <- ceiling(sqrt(n))
  q <- ceiling(n/r)
  p <- par(mfcol=c(r, q))
  power <- function(z) Re(z)^2 + Im(z)^2
  for (i in 1:n) {
    x <- sets[i,2][[1]]
    l <- length(x)
    a <- acf(x, plot=F, lag.max=l-1)
    fourier <- sapply(fft(a$acf)[-1], power)[1:floor(l/2)]
    k <- length(fourier)
    plot(fourier, log="xy", type="l", xlab="Frequency (k)",lwd=lwd,cex.axis=cex.axis,
    cex.lab=cex.lab,
    ylab="Power Density", main=sets[i,3][[1]])
    if (fit) plot.logfit(fourier)
  }
  par(p)
}


