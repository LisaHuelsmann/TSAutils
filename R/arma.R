
#' Generate ARMA data
#'
#' interesting
#'
#' @export
arma.gen <- function(n, ar=0, ma=0, gen=rnorm, ...) {
  p <- length(ar)
  q <- length(ma)
  inno <- gen(n + q, ...)
  out <- vector("numeric", length=n + p)
  if (p > 0)
    out[1:p] <- 0
  for (t in 1:n) {
    out[t + p] <- inno[t + q]
    if (p > 0)
      out[t + p] <- out[t + p] + sum(ar * out[(t+p-1):t])
    if (q > 0)
      out[t + p] <- out[t + p] + sum(ma * inno[(t+q-1):t])
  }
  if (p > 0)
    out <- out[-(1:p)]
  as.ts(out)
}

#' Plot ARMA data
#' @export
plot.arma <- function(n=5000, ar=numeric(0), ma=numeric(0), gen=rnorm, lag.max=40) {
  x <- arma.gen(n=n, ar=ar, ma=ma, gen=gen)
  #x11()
  par(mfrow=c(2,2), oma=c(0,0,0,0))
  par(mar=c(3,4,2,0)+0.1)
  plot(x, lwd=2, main=paste("AR(", length(ar), ")/MA(", length(ma), ")", sep=""), ylab="X")
  plot(x[0:floor(sqrt(n))], type="l", lwd=2, main="Zoom", ylab="X")
  par(mar=c(3,4,0,0)+0.1)
  acf(x, lwd=6, lag.max=lag.max,ylim=c(-1,1))
  pacf(x, lwd=6, lag.max=lag.max,ylim=c(-1,1))
}

#' Calculate spread??
#' @export
spread <- function(total, length=1) rep(total / length, length)
