
#' Binomial coefficient
#' @export
bc <- function(r, k) {
  b <- 1
  j <- 1
  if (k > 0)
    for (i in 1:k) {
      b <- b * r / j
      r <- r - 1
      j <- j + 1
    }
  b
}

#' bpow
#' @export
bpow <- function(x, y, r, n=100)
  sum(sapply(0:100, function(k) bc(r, k) * x^(r - k) * x^k))

#' bshift
#' @export
bshift <- function(x, k) {
  n <- length(x)
  if (k < n)
    c(rep(0, k), x[1:(n-k)])
  else
    rep(0, n)
}

#' fdiff
#' @export
fdiff <- function(x, d, k.max=30) {
  n <- length(x)
  y <- rep(0, n)
  for (k in 0:k.max)
    y <- y + bc(d, k) * (-1) ^ (k %% 2) * bshift(x, k) # bc=binomial coefficient, k%%2 gives k even or not
  y <- y[-(1:k.max)]
  as.ts(y)
}

#' Plot diff coefficients
#' @export
plot.diff.coefficients <- function(d, k.max=30) {
  ks <- 0:k.max
  bs <- sapply(ks, function(k) (-1)^k * bc(d, k))
  plot(ks, bs, type="h", lwd=6, ylim=c(min(0, min(bs)), max(1, max(bs))),
       xlab="k", ylab=expression(beta[k]), main=paste("Coefficients of (1-B)^", d, sep=""))
}

#' Plot nine
#' @export
plot.nine <- function(f, a=0, b=1) {
  par(mfrow=c(3,3))
  for (i in seq(a, b, length=9))
    f(i)
  par(mfrow=c(1,1))
}

