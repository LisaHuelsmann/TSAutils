
zero <- function(t) 0

plotterm <- function(t, f1, f2, f3, d=20) {
  n <- length(t)
  T <- t[n] - t[1]
  dt <- 0:(d*(n-1))/(d*(n-1))*T + t[1]
  points(dt, sapply(0:(d*(n-1))/d+1, function(t) f1(t) + f2(t) + f3(t)), type="l", col="blue", lwd=2)
}

approx <- function(t, x, f1=zero, f2=zero, f3=zero, d=20, main=NULL) {
  plot(t, x*1.5, col="white", xlab="t", ylab="x", main=main)
  points(t, x, type="p", lwd=2)
  plotterm(t, f1, f2, f3, d)
}

fourier.sin <- function(n, a, k) function(t) a[k+1,2] * sin(2 * pi * k / n * t)
fourier.cos <- function(n, a, k) function(t) a[k+1,3] * cos(2 * pi * k / n * t)

reconstruct <- function(t, x, main=NULL) {
  a <- dft(x)
  approx(t, x, undft.inter(a),d=4,main=main)
}

reconstruct.mult <- function(f, ns) {
  r <- ceiling(sqrt(length(ns)))
  q <- ceiling(length(ns)/r) 
  par(mfrow=c(r, q))
  for (n in ns)
    reconstruct(1:n,f(n),main=paste("n =", n))
  par(mfrow=c(1,1))
}

rect <- function(n) {
  k <- floor(n/2)
  c(rep(1, times=k), rep(-1, times=n-k))
}

wedge <- function(n) {
  k <- floor(n/2)
  c(0:(k-1)/(k-1), (n-k-1):0/(n-k-1))
}

saw <- function(n) {
  k <- floor(n/2)
  c(0:(k-1)/(k-1), 0:(n-k-1)/(n-k-1))
}


ztrans <- function(x) (x - mean(x)) / sd(x)


