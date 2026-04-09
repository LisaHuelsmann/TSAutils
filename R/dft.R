# Fourier Analysis
dft <- function(x) {
  n <- length(x)
  kmax <- floor(n/2)
  a <- matrix(nrow=kmax+1,ncol=3)
  colnames(a) <- c("k", "a", "b")
  for (k in 0:kmax) {
    s <- sapply(1:n, function (t) sin(2 * pi * k / n * t))
    c <- sapply(1:n, function (t) cos(2 * pi * k / n * t))
    a[k+1,1] <- k
    a[k+1,3] <- 2 / n * sum(x * s) #b_k=-2*Im(c_k)
    a[k+1,2] <- 2 / n * sum(x * c) #a_k=2_Re(c_k)
  }
  a[1,2] <- a[1,2] / 2 #a_0=c_0
  a[kmax+1,2] <- a[kmax+1,2] / 2 #a_(n/2)=c_(n/2)
  a
}


# Fourier Synthesis
undft <- function(coeffs) {
  n=length(coeffs[,2])*2-2
  x <- rep(0, times=n)
  for (t in 1:n) {
    s <- sapply(coeffs[,1], function (k) sin(2 * pi * k / n * t))
    c <- sapply(coeffs[,1], function (k) cos(2 * pi * k / n * t))
    x[t] <- sum(coeffs[,3] * s + coeffs[,2] * c)
  }
  x
}

undft.inter <- function(a) {
  n <- 2 * (nrow(a) - 1)
  s <- function(t) sapply(a[,1], function (k) sin(2 * pi * k / n * t))
  c <- function(t) sapply(a[,1], function (k) cos(2 * pi * k / n * t))
  function(t) sum(a[,3] * s(t) + a[,2] * c(t))
}
