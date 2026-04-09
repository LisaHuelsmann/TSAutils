


#' Verify Orthogonality
#' @export
plot.ortho <- function(f, g, kmax=3) {
  n <- round(1024/kmax)
  op=par(mfcol=c(kmax, kmax),mar=c(4,4,1,1))
  for (i in 1:kmax)
    for (j in 1:kmax) {
      f1 <- function(t) f(2 * pi * i / n * t)
      g1 <- function(t) g(2 * pi * j / n * t)
      h <- function (t) f1(t) * g1(t)
	up <- function (t) max(h(t),0)
	down <- function (t) min(h(t),0)
	plot(c(1,n), c(1,-1), col="white", xlab='t',ylab='')
	points(sapply(1:n, up), type="h", col="green")
	points(sapply(1:n, down), type="h", col="red")
      points(sapply(1:n, f1), type="l", col="cyan",lwd=2)
      points(sapply(1:n, g1), type="l", col="magenta",lwd=2)
    }
par(op)
}

#' Fourier analysis
#' @export
dft <- function(x) {
  n <- length(x)
  if (n %% 2 == 0) {
    kmax = n/2-1
    a <- matrix(nrow=kmax+2,ncol=3)} else {
      kmax = (n-1)/2
      a <- matrix(nrow=kmax+1,ncol=3)}
  #kmax <- floor(n/2)
  #a <- matrix(nrow=kmax+1,ncol=3)
  colnames(a) <- c("k", "a", "b")
  for (k in 0:kmax) {
    s <- sapply(0:(n-1), function (t) sin(2 * pi * k / n * t)) #korrigiert. war t = 1:n
    c <- sapply(0:(n-1), function (t) cos(2 * pi * k / n * t)) #korrigiert. war t = 1:n
    a[k+1,1] <- k
    a[k+1,2] <- 2 / n * sum(x * c) #a_k=2_Re(c_k)
    a[k+1,3] <- 2 / n * sum(x * s) #b_k=-2*Im(c_k)
  }
  a[1,2] <- a[1,2] / 2 #a_0=c_0
  if (n %% 2 == 0) {
    c <- sapply(0:(n-1), function (t) cos(2 * pi * (kmax+1) / n * t))
    a[kmax+2,1] <- kmax + 1
    a[kmax+2,2] <- 1 / n * sum(x * c) #a_(n/2)=c_(n/2)
    a[kmax+2,3] = 0}
  a
}

#' Fourier Synthesis
#' @export
undft <- function(coeffs, n) {
  x <- rep(0, times=n)
  for (t in 0:(n-1)) {
    s <- sapply(coeffs[,1], function (k) sin(2 * pi * k / n * t))
    c <- sapply(coeffs[,1], function (k) cos(2 * pi * k / n * t))
    x[t+1] <- sum(coeffs[,3] * s + coeffs[,2] * c)
  }
  x
}

#' Return zero
#' @export
zero <- function(t) 0

#' Plot terms
#' @export
plotterms <- function(x, g1=zero, g2=zero, g3=zero,
                      g4=zero, g5=zero, h=1) {
  t <- 0:(h*(length(x)-1))/h
  points(t+1, sapply(t, function (t) g1(t) + g2(t) + g3(t) + g4(t)),
         type="b", col="blue",lwd=2)
}

#' Approximation
#' @export
approx <- function(x, g1=zero,g2=zero,g3=zero,g4=zero,g5=zero,...) {
  plot(x, type="o",lwd=2,...)
  plotterms(x, g1, g2, g3, g4,g5)
}

#' Fourier sinus
#' @export
fourier.sin <- function(coeffs, k, n, h=1) {
  t <- 0:(h*(n-1))/h
  coeffs[k+1,3] * sin(2 * pi * k / n * t)
  }

#' Fourier cosinus
#' @export
fourier.cos <- function(coeffs, k, n, h=1) {
  t <- 0:(h*(n-1))/h
  coeffs[k+1,2] * cos(2 * pi * k / n * t)
  }

# for (t in 0:(n-1)) {
#   s <- sapply(coeffs[,1], function (k) sin(2 * pi * k / n * t))
#   c <- sapply(coeffs[,1], function (k) cos(2 * pi * k / n * t))
#   x[t+1] <- sum(coeffs[,3] * s + coeffs[,2] * c)
# }

