
#' Hist normal
#' @export
hist.normal <- function(x, n=40) {
  low <- floor(min(x))
  high <- ceiling(max(x))
  rng <- 0:n * (high-low) / n + low
  rng2 <- (2*low):(2*high) / 2
  a=hist(x, breaks=rng,plot=F)
  hist(x, breaks=rng, main=paste("Histogram,",n,"bins"),
       xlab="Observations",ylim=c(0,1.1*max(a$counts)))
  dens <- density(x, n=2*n)
  lines(dens$x, dens$y * length(x) * (high-low) / n, col="red", lw=2)
  lines(rng2, sapply(rng2, histnorm(x, (high-low) / n)), col="blue", lw=2)
  legend("topright", legend=c("Frequency", "Density", "Normal"), lty=1, lwd=2, col=c("black", "red", "blue"))#,text.width)
}

#' Histnorm???
#' @export
histnorm <- function(x, scale=1) {
  m <- mean(x)
  v <- var(x)
  n <- length(x)
  function(t) { n*scale/sqrt(2 * pi * v) * exp(-((t - m)^2 / (2 * v))) }
}
