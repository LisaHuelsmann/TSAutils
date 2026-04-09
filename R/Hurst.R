




#' Select a window from a vector
#' @export
portion <- function(x, first=1, last=length(x)) {
  if (last < first)
    numeric(0)
  else
    x[first:last]
}
#  result has length max(last - first + 1, 0)


#' Z-transform a vector
#' @export
ztrans <- function(x)  (x-mean(x))/sd(x)
#  result has mean 0 and sd 1

#' eq space
#' @export
eqspace <- function(n, min=0, max=1) {
  if (n == 1)
    return (max+min)/2
  else
    sapply(0:(n-1), function (k) k * (max - min) / (n - 1) + min)
}

#' logspace
#' @export
logspace <- function(n, min=1, max=exp(1)) {
  exp(eqspace(n,log(min),log(max)))
}


#' Hurst rescaled
#' @export
hurst.rescaled.range <- function (x, windows=10, sizes=50, min.size=10, logkmin=0.5) {
  rs <- function(t, k) {
    win <- portion(x, t+1, t+k)
    rng <- range(cumsum(ztrans(win)))
    rng[2] - rng[1]
  }
  samp <- matrix(nrow=windows, ncol=sizes)
  n <- length(x)
  ks <- round(logspace(sizes, min.size, floor(n/windows))) # sizes of windows
  for (j in 1:sizes) {
    k <- ks[j]
    ts <- round(eqspace(windows, 1, n-k+1)) # start points of windows
    for (i in 1:windows) {
      t <- ts[i]
      samp[i,j] <- rs(t-1, k)
    }
  }
  result <- matrix(nrow=sizes*windows, ncol=2)
  for (j in 1:sizes)
    if (log(ks[j], base=10) >= logkmin)
      for (i in 1:windows)
        result[(j-1)*windows+i,] <- log(c(ks[j], samp[i, j]), base=10)
    else
      result[(j-1)*windows+(1:windows),] <- NA
  result
}

#' Plot Hurst rescaled
#' @export
plot.hurst.rescaled.range <- function(x) {
  # x11()
  plot(x, type="l", lwd=2, main="Input Data", ylab="Value")
  # x11()
  par(mfrow=c(2, 2))
  acf(x, lwd=6)
  rs.stuff <- hurst.rescaled.range(x)
  rs.lm <- lm(rs.stuff[,2] ~ rs.stuff[,1])
  b <- rs.lm$coefficients[1]
  plot(rs.stuff, main=paste("Hurst R/S Estimation, H~=", round(rs.lm$coefficients[2], digits=2)),
       xlab="log(k)", ylab="log(R/S)",
       ylim=range(c(b, na.omit(rs.stuff[,2]))))
  abline(rs.lm$coefficients, lwd=2, col="blue")
  abline(b, 1, lwd=2, lty=2, col="red")
  abline(b, 0.5, lwd=2, lty=3, col="green2")
  abline(h=b, lwd=2, lty=2, col="magenta")
  legend("topleft", lwd=2, lty=c(1,2,3,2), legend=c("H","1","0.5","0"),
         col=c("blue", "red", "green2", "magenta"))
  qqnorm(rs.lm$residuals, main="Normal Q-Q Plot of Residuals")
  qqline(rs.lm$residuals, lwd=2)
  plot(rs.lm$fitted, rs.lm$residuals, main="Scatter Plot of Residuals", xlab="Fitted",
       ylab="Residuals")
  abline(h=0, lwd=2, col="blue")
}

#' Hurst rs
#' @export
hurst.rs <- function(x, windows =10, sizes=50, min.size) {
  rs.stuff <- hurst.rescaled.range(x)
  rs.lm <- lm(rs.stuff[,2] ~ rs.stuff[,1])
  round(as.vector(rs.lm$coefficients[2]), digits=4)
  }

#' Hurst aggregated variances
#' @export
hurst.aggvar <- function(x, sizes=50) {
  rs.stuff <- hurst.aggregated.variance(x, sizes=50)
  rs.lm <- lm(rs.stuff[,2] ~ rs.stuff[,1])
  round(as.vector((rs.lm$coefficients[2]+2)/2), digits=4)
}

#' #' Hurst aggregated variances helper
#' @export
hurst.aggregated.variance <- function(x, sizes=50, kmin=10) {
  n <- length(x)
  ms <- floor(logspace(sizes, 2, floor(n/2)))
  result <- matrix(nrow=sizes, ncol=2)
  for (j in 1:sizes) {
    m <- ms[j]
    windows <- floor(n/m)
    if (windows < kmin)
      result[j,] <- NA
    else {
      samp <- sapply(1:windows, function(k) mean(portion(x, (k-1)*m+1, k*m)))
      result[j,] <- log(c(m, var(samp)), base=10)
    }
  }
  result
}

#' Plot Hurst aggregated variances
#' @export
plot.hurst.aggregated.variance <- function(x) {
  plot(x, type="l", lwd=2, main="Input Data", ylab="Value")
  par(mfrow=c(2, 2))
  acf(x, lwd=6)
  rs.stuff <- hurst.aggregated.variance(x)
  rs.lm <- lm(rs.stuff[,2] ~ rs.stuff[,1])
  b <- rs.lm$coefficients[1]
  plot(rs.stuff, main=paste("Hurst Variance Estimation, H~=", round((2+rs.lm$coefficients[2])/2, digits=2)),
       xlab="log(m)", ylab=expression(log(s[m]^2)),
       ylim=range(c(b, na.omit(rs.stuff[,2]))))
  abline(rs.lm$coefficients, lwd=2, col="blue")
  abline(b, 0, lwd=2, lty=2, col="red")
  abline(b, -1, lwd=2, lty=3, col="green2")
  abline(b, -2, lwd=2, lty=2, col="magenta")
  legend("bottomleft", lwd=2, lty=c(1,2,3,2), legend=c("H","1","0.5","0"),
         col=c("blue", "red", "green2", "magenta"))
  qqnorm(rs.lm$residuals, main="Normal Q-Q Plot of Residuals")
  qqline(rs.lm$residuals, lwd=2)
  plot(rs.lm$fitted, rs.lm$residuals, main="Scatter Plot of Residuals", xlab="Fitted",
       ylab="Residuals")
  abline(h=0, lwd=2, col="blue")
}


#' Plot Hurst
#' @export
plot.hurst <- function(x, method="rescaled.range", ...) {
  get(paste("plot.hurst.", method, sep=""))(x, ...)
}

