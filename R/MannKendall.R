
#' Plot cutoff
#' @export
plot.cutoff <- function(x, n, k) {
  lens <- length(x) - 1:n * k
  mkp <- sapply(lens, function(l) MannKendall(x[1:l]))
  mks <- sapply(lens, function(l) sens.slope(x[1:l])$estimate)
  op <-par(mfcol=c(3,1),mar=.1+c(4,4,2,1))
  plot(lens, unlist(mkp[1,]), type="b",ylab='tau',xlab='Length of the data set')
  abline(h=0,col='red')
  plot(lens, unlist(mks), type="b",ylab='Sen slope',xlab='Length of the data set')
  #abline(c(0.05,0), col="blue")
  plot(lens, unlist(mkp[2,]), type="b",ylab='p-value',xlab='Length of the data set')
  abline(c(0.05,0), col="blue")
  legend('topright',legend='95% significance level',col='blue',lty=1 )
  par(op)
}
