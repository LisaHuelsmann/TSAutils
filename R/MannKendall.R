
#' Plot Mann-Kendall trend results over decreasing sample lengths
#'
#' Computes the Mann-Kendall trend test and Sen's slope estimator on
#' progressively shorter prefixes of a numeric series, then plots the
#' resulting Kendall's tau, Sen slope, and p-values against the retained
#' data length.
#'
#' This is useful for assessing how sensitive trend estimates are to the
#' length of the time series used in the analysis.
#'
#' @param x A numeric vector containing the time series to analyse.
#' @param n An integer giving the number of cutoff steps to evaluate.
#' @param k An integer giving the step size for shortening the series.
#'   For each step, the series length is reduced by \code{k}.
#'
#' @details
#' The function evaluates the series lengths
#' \code{length(x) - (1:n) * k} and, for each retained length, computes:
#' \describe{
#'   \item{Kendall's tau}{Using \code{MannKendall()}.}
#'   \item{Sen slope}{Using \code{sens.slope()$estimate}.}
#'   \item{p-value}{From \code{MannKendall()}.}
#' }
#'
#' Three panels are produced:
#' \enumerate{
#'   \item Kendall's tau versus retained series length
#'   \item Sen slope versus retained series length
#'   \item p-value versus retained series length
#' }
#'
#' Horizontal reference lines are added at 0 for Kendall's tau and at
#' 0.05 for the p-value panel.
#'
#' The function is called for its side effect of producing plots and
#' returns no value.
#'
#' @return
#' No return value. This function is called for its side effect of
#' producing diagnostic plots.
#'
#' @seealso
#' \code{\link[Kendall]{MannKendall}},
#' \code{\link[trend]{sens.slope}}
#'
#' @examples
#' \dontrun{
#' x <- cumsum(rnorm(100, mean = 0.1, sd = 1))
#' MannKendall_stability(x, n = 10, k = 5)
#' }
MannKendall_stability <- function(x, n, k) {
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
