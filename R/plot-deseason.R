
#' Plot deseason
#' @export
plot.deseason=function(t,x,d,n=40) {
  p=d
  dummy=deseason(t,x,p)
  dummy.loess=loess(dummy~t,span=0.3)
  x.loess=loess(x~t,span=0.3)
  op=par(mfrow= c(2,2))
  plot(t,x,type='l',ylab='Observations',xlab='Time',main='Original data')
  lines(t,x.loess$fitted,type='l',col='red',lwd=2)
  legend('bottomleft',legend=c('Data','Smooth curve'),lty=1,col=c('black','red'))
  plot(t,dummy,type='l',ylab='Observations',xlab='Time',main='Deseasonalised data')
  lines(t,dummy.loess$fitted,type='l',col='red',lwd=2)
  legend('bottomleft',legend=c('Data','Smooth curve'),lty=1,col=c('black','red'))
  hist.normal(x,n)
  hist.normal(dummy,n)
  par(op)
}





