periodogram <- function(x,fscale=1,funit="1/t",log="",type="l") {
  Sq <- function(x) Re(x)^2 + Im(x)^2
  per <- sapply(fft(x)[-1][1:floor(length(x)/2)], Sq)
  per <- per / sum(per)
  plot(1:length(per)/length(x)*fscale,per,log=log,type=type,main="Periodogram",xlab=paste("Frequency (",funit,")"),ylab="Spectral Power")
}

