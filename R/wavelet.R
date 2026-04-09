
#' z-transforma data
#' @export
ztransform <- function(x) (x-mean(x))/sd(x)

#' Shorten data
#' @export
shorten <- function(x, by=2, op=mean) {
  if (is.matrix(x)) {
    n <- nrow(x)
    m <- ncol(x)
    y <- matrix(nrow=ceiling(n/by), ncol=m)
    for (j in 1:m)
      y[,j] <- as.vector(tapply(x[,j], floor(0:(n-1)/by)+1, op))
    if (n %% by > 0)
      y[-ceiling(n/by),]
    else
      y
  }
  else {
    n <- length(x)
    y <- tapply(x, floor(0:(n-1)/by)+1, op)
    if (n %% by > 0)
      y[-ceiling(n/by)]
    else
      y
  }
}


#' Plot cwt
#' @export
plotcwt <- function(cw, nlevels=20, contour=T, shortage=max(1, ceiling(nrow(cw)/1000)), shortop=mean, main="Wavelet Powerspectrum", ar=NULL,
                    palette=pal, tunit='years',
                    rescale.method="quantile", ...) {
  spectr <- Mod(as.matrix(cw))^2
  mycol <- palette(nlevels)
  myx <- shorten(attr(cw, "time"), by=shortage, op=shortop)
  myy <- log(attr(cw, "scale") * attr(cw, "fourier_factor"), base=2)
  myz <- shorten(spectr, by=shortage, op=shortop)
  if (rescale.method == "quantile") {
    qs <- quantile(as.vector(myz), seq(0, 1, length=nlevels+1))
    squeeze <- interpol(qs, 1:length(qs))
    myz <- matrix(squeeze(as.vector(myz)), nrow=nrow(myz), ncol=ncol(myz))
  }
  if (rescale.method == "log") {
    for (i in 1:ncol(myz))
      myz[,i] <- myz[,i] / (attr(cw, "scale")[i] * attr(cw, "fourier_factor"))
    eps <- 1/sd(abs(as.vector(myz)))
    f <- function(x) sign(x) * log(abs(x) * eps + 1, base = 2)
    g <- function(x) sign(x) * (2^abs(x) - 1) / eps
    r <- sapply(smart.range(myz), f)
    qs <- sapply(seq(r[1], r[2], length = nlevels + 1), g)
    myz <- (matrix(sapply(myz, f), nrow = nrow(myz)) - r[1]) / (r[2] - r[1]) * nlevels + 1
  }
  #print(summary(as.vector(myz)))
  #print(qs)
  myx.coi <- c(min(myx), min(myx), myx, max(myx), max(myx))
  myy.coi <- c(max(myy), min(myy), log(shorten(attr(cw, "coi"), by=shortage), base=2), min(myy), max(myy))
  drawcoi <- function() polygon(myx.coi, myy.coi, density=18)
  if (contour) {
    ticks <- pretty(1:length(qs))+1
    filled.contour(myx, myy, myz, levels=1:length(qs), col=mycol, ...,
                   xlab=paste("Time (",tunit,")", sep=""),
                   ylab=paste("Scale (",tunit,")", sep=""),
                   main=main,
                   plot.axes={axis(1);logyrs <- ceiling(min(myy)):floor(max(myy));axis(2, at=logyrs, labels=signif(2^logyrs, 2));drawcoi()},
                   key.axes={axis(4,at=ticks,labels=signif(qs[ticks], digits=2))},
                   extra.space=0.25, extra.plot={plotgw(cw, sub=T, ar=ar)})
  }
  else {
    image(x=myx, y=myy, z=myz, breaks=qs, col=mycol, ..., main="Wavelet Powerspectrum",
                   xlab=paste("Time (",tunit,")", sep=""),
                   ylab=paste("Scale (",tunit,")", sep=""))
    drawcoi()
  }
}


#' Global wavelet
#' @export
global.wavelet <- function(cw) {
  n <- nrow(cw)
  m <- ncol(cw)
  scales <- attr(cw, "scale")
  gp <- vector(length=m)
  for (i in 1:m) {
    row <- cw[incoi(cw, scales[i] * attr(cw, "fourier_factor")),i]
    if (length(row) > 0)
      gp[i] <- mean(sapply(row, function(x) Mod(x)^2))
    else
      gp[i] <- NA
  }
  gp
}

#' Plot global wavelet
#' @export
plotgw <- function(cw, sub=F, ar=NULL, ...) {
  gp <- global.wavelet(cw)
  myscales <- attr(cw, "scale")
  fou <- attr(cw, "fourier_factor")
  myy <- log(myscales * fou, base=2)
  if (!sub)
    plot(gp, myy, type="l", ylim=range(myy), xlim=c(0, max(gp, na.rm=T)), yaxs='i', lwd=2,
         main="Global Wavelet", xlab="Variance", ylab="log2(scale [years])", ...)
  else {
    mar <- par("mar")
    mar[2] <- 0
    mar[3] <- 1
    plot.new()
    title("Global Wavelet", xlab="Variance",...)
    plot.window(ylim=range(myy), xlim=c(0, max(gp, na.rm=T)), yaxs='i',...)
    lines(gp, myy, type="l", lwd=2)
    axis(1)
    box()
  }
  x <- attr(cw, "series")
  if (is.null(ar))
    myalpha <- guess.ar(x)
  else
    myalpha <- ar
  theox <- noise.power(length(x), myalpha)[-1]
  theoy <- log(attr(cw, "sampling.interval") * length(x) / 1:length(theox) / fou, base=2)
  lines(theox, theoy , lty=3)
  if (attr(cw, "wavelet")=='morlet')
    mydf <- 2
  else
    mydf <- 1
  lines(1/mydf * qchisq(0.95, mydf) * theox, theoy , lty=2) # 1/mydf hat gefehlt
}

#' CWT
#' @export
cwt <- function(x,  wavelet="morlet", shift=5, ...) {
  if (!is.ts(x))
    x <- ts(x, start=1, deltat=1/365)
  raw <- ztransform(x)
  f <- freefoufac(wavelet, shift)
  smin <- max(c(1,2/f))
  smax <- length(x)/(2*f)
  cw <- wavCWT(raw, wavelet=wavelet, shift=shift, scale.range=deltat(x)*c(1, smax), ...)
  attr(cw, "fourier_factor") <- foufac(cw)
  attr(cw, "coi") <- coi(cw)
  cw
}

#' Foufac
#' @export
foufac <- function(cw) freefoufac(attr(cw, "wavelet"), attr(cw, "filter.arg"))

#' Freefoufac
#' @export
freefoufac <- function(wavelet, omega) {
  fourier_factor = 1
  if (wavelet=='gaussian2' || wavelet=='gauss2')   fourier_factor = (2*pi)/sqrt(2+1/2)
  if (wavelet=='gaussian1' || wavelet=='gauss1')   fourier_factor = (2*pi)/sqrt(1+1/2)
  if (wavelet=='morlet') fourier_factor=(4*pi)/(omega+sqrt(2+omega^2))
  fourier_factor
}

#' coi
#' @export
coi <- function(cw) {
  wavelet <- attr(cw, "wavelet")
  x <- attr(cw, "series")
  coi.number = 1/sqrt(2)
  coi.number*attr(cw, "sampling.interval")*c(1E-5,1:((length(x)+1)/2-1),rev((1:(length(x)/2-1))),1E-5)
}

#' incoi
#' @export
incoi <- function(cw, scale) {
  mycoi <- attr(cw, "coi")
  mycoi[1:nrow(cw)] >= scale
}

#' Interpolation
#' @export
interpol <- function(xs, ys) {
  n <- length(xs)
  f <- function(x) {
    if (x < xs[1]) {
      NA
    }
    else if (x > xs[n]) {
      NA
    }
    else {
      for (i in 1:n) {
        if (x == xs[i]) {
          return(ys[i])
        }
        else if (x < xs[i]) {
          d <- (x - xs[i-1]) / (xs[i] - xs[i - 1])
          return((1-d) * ys[i-1] + d * ys[i])
        }
      }
    }
  }
  function(x) sapply(x, f)
}

#' Monte
#' @export
monte <- function(f, n, k=20, wavelet="morlet", ...) {
  mycw <- cwt(f(n), wavelet=wavelet, ...)
  tab <- matrix(ncol=k, nrow=length(attr(mycw, "scale")))
  tab[,1] <- global.wavelet(mycw)
  if (k > 1)
    for (j in 2:k) {
      mycw <- cwt(f(n), wavelet=wavelet, ...)
      tab[,j] <- global.wavelet(mycw)
    }
  tab
  ls <- c(0, 0.05, 0.5, 0.95, 1)
  qs <- matrix(nrow=nrow(tab), ncol=length(ls))
  for (i in 1:nrow(tab))
    qs[i,] <- quantile(tab[i,], ls, na.rm=T)
  attr(qs, "scale") <- attr(mycw, "scale")
  qs
}

#' Plot monte
#' @export
plotmonte <- function(m) {
  myy <- log(attr(m, "scale"), base=2)
  plot(c(), ylim=range(myy[which(!is.na(m[,3]))]), xlim=range(na.omit(m[,2:4])))
  polygon(x=c(m[,2], m[,4]), y=c(myy, myy), col="blue")
  lines(m[,3], myy, lwd=2)
 # for (j in 1:ncol(m)) {
 #   lines(m[,j], attr(m, "scale"))
 # }
}

# k*deltat = sqrt(2)*s
# foufac * s = 2 * deltat

# lambda = foufac * s = deltat * n / k

#' Guess AR
#' @export
guess.ar <- function(x) {
  myac <- acf(x, plot=F)
  mean(c(myac$acf[2], sqrt(max(0,myac$acf[3]))))
}

#' Noise power
#' @export
noise.power <- function(n, alpha=0) {
  (1 - alpha^2) / (1 + alpha^2 - 2 * alpha * cos(2 * pi * (0:floor(n/2)) / n))
}

#' Smart range
#' @export
smart.range <- function(x) {
  r <- range(x)
  if (r[1] > 0)
    c(0, r[2])
  else if (r[2] < 0)
    c(r[1], 0)
  else {
    s <- max(abs(r))
    c(-s, s)
  }
}

#' Get color palette
#' @export
pal=function(n) rev(rainbow(n, start=0, end=4/6))
