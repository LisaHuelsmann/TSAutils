purely.random <- function(n) rnorm(n)

add.trend <- function(x, strength=1)
  x + seq(from=0, to=strength*sd(x), length=length(x))

add.periodicity <- function(x, period=floor(length(x)/2))
  x + sapply(1:length(x), function(t) sin(2 * pi * t / period))
  
add.jumps <- function(x, strength=1, rate=2/3*log(length(x))) {
  aha <- cumsum(floor(rexp(length(x), rate=rate))) + 1
  x + strength * (runif(max(aha)))[aha]
}

