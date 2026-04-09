deseason <- function(t, x, d=365) {
  p=d
  frac <- 0:p / p  #calculate fractions of the year
  bin <- rep(0, p)
  n <- length(x)
  diff <- rep(0, n)
  for (i in 1:p) {
    season <- t - floor(t)  #extract fractions of the year from time information
    index <- which(frac[i] <= season & season < frac[i+1])  #bin the data into fraction classes
    bin[i] <- mean(x[index])   #calculate the means of the bins
    diff[index] <- bin[i]
  }
    x - diff + mean(x)     #subtract seasonal means and add overall mean
}


