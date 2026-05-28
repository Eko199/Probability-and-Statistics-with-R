fx <- function (x) {
  2*exp(-x^2/2)/sqrt(2*pi)
}

fy <- function(y) {
  exp(-y)
}

c <- sqrt(2*exp(1)/pi)

genNorm <- function() {
  while (1) {
    y <- -log(runif(1))
    u <- runif(1)
    
    if (u <= fx(y)/(c*fy(y))) {
      break
    }
  }
  
  rand <- runif(1, -1, 1)
  
  if (rand > 0) {
    y
  } else {
    -y
  }
}

replicate(10^5, genNorm())

