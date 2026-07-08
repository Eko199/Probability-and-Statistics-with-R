# Problem 3

frogWalk <- function() {
  steps <- 0
  curr <- 1
  moves <- c(-1, 1)
  visited <- c(T, F, F, F, F)
  
  while (any(!visited)) {
    move <- sample(moves, 1)
    curr <- curr + move
    
    if (curr == 0) {
      curr <- 5
    }
    
    if (curr == 6) {
      curr <- 1
    }
    
    visited[curr] <- T
    steps <- steps + 1
  }
  
  steps
}

result <- replicate(10^3, frogWalk())
mean(result)

result <- replicate(10^4, frogWalk())
mean(result)

result <- replicate(10^5, frogWalk())
mean(result)

hist(result)

# Problem 7
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

