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

# Problem 8

theta <- seq(0, 2 * pi, length.out = 1000)
plot(cos(theta), sin(theta), type = "l", asp = 1)

sim1 <- function() {
  angles <- runif(2, 0, 2*pi)
  
  a.x <- cos(angles[1])
  a.y <- sin(angles[1])
  
  b.x <- cos(angles[2])
  b.y <- sin(angles[2])
  
  segments(a.x, a.y, b.x, b.y, col = adjustcolor("red", alpha.f = 0.02))
  
  chorde <- sqrt((a.x-b.x)^2 + (a.y-b.y)^2)
  chorde
}

n <- 10000

result1 <- replicate(n, sim1())

hist(result1)
mean(result1 > sqrt(3))

plot(cos(theta), sin(theta), type = "l", asp = 1)

sim2 <- function() {
  phi <- runif(1, 0, 2*pi)
  r <- runif(1)
  
  x <- r * cos(phi)
  y <- r * sin(phi)
  
  lambda <- sqrt(1/(x^2+y^2)-1)
  
  segments(x - lambda*y, y + lambda*x, x + lambda*y, y - lambda*x, col = adjustcolor("red", alpha.f = 0.02))
  
  chorde <- 2 * sqrt(1 - r^2)
  chorde
}

result2 <- replicate(n, sim2())

hist(result2)
mean(result2 > sqrt(3))

plot(cos(theta), sin(theta), type = "l", asp = 1)

sim3 <- function() {
  phi <- runif(1, 0, 2*pi)
  r2 <- runif(1)
  
  x <- sqrt(r2) * cos(phi)
  y <- sqrt(r2) * sin(phi)
  
  lambda <- sqrt(1/(x^2+y^2)-1)
  
  segments(x - lambda*y, y + lambda*x, x + lambda*y, y - lambda*x, col = adjustcolor("red", alpha.f = 0.02))
  
  chorde <- 2 * sqrt(1 - r2)
  chorde
}

result3 <- replicate(n, sim3())

hist(result3)
mean(result3 > sqrt(3))

