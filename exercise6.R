# Problem 60
for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate(1000, sum(rexp(n, 1/5)))
  hist(xsum, main=paste("n = ", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate(1000, sum(rexp(n, 1/5)))
  plot.ecdf(xsum, dо.points=FALSE, main=paste("n = ", n))
  curve(pnorm(x, 5*n, 5*sqrt(n)), add=T, col="red", lty="longdash")
}

# Problem 61
for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate(1000, sum(rexp(n, 1/5))/n)
  hist(xsum, main=paste("n = ", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate(1000, sum(rexp(n, 1/5))/n)
  plot.ecdf(xmean, dо.points=FALSE, main=paste("n = ", n))
  curve(pnorm(x, 5, 5/sqrt(n)), add=T, col="red", lty="longdash")
}

# Problem 62
for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate(1000, sum(rpois(n, 3)))
  hist(xsum, main=paste("n = ", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xsum <- replicate(1000, sum(rpois(n, 3)))
  plot.ecdf(xsum, dо.points=FALSE, main=paste("n = ", n))
  curve(pnorm(x, 3*n, sqrt(3*n)), add=T, col="red", lty="longdash")
}

# Problem 63
for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate(1000, sum(rpois(n, 3))/n)
  hist(xsum, main=paste("n = ", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate(1000, sum(rpois(n, 3))/n)
  plot.ecdf(xmean, dо.points=FALSE, main=paste("n = ", n))
  curve(pnorm(x, 3, sqrt(3)/sqrt(n)), add=T, col="red", lty="longdash")
}

# Problem 64
for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate(1000, sum(runif(n, 2, 8))/n)
  hist(xsum, main=paste("n = ", n))
}

for (n in c(3, 7, 10, 30, 90, 200)) {
  xmean <- replicate(1000, sum(runif(n, 2, 8))/n)
  plot.ecdf(xmean, dо.points=FALSE, main=paste("n = ", n))
  curve(pnorm(x, 5, sqrt((8-2)^2/12)/sqrt(n)), add=T, col="red", lty="longdash")
}

# Problem 65
a <- (980 - 900)/(900/sqrt(100))
1 - pnorm(a)

# Problem 66
# X ~ U(0, 60)
# X1, ..., X50
# P(25 <= X <= 35) = P(X <= 35) - P(X <= 25)
# P((X-mu)/(sigma/sqrt(n)) <= a) ~ pnorm(a)
# a = (35 - mu) / (sigma/sqrt(n))
# mu = EX = 0integral60(x*1/60) = x^2/120 from 0 to 60 = 30 - 0 = 30
# sigma = sqrt(DX) = (60-0)/sqrt(12) = 30/sqrt(3)
n <- 50
b <- (35 - 30)/(60/(sqrt(12)*sqrt(n)))
a <- (25 - 30)/(60/(sqrt(12)*sqrt(n)))
pnorm(b) - pnorm(a)

mean.vals <- replicate(100000, mean(runif(50, 0, 60)))
sum(mean.vals > 25 & mean.vals < 35) / length(mean.vals)

# Problem 67
x <- c(4:7)
p <- c(0.2, 0.4, 0.3, 0.1)
mu <- sum(x*p)
sigma <- sqrt(sum((x^2)*p) - mu^2)

a <- (5.5 - mu)/(sigma/sqrt(49))
1 - pnorm(a)

sample(x, 49, replace=T, prob=p)

mean.vals <- replicate(100000, mean(sample(x, 49, replace=T, prob=p)))
sum(mean.vals > 5.5)/length(mean.vals)

# Problem 68
a <- (4000 - 160*24) / (7*sqrt(160))
1 - pnorm(a)

# Problem 69
n <- 80
lambda <- 5
mu <- lambda
sigma <- sqrt(lambda)
b = (5.5 - mu)/(sigma/sqrt(n))
a = (4.5 - mu)/(sigma/sqrt(n))
pnorm(b) - pnorm(a)
