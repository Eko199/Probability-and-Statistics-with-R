# Problem 32

x <- runif(500, 3, 5)
hist(x, probability=T)
curve(dunif(x, 3, 5), add=T, from=0, to=max(x), lwd=2)
x <- runif(5000, 3, 5)
hist(x, probability=T)
curve(dunif(x, 3, 5), add=T, from=0, to=max(x), lwd=2)

# Problem 33
x <- rexp(500, 1/7)
hist(x, probability='T')
x <- rexp(5000, 1/7)
hist(x, probabilit=T)
curve(dexp(x, 1/7), add=T, from=0, to=max(x), lwd=2)

# Problem 34
x <- rnorm(500, 0, 1)
hist(x, probability = T)
curve(dnorm(x, 0, 1), add=T, lwd=2)
x <- rnorm(5000, 0, 1)
hist(x, probability=T)
curve(dnorm(x, 0, 1), add=T, lwd=2)

# Problem 35
n <- 200
x <- runif(n, 7, 9)
plot.ecdf(x, do.points=F)
curve(punif(x, 7, 9), add=T, col="red")
n <- 1000
x <- runif(n, 7, 9)
plot.ecdf(x)
curve(punif(x, 7, 9), add=T, col="red")

# Problem 36
n <- 200
x <- rexp(n, 3)
plot.ecdf(x, do.points=F)
curve(pexp(x, 3), add=T, col="red")
n <- 1000
x <- rexp(n, 3)
plot.ecdf(x, do.points=F)
curve(pexp(x, 3), add=T, col="red")

# Problem 37
n <- 200
x <- rnorm(n, 4, 1.2)
plot.ecdf(x, do.points=F)
curve(pnorm(x, 4, 1.2), add=T, col="red")
n <- 1000
x <- rnorm(n, 4, 1.2)
plot.ecdf(x, do.points=F)
curve(pnorm(x, 4, 1.2), add=T, col="red")

# Problem 38
x <- runif(1000, 7, 9)
par(mfrow=c(1,3))
curve(dunif(x, 7, 9), from=6, to=10)
curve(punif(x, 7, 9), from=6, to=10)
curve(qunif(x, 7, 9), from=0, to=1)
par(mfrow=c(1,1))

# Problem 39
x <- rexp(1000, 3)
par(mfrow=c(1,3))
curve(dexp(x, 3), from=0)
curve(pexp(x, 3), from=0)
curve(qexp(x, 3), from=0)
par(mfrow=c(1,1))

# Problem 40
x <- rnorm(1000, 4, 1.2)
par(mfrow=c(1,3))
curve(dnorm(x, 4, 1.2), from=0, to=8)
curve(pnorm(x, 4, 1.2), from=0, to=8)
curve(qnorm(x, 4, 1.2), from=0, to=1)
par(mfrow=c(1,1))

# Problem 41
# P(X > 500)
1 - punif(500, 495, 502)
#v=? P(x>v) = 0.8 => P(x<=v) = 0.2
495 + (502-495)*0.2
qunif(0.2, 495, 502)

# Problem 42
1 - pexp(3, 1/4)
pexp(2, 1/4)
(1 - pexp(6, 1/4)) / (1 - pexp(3, 1/4))
qexp(0.9, 1/4)

# Problem 43
1 - pnorm(51, 41, 5)
pnorm(50, 41, 5) - pnorm(45, 41, 5)
qnorm(0.99, 41, 5)

# Problem 44
#n <- 100
#x <- rnorm(n, -1, 1)
#y <- rnorm(n, -1, 1)