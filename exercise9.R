# Problem 89
# P(-z(a/2) <= (X_ - mu)/(sig/sqrt(n)) <= z(a/2)) = 1 - a
# P(-z(a/2)*sig/sqrt(n) <= X_-mu <= z(a/2)*sig/sqrt(n))
# P(X_-z(a/2)*sig/sqrt(n) <= mu <= X_ + z(a/2)*sig/sqrt(n))

z1.ci <- function(xbar, sigma, n, alpha) {
  b1 <- xbar - qnorm(1-alpha/2)*sigma/sqrt(n)
  b2 <- xbar + qnorm(1-alpha/2)*sigma/sqrt(n)
  c(b1, b2)
}

z1.ci(6.73, 0.12, 45, 0.05)
z1.ci(6.76, 0.12, 45, 0.05)

# Problem 90
t1.ci <- function(xbar, s, n, alpha) {
  b1 <- xbar - qt(1-alpha/2, n-1)*s/sqrt(n)
  b2 <- xbar + qt(1-alpha/2, n-1)*s/sqrt(n)
  c(b1, b2)
}

x <- c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)

t1.ci(mean(x), sd(x), length(x), 0.05)
t1.ci(mean(x), sd(x), length(x), 0.1)

t.test(x, conf.level = 0.95)$conf.int[1:2]
t.test(x, conf.level = 0.9)$conf.int[1:2]

# Problem 91
# P(-z(a/2) <= (X/n - p)/sqrt(p(1-p)/n) <= z(a/2)) = 1-a
# P(-z(a/2)*sqrt(p(1-p)/n) <= phat - p <= z(a/2)*sqrt(p(1-p)/n))
# P(phat-z(a/2)*sqrt(phat(1-phat)/n) <= p <= phat + z(a/2)*sqrt(phat(1-phat)/n))

prop1.ci <- function(x, n, alpha) {
  phat <- x/n
  b1 <- phat - qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)
  b2 <- phat + qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)
  c(b1, b2)
}

prop1.ci(58, 100, 0.05)
prop1.ci(116, 200, 0.05)
prop1.ci(61, 100, 0.05)

# Problem 92
z1.ci(61.9, 4.1, 66, 0.05)
z1.ci(61.9, 4.1, 88, 0.05)

# Problem 93
prop1.ci(32, 58, 0.05)

# Problem 94
prob94 <- function(n) {
  x <- runif(n, 5, 9)
  t1.ci(mean(x), sd(x), n, 0.05)
}

ints <- replicate(10000, prob94(20))
sum(ints[1,] <= 7 & 7 <= ints[2,]) / 10000

ints <- replicate(10000, prob94(50))
sum(ints[1,] <= 7 & 7 <= ints[2,]) / 10000

ints <- replicate(10000, prob94(100))
sum(ints[1,] <= 7 & 7 <= ints[2,]) / 10000

ints <- replicate(10000, prob94(500))
sum(ints[1,] <= 7 & 7 <= ints[2,]) / 10000

# Problem 95
prob95 <- function(n) {
  x <- runif(n, 5, 9)
  t.test(x, mu=7)$p.value > 0.05
}

res <- replicate(10000, prob95(20))
sum(res) / 10000

ints <- replicate(10000, prob95(50))
sum(res) / 10000

ints <- replicate(10000, prob95(100))
sum(res) / 10000

ints <- replicate(10000, prob95(500))
sum(res) / 10000

# Problem 96
prob96 <- function(n) {
  x <- runif(n, 5, 9)
  int <- t1.ci(mean(x), sd(x), n, 0.05)
  (t.test(x, mu=7)$p.value > 0.05) & int[1] <= 7 & 7 <= int[2]
}

res <- replicate(10000, prob96(20))
sum(res) / 10000

ints <- replicate(10000, prob96(50))
sum(res) / 10000

ints <- replicate(10000, prob96(100))
sum(res) / 10000

ints <- replicate(10000, prob96(500))
sum(res) / 10000

# Problem 97
left <- 25.0128
right <- 26.0212
xbar <- (left + right) / 2
n <- 50
alpha <- 0.05
s <- (xbar - left) / qt(1-alpha/2, n-1) * sqrt(n)
tobs <- (xbar - 25) / (s / sqrt(n))
pvalue <- 2 * (1 - pt(tobs, n-1))
pvalue < 0.05
# makes sense, 25 is outside the confidence interval