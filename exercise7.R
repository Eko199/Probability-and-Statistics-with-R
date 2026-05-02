# Problem 70
# H1: p = 1/2
# H2: p > 1/2
# Z = (X/n-p)/sqrt(p(1-p)/n) ~ N(0,1) for big n
# pvalue := P(Z >= z(58/100))
# pvalue <= alpha = 0.05 -> H1
# pvalue > alpha = 0.05 -> H0

x <- 58
n <- 100
p <- 1/2
zobs <- (x/n - p)/sqrt(p*(1-p)/n)
pvalue <- 1 - pnorm(zobs)
pvalue <= 0.05

# Problem 71

x <- 61
n <- 100
p <- 1/2
zobs <- (x/n - p)/sqrt(p*(1-p)/n)
pvalue <- 1 - pnorm(zobs)
pvalue <= 0.05

# Problem 72
# mu = EXi
# sigma = sqrt(DXi)
# H0: mu = 6.7
# H1: mu != 6.7
# X_ = (X1 + ... + Xn)/n
# Z = (X_ - mu)/(sigma/sqrt(n)) ~ N(0,1)
# pvalue = P(Z <= -|zobs|) + P(Z >= |zobs|) = 2P(Z >= |zobs|)
# pvalue <= alpha = 0.025 -> H1
# pvalue > alpha = 0.025 -> H0

mu = 6.7
sigma = 0.12
n = 45
zobs = (6.73 - mu)/(sigma/sqrt(n))
pvalue = 2*(1 - pnorm(abs(zobs)))
pvalue <= 0.025

# Problem 73

mu = 6.7
sigma = 0.12
n = 45
zobs = (6.76 - mu)/(sigma/sqrt(n))
pvalue = 2*(1 - pnorm(abs(zobs)))
pvalue <= 0.025

# Problem 74
# mu = EXi
# H0: mu = u
# H1: mu < u
# S = sqrt((sum(xi-x_)^2)/(n-1))
# Т = (X_ - mu)/(S/sqrt(n)) ~ t-разпределение с (n-1) сетепени на свобода
# pvalue = P(T <= tobs) = pt(tobs, df = n-1)

x <- c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)
x_ <- mean(x)
s <- sd(x)
n <- length(x)
mu <- 4

tobs = (x_ - mu)/(s/sqrt(n))
pvalue = pt(tobs, n-1)
pvalue <= 0.05

t.test(x, mu=4, alternative="less")

# Problem 75

x <- 32
n <- 58
p <- 0.51
zobs <- (x/n - p)/sqrt(p*(1-p)/n)
pvalue <- 1 - pnorm(zobs)
pvalue <= 0.05

# Problem 76

x <- c(12.3, 11.2, 14.2, 15.3, 14.8, 13.5, 11.1, 15.1, 15.4, 13.2)
x_ <- mean(x)
s <- sd(x)
n <- length(x)
mu <- 14.6

tobs <- (x_ - mu)/(s/sqrt(n))
pvalue <- 2*pt(tobs, n-1)
pvalue <= 0.05

t.test(x, mu=14.6)

pvalue <- pt(tobs, n-1)
pvalue <= 0.05

t.test(x, mu=14.6, alternative="less")

# Problem 77
p <- 7.5/100
n <- 200
x <- 14
zobs <- (x/n - p)/sqrt(p*(1-p)/n)

pvalue <- 2*(1 - pnorm(abs(zobs)))
pvalue <= 0.05
prop.test(x, n, p, correct=F)$p.value

pvalue <- pnorm(zobs)
pvalue <= 0.05
prop.test(x, n, p, correct=F, alternative="less")

# Problem 78
n <- 66
x <- 61.9
sigma <- 4.1
mu <- 60

zobs <- (x-mu)/(sigma/sqrt(n))
pvalue <- 2*(1-pnorm(abs(zobs)))
pvalue <= 0.05

# Problem 79
n <- 50
x <- 168
sigma <- 3.9
mu <- 170
zobs <- (x-mu)/(sigma/sqrt(n))
pvalue <- pnorm(zobs)
pvalue <= 0.05