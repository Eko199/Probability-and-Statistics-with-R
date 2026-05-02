# Problem 80

getwd()
examAB <- read.table("GitHub/Probability-and-Statistics-with-R/examAB.txt", header=T)
# Xi - оценка на i-ти с вар. А
# Yi - оценка на i-ти с вар. B
# mux = E(Xi)
# muy = E(Yi)
# H0: mux = muy
# H1: mux > muy
# X_ = sum(Xi)/n
# Y_ = sum(Yi)/n
# Sx = sqrt(sum((Xi-X_)^2)/(n-1))
# Sy = sqrt(sum((Yi-Y_)^2)/(m-1))
# T = (X_-Y_-(mux-muy))/sqrt(Sx/n - Sy/m) ~ t(v)
# pvalue = P(T >= tobs)
# pvalue <= 0.05 => H0
# pvalue > 0.05 => H1

x <- examAB$points[examAB$variant == 'A']
y <- examAB$points[examAB$variant == 'B']

t.test(x, y, alternative = "greater")
0.01668 < 0.05 #yes

# Problem 81
#T = (D_ - mud)/(Sd/sqrt(n)) ~ t(n-1)
reacttime <- read.table("GitHub/Probability-and-Statistics-with-R/reacttime.txt", header=T)

x <- reacttime$before
y <- reacttime$after

t.test(x, y, alternative = "less", paired=T)
t.test(x - y, alternative = "less")
3.24e-10 < 0.05 #yes

d <- x-y
d.bar <- mean(d)
t.obs <- d.bar/(sd(d)/sqrt(length(d)))
p.value <- pt(t.obs, df=length(d)-1)

# Problem 82
# X1 - A ~ Bin(200, p1)
# X2 - B ~ Bin(200, p2)
# H0: p1 = p2
# H1: p1 != p2
# P^ := (X1+X2)/(n1+n2)
# Z := (X1/n1-X2/n-(p1-p2))/sqrt((P^)(1-P^)(1/n1+1/n2)) ~ N(0,1)
# pvalue = 2P(Z>=|zobs|)
x1 <- 8
x2 <- 15
n1 <- 200
n2 <- 200
phat <- (x1+x2)/(n1+n2)
zobs <- (x1/n1 - x2/n2)/sqrt(phat*(1-phat)*(1/n1+1/n2))
pvalue <- 2 * (1-pnorm(abs(zobs)))
pvalue <= 0.05 #no

prop.test(x=c(8, 15), n=c(200,200), correct=F)

# Problem 83
# 1) paired
# 2) not paired
# 3) paired
# 4) not paired

# Problem 84
prob84 <- function(n, alpha) {
  x <- rnorm(n, 5, 1)
  y <- rnorm(n, 5, 0.8)
  
  sx <- sd(x)
  sy <- sd(y)
  
  tobs <- (mean(x) - mean(y)) / sqrt(sx^2/n + sy^2/n)
  v <- (sx^2/n + sy^2/n)^2/((sx^2/n)^2/(n-1)+(sy^2/n)^2/(n-1))
  
  pvalue <- 2*(1-pt(abs(tobs), v))
  pvalue <= alpha
}

n <- 10000
result <- replicate(n, prob84(20, 0.05))
mean(result)
result <- replicate(n, prob84(20, 0.1))
mean(result)
result <- replicate(n, prob84(50, 0.05))
mean(result)
result <- replicate(n, prob84(50, 0.1))
mean(result)
result <- replicate(n, prob84(100, 0.05))
mean(result)
result <- replicate(n, prob84(100, 0.1))
mean(result)
result <- replicate(n, prob84(500, 0.05))
mean(result)
result <- replicate(n, prob84(500, 0.1))
mean(result)

# Problem 85
prob85 <- function(n, alpha) {
  x <- rnorm(n, 5, 1)
  y <- rnorm(n, 5.2, 1)
  
  sx <- sd(x)
  sy <- sd(y)
  
  tobs <- (mean(x) - mean(y)) / sqrt(sx^2/n + sy^2/n)
  v <- (sx^2/n + sy^2/n)^2/((sx^2/n)^2/(n-1)+(sy^2/n)^2/(n-1))
  
  pvalue <- 2*(1-pt(abs(tobs), v))
  pvalue <= alpha
}

n <- 10000
result <- replicate(n, prob85(20, 0.05))
mean(result)
result <- replicate(n, prob85(20, 0.1))
mean(result)
result <- replicate(n, prob85(50, 0.05))
mean(result)
result <- replicate(n, prob85(50, 0.1))
mean(result)
result <- replicate(n, prob85(100, 0.05))
mean(result)
result <- replicate(n, prob85(100, 0.1))
mean(result)
result <- replicate(n, prob85(500, 0.05))
mean(result)
result <- replicate(n, prob85(500, 0.1))
mean(result)

# Problem 86
n <- 500
x <- 26
m <- 540
y <- 43

phat <- (x+y)/(n+m)
zobs <- (x/n - y/m)/sqrt(phat*(1-phat)*(1/n+1/m))
pvalue <- pnorm(zobs)
pvalue <= 0.05

# Problem 87
x <- c(1.2, 1.3, 1.5, 1.4, 1.7, 1.8, 1.4, 1.3)
y <- c(1.4, 1.7, 1.5, 1.3, 2.0, 2.1, 1.7, 1.6)

d <- x-y
tobs <- mean(d)/(sd(d)/sqrt(length(d)))
pvalue <- 2*(1-pt(abs(tobs), length(d)-1))
pvalue > 0.05

t.test(x, y, correct=F, paired=T)

# Problem 88
n <- 50
mx <- 7.88
sx <- 1.73
my <- 8.48
sy <- 2.12

tobs <- (mx - my) / sqrt(sx^2/n + sy^2/n)
v <- (sx^2/n + sy^2/n)^2/((sx^2/n)^2/(n-1)+(sy^2/n)^2/(n-1))

pvalue <- 2*(1-pt(abs(tobs), v))
pvalue <= 0.05