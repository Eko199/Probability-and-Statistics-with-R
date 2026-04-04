# Problem 44
# piR^2/(4R^2) = pi / 4
n <- 10^4
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)

4 * sum(x^2 + y^2 < 1) / n

plot(x, y, pch=".", col="gray")
curve(sqrt(1-x^2), from=-1, to=1, add=T, col="red")
curve(-sqrt(1-x^2), from=-1, to=1, add=T, col="red")

# Problem 45
n <- 10^7

f <- function(x) {
  exp(-x^2/2)/sqrt(2*pi)
}

x <- runif(n, 0.8, 4)
y <- runif(n, 0, f(0.8))

f(0.8) * (4-0.8) * sum(y < f(x)) / n

plot(x, y, pch=".", col="gray")
curve(f(x), from=0.8, to=4, add=T, col="red")

pnorm(4) - pnorm(0.8)

# Problem 46
x <- rexp(5000, 1/8)
hist(x)

y <- 1 - exp(-x/8)
hist(y)

# Problem 47
x <- runif(5000, 0, 1)
hist(x)

y <- (-1/(1/8)) * log(1 - x)
hist(y, prob=T)
curve(dexp(x, 1/8), add=T, col="red")

# Problem 48
1 - pnorm(240, 260, 50)
pnorm(300, 260, 50) - pnorm(180, 260, 50)
qnorm(0.1, 260, 50)

# Problem 49
library(MASS)
data(survey)
table(survey$Exer)
attach(survey)
table(Exer)
sort(table(Exer), decreasing=T)
100*table(Exer) / length(Exer)

barplot(table(Exer))
barplot(sort(table(Exer), decreasing=T))
barplot(100*table(Exer) / length(Exer))

pie(table(Exer), col=c("red", "yellow", "blue"))

# Problem 50
table(Pulse, useNA="ifany")
barplot(table(Pulse, useNA="ifany"))

pulse.interval <- cut(Pulse, breaks=seq(30,110,10))
pulse.interval
table(pulse.interval)
barplot(table(pulse.interval))
hist(Pulse)
hist(Pulse, breaks=seq(30,110,5))
stripchart(Pulse, method="stack", pch="^")

# Problem 51
table(Age)
barplot(table(Age))
pie(table(Age))

age.interval <- cut(Age, breaks=seq(15,75,10))
table(age.interval)
barplot(table(age.interval))
hist(Age)
stripchart(Age, method="stack", pch="*")

# Problem 52
v1 <- rep(4, 30)
v2 <- rep(c(3.5,4.5), 15)
v3 <- rep(c(3,5), 15)
v4 <- rep(c(2:6), 6)
v5 <- rep(c(2,6), 15)

hist(v1)
mean(v1)
sd(v1)

hist(v2)
mean(v2)
sd(v2)

hist(v3)
mean(v3)
sd(v3)

hist(v4)
mean(v4)
sd(v4)

hist(v5)
mean(v5)
sd(v5)

# Problem 53
getwd()
load("cereals.RData")
attach(cereals)

hist(carbo)
mean(carbo, na.rm=T)
median(carbo, na.rm=T)
sd(carbo, na.rm=T)

hist(sodium)
mean(sodium, na.rm=T)
median(sodium, na.rm=T)
sd(sodium, na.rm=T)

hist(potass)
boxplot(potass, horizontal=T)
mean(potass, na.rm=T)
median(potass, na.rm=T)
sd(potass, na.rm=T)

# Problem 54
library(MASS)
attach(survey)

mean(Pulse[W.Hnd=="Left"], na.rm=T)
mean(Pulse[W.Hnd=="Right"], na.rm=T)

median(Pulse[W.Hnd=="Left"], na.rm=T)
median(Pulse[W.Hnd=="Right"], na.rm=T)

# Problem 55
my.summary <- function(x) {
  res <- c(median(x, na.rm=T), mean(x, na.rm=T), sd(x, na.rm=T))
  names(res) <- c("Median", "Mean", "Standart deviation")
  res
}

my.summary(Pulse)
my.summary(Pulse[Sex=="Female"])
my.summary(Pulse[Age <= 25])
my.summary(Pulse[Exer=="Freq"])
my.summary(Pulse[Exer=="Freq" & Smoke=="Never"])

# Problem 56
p_freq <- mean(Pulse[Exer=="Freq"], na.rm=T)
p_none <- mean(Pulse[Exer=="None"], na.rm=T)
p_some <- mean(Pulse[Exer=="Some"], na.rm=T)

barplot(c(p_none, p_some, p_freq), names.arg = c("None", "Some", "Freq"))

# Problem 57
plot(Smoke)

# Problem 58
my.summary <- function(x) {
  res <- c(median(x, na.rm=T), mean(x, na.rm=T), sd(x, na.rm=T))
  names(res) <- c("Median", "Mean", "Standart deviation")
  res
}

my.summary(Age)
my.summary(Age[Smoke!="Never"])
my.summary(Age[W.Hnd=="Right"])
my.summary(Age[Pulse >= 70])
my.summary(Age[Exer=="None"])

# Problem 59
barplot(table(cut(Height, breaks=(seq(140, 200, 5)))))
barplot(table(Sex, cut(Height, breaks=(seq(140, 200, 10)))), beside=T, legend.text=T)
