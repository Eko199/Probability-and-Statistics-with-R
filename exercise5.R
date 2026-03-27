# Problem 44
# piR^2/(4R^2) = pi / 4
n <- 10^7
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)

4 * sum(x^2 + y^2 < 1) / n

plot(x, y, pch=".", col="gray")
curve(sqrt(1-x^2), from=-1, to=1, add=T, col="red")
curve(-sqrt(1-x^2), from=-1, to=1, add=T, col="red")

# Problem 45
# piR^2/(4R^2) = pi / 4
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

#...
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
