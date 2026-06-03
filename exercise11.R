# Problem 107
x <- rnorm(100, 5, 1)

# a)
y <- 2 * x
plot(x, y)
cor(x, y)

# b)
y <- 2 * x + rnorm(100)
plot(x, y)
cor(x, y)

# c)
y <- 2 * x + rnorm(100, 0, 2)
plot(x, y)
cor(x, y)

# d)
y <- 0.1 * x + rnorm(100, 0, 2)
plot(x, y)
cor(x, y)

# e)
y <- rnorm(100, 5, 1)
plot(x, y)
cor(x, y)

# f)
y <- -2 * x + rnorm(100)
plot(x, y)
cor(x, y)

# Problem 108
alco <- read.table("bac.txt", header=T)

model <- lm(bac ~ beers, data=alco)
# regression equation: yhat = 0.01796x - 0.0127
plot(bac ~ beers, data=alco)
abline(coef(model), lwd = 2)

# +1 beer -> +0.01796 bac
# 0 beers -> 0.0127 bac

summary(model)$coefficients
summary(model)$coefficients[2,4] < 0.05

confint(model)
#yes: 0.02 is in [0.01281262, 0.02311490]

predict(model, data.frame(beers=c(5)), interval="confidence")

frame <- data.frame(beers=c(1:9))
cint <- predict(model, frame, interval="confidence")
pint <- predict(model, frame, interval="prediction")

plot(bac ~ beers, data=alco)
abline(coef(model), lwd = 2)
lines(frame$beers, cint[, 2], type="l", lty="dashed", col="blue", lwd=2)
lines(frame$beers, cint[, 3], type="l", lty="dashed", col="blue", lwd=2)
lines(frame$beers, pint[, 2], type="l", lty="dashed", col="gray", lwd=2)
lines(frame$beers, pint[, 3], type="l", lty="dashed", col="gray", lwd=2)

# Problem 109
x <- runif(50, 1, 7)


e <- rnorm(50, 0, 2)
y <- 2 + 1.5*x + e
model <- lm(y ~ x)
plot(x, y)
abline(coef(model))
summary(model)$r.squared
confint(model)[2,]

e <- rnorm(50)
y <- 2 + 1.5*x + e
model <- lm(y ~ x)
plot(x, y)
abline(coef(model))
summary(model)$r.squared
confint(model)[2,]

e <- rnorm(50)
y <- 2 + 0.17*x + e
model <- lm(y ~ x)
plot(x, y)
abline(coef(model))
summary(model)$r.squared
confint(model)[2,]

# Problem 110
x <- runif(50, 1, 7)
e <- rnorm(50, 0, 2.5)
y <- 2 + 1.1*x^2 + e

model <- lm(y ~ x)
plot(x, y)
abline(coef(model))
summary(model)$r.squared

# Problem 111
sat <- read.table("satgpa.txt", header=T)

# a)
m <- lm(fy_gpa ~ hs_gpa, data=sat)
# regression equation: y = 0.09132 + 0.74314*x
plot(fy_gpa ~ hs_gpa, data=sat)
abline(coef(m))

# b)
pvalue <- summary(m)$coefficients[2,4]
pvalue < 0.05

# c)
predict(m, data.frame(hs_gpa = c(3.5)), interval="confidence")
predict(m, data.frame(hs_gpa = c(3.5)), interval="prediction")

# d)
m2 <- lm(fy_gpa ~ hs_gpa + sat_sum, data=sat)
m3 <- lm(fy_gpa ~ hs_gpa + sat_v, data=sat)
m4 <- lm(fy_gpa ~ hs_gpa + sat_m, data=sat)
m5 <- lm(fy_gpa ~ hs_gpa + sex, data=sat)
m6 <- lm(fy_gpa ~ hs_gpa + sat_v + sat_m, data=sat)
m7 <- lm(fy_gpa ~ hs_gpa + sat_sum + sat_v + sat_m, data=sat)
m8 <- lm(fy_gpa ~ hs_gpa + sat_sum + sat_v + sat_m + sex, data=sat)

summary(m)$r.squared
summary(m2)$r.squared
summary(m3)$r.squared
summary(m4)$r.squared
summary(m5)$r.squared
summary(m6)$r.squared
summary(m7)$r.squared
summary(m8)$r.squared
#yes

# Problem 112
cherry <- read.table("cherry.txt", header=T)

# a)
m <- lm(volume ~ diam, data=cherry)
# regression equation: v = -36.943 + 5.066*d
plot(volume ~ diam, data=cherry)
abline(coef(m))

# b) +1inch => +5.066 feet^3

# c)
m <- lm(volume ~ diam + height, data=cherry)
# +1inch diameter => +4.7082 feet^3
# +1foot height => + 0.3393 feet^3

# d)
predict(m, data.frame(diam=c(14), height=c(70)), interval="confidence")

# e)
cherry$diam2 <- cherry$diam^2
m2 <- lm(volume ~ diam2 + height, data=cherry)
predict(m2, data.frame(diam2=c(14^2), height=c(70)), interval="confidence")

# Problem 113
duke <- read.csv("duke_forest.csv", header=T)
pairs( duke[,c(2,3,4,5,7,11)], gap=0, cex.labels=0.9 )

m1 <- lm(price ~ bed + bath + area + year_built + lot, data=duke)
summary(m1)$r.squared
summary(m1)$coefficients

m2 <- lm(price ~ bath + area + year_built + lot, data=duke)
summary(m2)$r.squared
summary(m2)$coefficients

m3 <- lm(price ~ bath + area + lot, data=duke)
summary(m3)$r.squared
summary(m3)$coefficients

cor( duke[,c(3,4,5)], use="complete.obs" )

# Problem 114
prob114 <- function(n) {
  x <- runif(n, 1, 10)
  e1 <- rnorm(n, 0, 5)
  e2 <- rexp(n, 1/5)
  
  y1 <- 2 + 5*x + e1
  y2 <- 2 + 5*x + e2
  
  m1 <- lm(y1 ~ x)
  m2 <- lm(y2 ~ x)
  
  c(coef(m1)[2], coef(m2)[2])
}

result <- replicate(10000, prob114(30))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114(50))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114(100))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114(500))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

prob114.2 <- function(n) {
  x <- runif(n, 1, 10)
  e1 <- rnorm(n, 0, 5)
  e2 <- rexp(n, 1/5)
  
  y1 <- 2 + 5*x + e1
  y2 <- 2 + 5*x + e2
  
  m1 <- lm(y1 ~ x)
  m2 <- lm(y2 ~ x)
  
  res1 <- confint(m1)[2,1] <= 5 & 5 <= confint(m1)[2,2]
  res2 <- confint(m2)[2,1] <= 5 & 5 <= confint(m2)[2,2]
  
  c(res1, res2)
}

result <- replicate(10000, prob114.2(30))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114.2(50))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114.2(100))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114.2(500))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

prob114.3 <- function(n) {
  x <- runif(n, 1, 10)
  e1 <- rnorm(n, 0, 5)
  e2 <- rexp(n, 1/5)
  
  y1 <- 2 + 5*x + e1
  y2 <- 2 + 5*x + e2
  
  m1 <- lm(y1 ~ x)
  m2 <- lm(y2 ~ x)
  
  res1 <- confint(m1)[2,2] - confint(m1)[2,1]
  res2 <- confint(m2)[2,2] - confint(m2)[2,1]
  
  c(res1, res2)
}

result <- replicate(10000, prob114.3(30))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114.3(50))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114.3(100))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])

result <- replicate(10000, prob114.3(500))
sum(result[1,])/length(result[1,])
sum(result[2,])/length(result[2,])
