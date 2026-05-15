# Problem 98

x <- c(28, 36, 36, 30, 27, 23)
probs <- rep(1/6, 6)
n <- sum(x)

chi2.obs <- sum((x-n*probs)^2/(n*probs))
chi2.obs

pvalue <- 1 - pchisq(chi2.obs, df=length(x) - 1)
pvalue

pvalue > 0.05

chisq.test(x=x, p=probs)

# Problem 99
x <- c(221, 153, 183, 111, 113, 152, 103, 197, 38, 104, 629)
probs <- c(12.02, 9.10, 8.12, 7.68, 7.31, 6.95, 6.28, 6.02, 5.92, 4.32, 26.28) / 100
n <- sum(x)

chi2obs <- sum((x-n*probs)^2/(n*probs))
pvalue <- 1 - pchisq(chi2obs, length(x) - 1)
pvalue >= 0.05

chisq.test(x=x, p=probs)

# Problem 100
probs <- c(1/4, 1/2, 1/4)
x <- c(141, 291, 132)
n <- 564

chi2obs <- sum((x-n*probs)^2/(n*probs))
pvalue <- 1 - pchisq(chi2obs, df=length(x)-1)
pvalue >= 0.05

chisq.test(x=x, p=probs)

# Problem 101
load("pi2000.RData")
x <- c(sum(pi2000 == 0), sum(pi2000 == 1), sum(pi2000 == 2),
       sum(pi2000 == 3), sum(pi2000 == 4), sum(pi2000 == 5),
       sum(pi2000 == 6), sum(pi2000 == 7), sum(pi2000 == 8), sum(pi2000 == 9))
probs <- rep(1/10, 10)
n <- 2000

chi2obs <- sum((x - n*probs)^2/(n*probs))
pvalue <- 1 - pchisq(chi2obs, 10-1)
pvalue >= 0.05

chisq.test(x=x,p=probs)

# Problem 102
library(MASS)
attach(survey)

x <- table(Smoke, Sex)
smoke <- apply(x, 1, sum)
gender <- apply(x, 2, sum)
n <- sum(x)

chi2obs <- sum((x - (smoke %o% gender)/n)^2/((smoke %o% gender)/n))
pvalue <- 1 - pchisq(chi2obs, df=(nrow(x) - 1) * (ncol(x) - 1))
pvalue >= 0.05

chisq.test(x)

#Problem 103

data <- read.table("ManWomanEye.txt", header=T)

x <- table(data$man, data$woman)
n <- sum(x)
man <- apply(x, 1, sum)
woman <- apply(x, 2, sum)

chi2obs <- sum((x-(man %o% woman)/n)^2/((man %o% woman)/n))
pvalue <- 1 - pchisq(chi2obs, (nrow(x)-1)*(ncol(x)-1))
pvalue < 0.05

chisq.test(x)

# Problem 104
data(HairEyeColor)
x <- HairEyeColor[,,1] + HairEyeColor[,,2]
hair <- apply(x, 1, sum)
eye <- apply(x, 2, sum)
n <- sum(x)

chi2obs <- sum((x-(hair %o% eye)/n)^2/((hair %o% eye)/n))
pvalue <- 1 - pchisq(chi2obs, (nrow(x) - 1)*(ncol(x) - 1))
pvalue < 0.05

chisq.test(x)

# Problem 105
x <- matrix(c(12813, 647, 359, 42, 65963, 4000, 2642, 303), nrow = 2, byrow=T)
rownames(x) <- c("С колан", "Без колан")
colnames(x) <- c("Няма", "Леки", "Средни", "Тежки")

n <- sum(x)
belt <- apply(x, 1, sum)
hurt <- apply(x, 2, sum)

chi2obs <- sum((x-(belt %o% hurt)/n)^2/((belt %o% hurt)/n))
pvalue <- 1 - pchisq(chi2obs, (nrow(x)-1)*(ncol(x)-1))
pvalue < 0.05

chisq.test(x)

# Problem 106
sim <- function(n) {
  dice <- sample(c(1:6), n, replace=T)
  x <- c(sum(dice == 1), sum(dice == 2), sum(dice == 3), 
         sum(dice == 4), sum(dice == 5), sum(dice == 6))
  chisq.test(x=x,p=rep(1/6,6))$p.value > 0.05
}

result <- replicate(10000, sim(100))
sum(result)/length(result)
result <- replicate(10000, sim(200))
sum(result)/length(result)
result <- replicate(10000, sim(400))
sum(result)/length(result)