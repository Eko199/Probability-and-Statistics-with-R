# Problem 115
# b) 0.9524/2
# c) 1-0.9524/2
# d) (1.1249405 + 0.7998512)/2
# e) yes

# Problem 116
# no

# Problem 117
pbinom(86, 200, 0.5) + 1 - pbinom(114, 200, 0.5)
pbinom(86, 200, 0.61) + 1 - pbinom(114, 200, 0.61)

# Problem 118
x <- c(25, 29, 18, 29, 22, 20, 27, 24, 20, 29, 18, 20, 31, 25, 21, 24, 24, 21, 18, 24, 24, 29, 25, 24, 27, 22, 25, 22, 27, 25)

tobs <- (mean(x)-25)/(sd(x)/sqrt(length(x)))
pvalue <- pt(tobs, length(x)-1)
pvalue >= 0.05

t.test(x, mu=25, alternative="less")

# Problem 119
x <- c(144, 170, 158, 172, 148, 152, 156)
n <- sum(x)

chiobs <- sum((x - n/7)^2/(n/7))
pvalue <- 1 - pchisq(chiobs, length(x)-1)
pvalue >= 0.05

chisq.test(x, p=rep(1/7, 7))

# Problem 120
tomato <- read.table("tomato2.txt", header=T)
n <- length(tomato[,1])

nu <- (sd(tomato[,1])^2/n+sd(tomato[,2])^2/n)^2/((sd(tomato[,1])^2/n)^2/(n-1)+(sd(tomato[,2])^2/n)^2/(n-1))

tobs <- (mean(tomato[,1]) - mean(tomato[,2]))/sqrt(sd(tomato[,1])^2/n+sd(tomato[,2])^2/n)
pvalue <- pt(tobs, nu)
pvalue < 0.05

# Problem 121
phat <- (22+16)/(30+30)
zobs <- (22/30 - 16/30)/sqrt(phat*(1-phat)*(1/30+1/30))
pvalue <- 1 - pnorm(zobs)
pvalue < 0.05

prop.test(c(22, 16), c(30, 30), correct=F, alternative="greater")

# Problem 122
books <- read.table("books.txt", header=T)
x <- books[,2]
y <- books[,3]

d <- x-y
tobs <- mean(d)/(sd(d)/length(d))
pvalue <- 1 - pt(tobs, length(d)-1)
pvalue < 0.05

t.test(x, y, paired=T, alternative="greater")

# Problem 123
x <- 89
n <- 500
p <- 17/100
zobs <- (x/n-p)/sqrt(p*(1-p)/n)
pvalue <- 1 - pnorm(zobs)
pvalue < 0.05

prop.test(x, n, p=p, alternative="greater", correct=F)
