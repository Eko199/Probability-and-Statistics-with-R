x <- c(1,2,3,4)
x+3
x^2

seq(20, 2, -2)
rep(5, 10)

sample(c(1:10), 3, replace=T)

# Problem 1

sim.zad1 <- function() {
  x <- sample(c(1:8), 2, replace=T)
  x[1] == x[2]
}

sim.zad1()
res <- replicate(300000, sim.zad1())
sum(res) / length(res)

# Problem 2

sim.zad2 <- function() {
  socks <- c(1,1,2,2,3,3)
  x <- sample(socks, 2, replace=F)
  x[1] == x[2]
}

res <- replicate(300000, sim.zad2())
sum(res) / length(res)

# Problem 3

sim.zad3 <- function() {
  keys <- c(1,2,3,4)
  x <- sample(keys, 4, replace=F)
  x[4] == 1
}

res <- replicate(100000, sim.zad3())
sum(res) / length(res)


# Problem 4

sim.zad4 <- function() {
  conspect <- c(rep(0,3), rep(1,17))
  x <- sample(conspect, 2, replace=F)
  sum(x) == 1
}

res <- replicate(100000, sim.zad4())
sum(res) / length(res)


# Problem 5

sim.zad5 <- function() {
  dates <- c(1:365)
  x <- sample(dates, 25, replace=T)
  any(duplicated(x))
}

res <- replicate(100000, sim.zad5())
sum(res) / length(res)

# Problem 6

sim.zad6 <- function() {
  names <- c(1:20)
  x <- sample(names, 20, replace=F)
  d <- x - names
  any(d==0)
}

res <- replicate(100000, sim.zad6())
sum(res) / length(res)

# Problem 7

sim.zad7 <- function() {
  m1 <- sample(c(2,3), 1)
  m2 <- sample(c(1,3), 1)
  m3 <- sample(c(1,2), 1)
  m <- c(m1,m2,m3)
  length(unique(m)) == 3
}

res <- replicate(100000, sim.zad7())
sum(res) / length(res)
