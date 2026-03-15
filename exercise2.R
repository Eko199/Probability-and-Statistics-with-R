# Problem 8

sim.zad8 <- function() {
  eggs <- c(rep("r", 6), rep("b", 2))
  draws <- sample(eggs, 8, replace=F)
  player1 <- draws[seq(1,7,2)]
  player2 <- draws[seq(2,8,2)]
  b1 <- sum(player1 == "b")
  b2 <- sum(player2 == "b")
  c(b1, b2)
}

N <- 100000
res <- replicate(N, sim.zad8())

#A
(sum(res[1,]==2) + sum(res[2,]==2)) / N
#B
sum(res[1,]==1) / N
#C
sum(res[1,]==2)/ N
#D
sum(res[2,]==2)/ N

# Problem 9

sim.zad9 <- function() {
  ivan <- sample(c(0,1), 10, replace=T, prob=c(0.75, 0.25))
  sum(ivan)
}

N <- 100000
res <- replicate(N, sim.zad9())
sum(res >= 5) / length(res)

# Problem 10

sim.zad10 <- function() {
  passengers <- sample(c(0,1), 143, replace=T, prob=c(0.08, 0.92))
  sum(passengers)
}

N <- 100000
res <- replicate(N, sim.zad10())
sum(res <= 138) / N
sum(res == 137) / N

# Problem 11

sim.zad11 <- function() {
  box1 <- c("g", "g", "r", "r")
  box2 <- c("g", "r", "r", "r", "r")
  dice <- sample(seq(1,6), 1)
  
  if (dice == 6) {
    ball <- sample(box1, 1)
  } else {
    ball <- sample(box2, 1)
  }
  
  c(dice, ball)
}

N <- 100000
res <- replicate(N, sim.zad11())
sum(res[2,] == "g") / N
sum(res[2,] == "g" & res[1,] != 6) / sum(res[2,] == "g")

# Problem 12

t11 <- c(1,1)
t12 <- c(1,2)
t22 <- c(2,2)

sim.zad12 <- function() {
  box <- list(t11, t11, t22, t12, t12)
  coin <- sample(box, 1)[[1]]
  toss <- sample(coin, 1)
  
  c(coin, toss)
}

N <- 100000
res <- replicate(N, sim.zad12())
sum(res[3,] == 1) / N
sum(res[3,] == 1 & res[2,] == 2) / sum(res[3,] == 1)

# Problem 13

sim.zad13 <- function() {
  cards <- c("ww", "bb", "wb")
  card <- sample(cards, 1)
  side <- sample(c(1, 2), 1)
  side <- substr(card, side, side)
  c(side, card)
}

res <- replicate(100000, sim.zad13())
sum(res[2,] == "ww") / sum(res[1,] == "w")

# Problem 14

sim.zad14 <- function() {
  balls <- sample(c(1:99), 4, replace='F')
  balls[1] == max(balls)
}

res <- replicate(100000, sim.zad14())
sum(res) / length(res)

# Problem 15

sim.zad15 <- function() {
  people <- c(0, 0, c(1:18))
  row <- sample(people, 20, replace='F')
  sum(row == c(row[-1], -1))
}

res <- replicate(100000, sim.zad15())
sum(res) / length(res)

# Problem 16

sim.zad16 <- function() {
  deck <- c(1, 1, 1, 1, rep(0, 48))
  hands <- sample(deck, 52, replace='F')
  1 %in% hands[1:13] & 1 %in% hands[14:26] & 1 %in% hands[27:39] & 1 %in% hands[40:52] 
}

res <- replicate(100000, sim.zad16())
sum(res) / length(res)

# Problem 17

sim.zad17 <- function() {
  waiting <- sample(c(2:16), 7, replace='T')
  c(anyDuplicated(waiting) > 0, waiting[1] %in% waiting[-1])
}

res <- replicate(100000, sim.zad17())
sum(res[1,]) / length(res)
sum(res[2,]) / length(res)
