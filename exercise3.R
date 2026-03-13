# Problem 18
# X = #6 при n хвърляния (бернулиеви опити)
# X ~ Bi(n, p) (геометрично разпределение)
# P(X = k) = dbinom(k, n, p) = choose(n, k)*p^k*(1-p)^(n-k)
# P(X <= k) = pbinom(k - 1, p)

# P(X = 2)
dbinom(2, 10, 1/6)
choose(10, 2)*(1/6)^2*(5/6)^8

# P(X <= 2)
pbinom(2, 10, 1/6)
dbinom(0, 10, 1/6) + dbinom(1, 10, 1/6) + dbinom(2, 10, 1/6)

# P(X >= 2) = 1 - P(X < 2)
1 - dbinom(0, 10, 1/6) - dbinom(1, 10, 1/6)

# P(3 <= X <= 8) = P(X <= 8) - P(X <= 2)
dbinom(3, 10, 1/6) + dbinom(4, 10, 1/6) +
  dbinom(5, 10, 1/6) + dbinom(6, 10, 1/6) +
  dbinom(7, 10, 1/6) + dbinom(8, 10, 1/6)
pbinom(8, 10, 1/6) - pbinom(2, 10, 1/6)
sum(dbinom(3:8, 10, 1/6))

# Problem 19
# X = #хвърляния на зар до 6
# X ~ Gk(p) (геометрично разпределение)
# P(X = k) = dgeom(k - 1, p) = (1-p)^(k-1) * p
# P(X <= k) = pgeom(k - 1, p)

# P(X <= 10)
pgeom(10 - 1, 1/6)

# P(X >= 6)
1 - pgeom(5 - 1, 1/6)

# Problem 20
# X = #опити до r-ти успех (вкл.)
# X ~ NB(r, p) (отрицателно биномно разпределение)
# P(X = k) = dnbinom(k-r, r, p)
# P(X <= k) = pnbinom(k-r, r, p)

# P(X <= 20)
pnbinom(20 - 3, 3, 1/6)

# Problem 21
# X = брой работещи батерии от избраните
# хипергеометрично разпределение
# P(X < 2)
dhyper(0, 5, 3, 2) + dhyper(1, 5, 3, 2)
1 - dhyper(2, 5, 3, 2)

# Problem 22
pbinom(2, 1500, 1/500)
ppois(2, 1500/500)

sum(dbinom(1:3, 1500, 1/500))
sum(dpois(1:3, 1500/500))

# Problem 23
# X ~ Bi(10, 1/4)
# P(X >= 5) = 1 - P(X <= 4)
1 - pbinom(4, 10, 1/4)

# Problem 24
# X ~ Bi(143, 0.92)
# a) P(X <= 138)
pbinom(138, 143, 0.92)

# b) P(X = 137)
dbinom(137, 143, 0.92)

# Problem 25
# X ~ Gk(0.03)
# a) P(X <= 10)
pgeom(10-1, 0.03)

# Y ~ Bi(50, 0.03)
# b) P(Y >= 2)
1 - pbinom(1, 50, 0.03)

# Problem 26
# P(X >= 2)
1 - phyper(1, 3, 97, 50)

# Problem 27
# P(X >= 2)
1 - phyper(1, 90, 2910, 50)