library(dplyr)
#library(rstan)
library(tensor)

ensure.norm <- function (m, dims) stopifnot(sum(abs(apply(m, dims, sum)-1)) < 1e-8)

d <- read.csv("23737467ehdokasvastaukset-18-02.csv", header=T, sep=";")
a <- d %>% select(matches("^q[0-9]+")) %>% as.matrix
ba <- outer(a, min(a):max(a), function (a, b) as.numeric(a==b))

prop.table(apply(ba, 3, sum)) # little answers in the middle

dim.ba <- dim(ba)
N.u <- dim.ba[[1]]; N.q <- dim.ba[[2]]; N.c <- dim.ba[[3]]; K <- 3L
theta <- array(runif(N.u*K), c(N.u, K))  # p(k | user)
phi <- array(runif(K*N.q*N.c), c(K, N.q, N.c)) # p(choice | q, k)

for (i in 1:100) {
# Proper normalization for theta and phi
  theta <- theta + .1; phi <- phi + .1
  theta <- theta/outer(apply(theta, 1, sum), rep(1, K)); ensure.norm(theta, 1)
  phi <- phi/outer(apply(phi, c(1, 2), sum), rep(1, N.c)); ensure.norm(phi, c(1, 2))
  
  # G = theta*phi = p(choice, k | q, user) with some independence assumptions (from the model)
  G <- outer(outer(theta, rep(1, N.q)), rep(1, N.c)) * outer(rep(1, N.u), phi); ensure.norm(G, c(1, 3))
  # p <- tensor(theta, phi, alongA=2, alongB=1); ensure.norm(p, c(1, 2))
  p <- apply(G, c(1, 3, 4), sum) # predictions for all possible answers
  p.v <- as.vector(p)[as.vector(ba==1)]
  lh <- sum(log(p.v)); cat(lh, " "); cat(apply(theta, 2, sum), "\n")
  hist(p.v, n=100)
  
  q.z <- aperm(outer(ba, rep(1, K)), c(1, 4, 2, 3)) * G # with unrealized choices
  p.z <- q.z / aperm(outer(apply(q.z, c(1, 3, 4), sum), rep(1, K)), c(1, 4, 2, 3))
  p.z[is.na(p.z)] <- 0
  
  theta <- apply(q.z, c(1, 2), sum)
  phi <- apply(p.z, c(2, 3, 4), sum)
  #theta <- apply(q.z, c(1, 2), sum)
  #phi <- apply(q.z, c(2, 3, 4), sum)
}

pol <- data.frame(name=d$name, party=d$party, theta)
pol %>% arrange(desc(X1)) %>% head(10)
pol %>% arrange(desc(X2)) %>% head(10)
pol %>% arrange(desc(X3)) %>% head(10)
