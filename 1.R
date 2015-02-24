library(dplyr)
#library(rstan)
library(tensor)

ensure.norm <- function (m, dims) stopifnot(sum(abs(apply(m, dims, sum)-1)) < 1e-8)

d <- read.csv("23737467ehdokasvastaukset-18-02.csv", header=T, sep=";")
a <- d %>% select(matches("^q[0-9]+")) %>% as.matrix
ba <- outer(a, min(a):max(a), function (a, b) as.numeric(a==b))

prop.table(apply(ba, 3, sum)) # little answers in the middle

dim.ba <- dim(ba)
N.u <- dim.ba[[1]]; N.q <- dim.ba[[2]]; N.c <- dim.ba[[3]]; K <- 7L
theta <- array(runif(N.u*K), c(N.u, K))  # p(k | user)
phi <- array(runif(K*N.q*N.c), c(K, N.q, N.c)) # p(choice | q, k)

n.iter <- 100; i.iter <- 0
while(T) {
# Proper normalization for theta and phi
  theta <- theta + .5; phi <- phi + .5
  theta <- theta/outer(apply(theta, 1, sum), rep(1, K)); ensure.norm(theta, 1)
  phi <- phi/outer(apply(phi, c(1, 2), sum), rep(1, N.c)); ensure.norm(phi, c(1, 2))
  
  # G = theta*phi = p(choice, k | q, user) with some independence assumptions (from the model)
  G <- outer(outer(theta, rep(1, N.q)), rep(1, N.c)) * outer(rep(1, N.u), phi); ensure.norm(G, c(1, 3))
  # p <- tensor(theta, phi, alongA=2, alongB=1); ensure.norm(p, c(1, 2))
  p <- apply(G, c(1, 3, 4), sum) # predictions for all possible answers
  p.v <- as.vector(p)[as.vector(ba==1)]
  lh <- sum(log(p.v)); cat(i.iter, " ", lh, " "); cat(apply(theta, 2, sum), "\n")
  #hist(p.v, n=100)
  i.iter <- i.iter + 1
  if (i.iter>=n.iter) break
  
  q.z <- aperm(outer(ba, rep(1, K)), c(1, 4, 2, 3)) * G # with unrealized choices
  p.z <- q.z / aperm(outer(apply(q.z, c(1, 3, 4), sum), rep(1, K)), c(1, 4, 2, 3)) # p(k | ...)
  p.z[is.na(p.z)] <- 0
  
  theta <- apply(q.z, c(1, 2), sum)
  phi <- apply(p.z, c(2, 3, 4), sum)
  #theta <- apply(q.z, c(1, 2), sum)
  #phi <- apply(q.z, c(2, 3, 4), sum)
}

pol <- data.frame(name=d$name, party=d$party, theta)
comp.names <- paste("X", 1:K, sep="")
for (cname in comp.names) {
  cat("\nComp ", cname, "\n")
  print(pol %>% arrange_(paste("-", cname, sep="")) %>% select_("name", "party", cname) %>% head(10))
}

hist(p.v, n=100)
heatmap(theta)

# Compute outer theta * party summed over candidates

