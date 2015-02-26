library(dplyr)

ensure.norm <- function (m, dims) stopifnot(sum(abs(apply(m, dims, sum)-1)) < 1e-8)

data.file <- "http://dynamic.hs.fi/arkku/tiedostot/23737467ehdokasvastaukset-18-02.csv"
d <- read.csv(data.file, fileEncoding="861", header=T, sep=";")
saveRDS(d, "d.rds")

ba <- outer(a, min(a):max(a), function (a, b) as.numeric(a==b))

prop.table(apply(ba, 3, sum)) # little answers in the middle

dim.ba <- dim(ba)
N.u <- dim.ba[[1]]; N.q <- dim.ba[[2]]; N.c <- dim.ba[[3]]; K <- 6L
theta <- array(runif(N.u*K), c(N.u, K)) + 100  # p(k | user)
phi   <- array(runif(K*N.q*N.c), c(K, N.q, N.c)) + 100 # p(choice | q, k)

n.iter <- 200; i.iter <- 0
while(T) {
  # Proper normalization for theta and phi
  theta <- theta/outer(apply(theta, 1, sum), rep(1, K)); ensure.norm(theta, 1)
  phi <- phi/outer(apply(phi, c(1, 2), sum), rep(1, N.c)); ensure.norm(phi, c(1, 2))
  
  # G = theta*phi = p(choice, k | q, user), after some independence assumptions from the model.
  G <- outer(outer(theta, rep(1, N.q)), rep(1, N.c)) * outer(rep(1, N.u), phi); ensure.norm(G, c(1, 3))
  p <- apply(G, c(1, 3, 4), sum) # predictions for all possible answers
  p.v <- as.vector(p)[as.vector(ba==1)]
  llh <- sum(log(p.v))
  cat(i.iter, " ", llh, " "); cat(apply(theta, 2, sum), "\n")
  i.iter <- i.iter + 1
  if (i.iter>=n.iter) break
  
  q.z <- aperm(outer(ba, rep(1, K)), c(1, 4, 2, 3)) * G # with unrealized choices
  p.z <- q.z / aperm(outer(apply(q.z, c(1, 3, 4), sum), rep(1, K)), c(1, 4, 2, 3)) # p(k | ...)
  p.z[is.na(p.z)] <- 0
  
  theta <- apply(q.z, c(1, 2), sum) + .5
  phi <- apply(p.z, c(2, 3, 4), sum) + .1
}


comp.names <- paste("C", 1:K, sep="")
saveRDS(list(K=K, llh=llh, theta=theta, phi=phi), "sol6b.rds")
