sigma = matrix(c(1,0.9,0.9,1),nrow = 2,ncol = 2)
mu = c(0,0)
rmvn.chol <- function(n,sigma,mu){
  d = length(mu)
  Z <- matrix(rnorm(n*d),nrow = n,ncol = d)
  Q <- chol(sigma)
  X <- Z %*% Q + matrix(mu, n, d, byrow = TRUE)
  X
}
x=rmvn.chol(1000,sigma,mu)
plot(x)

rmvn.eigen <- function(n,sigma,mu){
  d = length(mu)
  Z <- matrix(rnorm(n*d),nrow = n,ncol = d)
  eg = eigen(sigma,symmetric = TRUE)
  lmda = eg$values
  P = eg$vectors
  Q = P%*%diag(sqrt(lmda))%*%t(P)
  X <- Z %*% Q + matrix(mu, n, d, byrow = TRUE)
  X
}
x=rmvn.eigen(1000,sigma,mu)
plot(x)

rmvn.svd <- function(n,sigma,mu){
  d = length(mu)
  Z <- matrix(rnorm(n*d),nrow = n,ncol = d)
  S = svd(sigma)
  Q = S$u%*%diag(sqrt(S$d))%*%t(S$v)
  X <- Z %*% Q + matrix(mu, n, d, byrow = TRUE)
  X
}
x = rmvn.svd(1000,sigma,mu)
plot(x)