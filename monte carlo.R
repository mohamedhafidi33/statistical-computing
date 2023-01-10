g <- function(x){exp(x)}
n <- 1000

#using monte carlo integration
u <- runif(n)
T1 <- g(u)
mean(T1)
var(T1)

#using controle  variate
cv = cov(g(u),u)
v = var(u)
mu = mean(u)
c = -cv/v
T2 = g(u) + c*(u-mu)
mean(T2)
var(T2)


#using antithetic variate
u1 = runif(n/2)
u2 = 1-u1
Y1 = g(u1)
Y2 = g(u2)
T3 = (Y1+Y2)/2
mean(T3)
var(T3)