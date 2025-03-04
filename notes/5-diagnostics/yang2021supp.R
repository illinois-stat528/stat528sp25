
#######################################
### Code for "Assessment of Regression Models with Discrete Outcomes Using
### Quasi-Empirical Residual Distribution Functions"
#######################################
#######################################
#### Preamble

library(np)
### A function needed for bandwidth selection
listvec = function(x) {
  x[1]:x[2]
}
########################################
#######################################





########################################
########################################
## Poisson example
########################################
########################################


### \hat{U}. y is the vector of outcomes,
### and lambdaf is the vector of fitted Poisson mean
marginesti = function(u, y, lambdaf) {
  n = length(y)
  # F^{-1}(u)
  inv1 = qpois(u, lambdaf)
  inv1m = ifelse(inv1 > 0, inv1 - 1, 0)
  n1 = cbind(inv1, inv1m)
  # H^+ and H^_
  p1 = ppois(n1, lambdaf)
  # find out which one is closer to u
  ind1 = apply(abs(p1 - u), 1, which.min)
  # weight function
  wei = 1 * ((p1[cbind(1:n, ind1)] - u)^2 < 5 * h^2) *
    (1 - ((p1[cbind(1:n, ind1)] - u)^2) / h^2 / 5)
  l = sum(wei * 1 * (y <= n1[cbind(1:n, ind1)])) / sum(wei)
  l
}

marginesti = Vectorize(marginesti, "u")


####### bandwidth selector
bandwidthp = function(y, lambdaf) {
  n = length(y)
  newout = unlist(sapply(split(cbind(qpois(0.1, lambdaf), qpois(0.9, lambdaf)), 1:n), listvec))
  newlambda = rep(lambdaf, times = qpois(0.9, lambdaf) - qpois(0.1, lambdaf) + 1)
  newx = ppois(newout, newlambda)
  newy = 1 * (rep(y, times = qpois(0.9, lambdaf) - qpois(0.1, lambdaf) + 1) <= newout)
  
  bws = npregbw(ydat = newy[which(newx <= 0.9)], xdat = newx[which(newx <= 0.9)], ckertype = "epanechnikov")
  return(bws$bw)
}
