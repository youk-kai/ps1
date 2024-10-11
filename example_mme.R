#  MME Micky Mouse example

set.seed(5487)
n = 20
k = 3
x = matrix(rnorm(n*k, 0, 1), n, k)

eps = rnorm(n, 0, 2)
beta = c(2,3,4)

y = x %*% beta + eps

#  GMM estimator

#  Suppose we want to calculate g_i(beta) for one x_i!
#  Specify one potential candidate for beta (1,1,1) and calculate

beta_hat = c(1,1,1)

g_1 = beta_hat[1] * x[1,1] + beta_hat[2] * x[1,2] + beta_hat[3] * x[1,3]

#  Specify g_1 as a function of beta

g_1 = function(beta) {
  z = x[1,] * (y[1] - (beta[1] * x[1,1] + beta[2] * x[1,2] + beta[3] * x[1,3]))
  return(z)
}

#  Try out:
g_1(c(1,1,1))
g_1(c(1,1,2))
# ... and so on


#  Calculate g_i for all observations i, and average!

g_bar = function(beta) {
  z = x * as.vector(y - (beta[1] * x[,1] + beta[2] * x[,2] + beta[3] * x[,3]))
  z = apply(z, 2, mean)
  return(z)
}

g_bar(c(1,1,1))
g_bar(c(1,1,2))
g_bar(c(1,1,3))
g_bar(c(2,3,4))

#  Now find beta_hat which sets g_bar to c(0,0,0)!
library("nleqslv")
res = nleqslv(c(0,0,0),fn=g_bar)
print(res)

beta
g_bar(beta)
res$x
g_bar(res$x)

###  Illustration of nleqslv: https://stackoverflow.com/questions/48832731/solving-a-system-of-nonlinear-equations-in-r


