library("tidyverse")


# ------------------------------
#  Grid search 
# ------------------------------

# Set up a grid of points

xg = seq(-1, 1, by=0.01)
cat(head(xg), "...", tail(xg), "\n")

# Evaluate and find the highest point on there

n  =  seq(0, 1001, 1) # we cannot actually sum to infinity in a computer...

f  =  function(x) {
  z  =  (-1)^n * x^(2^n)
  z  =  sum(z)
  return(z)
}

xg = seq(-1, 1, by=0.01)
y = sapply(xg, f) # apply the function osc to each element of xg
which(y==max(y)) # which element of y contains the maximum?
xg[which(y==max(y))]
plot(xg, y, type="l")


# Search on a finer grid

xg = seq(0.95, 0.9999, by=0.0001)
cat(head(xg), "...", tail(xg), "\n")
y = sapply(xg, f) # apply the function osc to each element of xg
which(y==max(y)) # which element of y contains the maximum?
xg[which(y==max(y))]
plot(xg, y, type="l")

# -> back to lecture notes

# ------------------------------
#  Gradient methods, simple examples
# ------------------------------

# ------------------------------
#  Simple example 
#  No information from second derivative (Hessian)
#  Calculate slope of gradient at point b
#  Then solve for where this linearized gradient is 0
#  this is a linear function with slope g(b) and offset f(b)
#  f(b) + g(b)*(bnew-b) = 0
#   =>  bnew = b - f(b)/grad(b)
# ------------------------------

#  Example: f(x)=(x-5)^2

f = function(x) {(x-5)^2}
grad = function(x) {2*(x-5)}


xgrid = seq(0, 10, 0.1)
b0 = 1  # starting value
plot(xgrid, f(xgrid), type="line", lwd=2)
abline(h=0, lty=3)
for (i in 1:10) {
  if (i==1) {b=b0}
  cat("\n# ----------------------------------------\n")  
  cat("# Iteration:", i, "\n")  
  cat("# ----------------------------------------\n")  
  cat("Parameter:", b, "\n")  
  cat("Function value:", f(b), "\n")  
  g = grad(b)
  if (i<4) {
    points(b, f(b), col=i, pch=16, cex=1.5)
    text(b+0.5, f(b)+0.5, i, col=i)
    abline(f(b)-g*b, g, col=i)
    abline(v=b-f(b)/g, col=i)
    points(b-f(b)/g,0, col=i, pch=15)
  }
  bnew = b - f(b)/g
  cat("Gradient:", g, "\n")  
  cat("Step:", - f(b)/g, "\n")  
  cat("New value:", bnew, "\n")  
  b = bnew
}
points(b, f(b), col="blue", pch=15)
text(b+0.5, f(b)+0.5, "final", col="blue")
#  10 iterations and still going...



# ------------------------------
#  Simple example 
#  ! Use information from second derivative (Hessian)
# ------------------------------

#  Example: f(x)=(x-5)^2

f = function(x) {(x-5)^2}
grad = function(x) {2*(x-5)}
h = 2

xgrid = seq(0, 10, 0.1)

A = -solve(h)
b0 = 1  # starting value
plot(xgrid, f(xgrid), type="line", lwd=2)
abline(h=0, lty=3)
for (i in 1:5) {
  if (i==1) {b=b0}
  cat("\n# ----------------------------------------\n")  
  cat("# Iteration:", i, "\n")  
  cat("# ----------------------------------------\n")  
  cat("Parameter:", b, "\n")  
  cat("Function value:", f(b), "\n")  
  points(b, f(b), col="blue", pch=16, cex=1.5)
  g = grad(b)
  if (g!=0) {
    text(b+0.5, f(b)+0.5, i, col="blue")
    abline(f(b)-g*b, g, col="blue")
  }
  step = A %*% g
  cat("Gradient:", g, "\n")  
  cat("Step:", step, "\n")  
  bnew = b + step
  cat("New value:", bnew, "\n")  
  b = bnew
}
points(b, f(b), col="blue", pch=15)
text(b+0.5, f(b)+1, "final", col="blue")
#  Found the minimum in second iteration...


# ------------------------------
#  Less simple example 
# ------------------------------

#  Use function f(x)=(x-5)^3 + x^2
f = function(x) {x^3 - 10*x^2 + 10*x}
grad = function(x) {3*x^2 - 20*x + 10}
h = function(x) {6*x-20}

library("colorspace")
palette = rev(colorspace::sequential_hcl(6))

#  Try this for starting values -5, 10, 6, 4 ----------
b=4  # starting value
print_table = c()
xx = seq(-5,10,0.1)
#xx = seq(3.5,7,0.1)
plot(xx,f(xx),ty="l")
points(b, f(b), cex=1.5, pch=1)
abline(v=b, lty=3)
for (i in 1:10) {
  g = grad(b)
  points(b, f(b), col=palette[i], cex=1.5, pch=16)
  A = -solve(h(b))
  step = A %*% g
  bnew = b + step
  printline = round(c(i, b, f(b), g, A, step), 3)
  if (i==1) {print_table=printline} else {print_table=rbind(print_table, printline)}
  b = bnew
}
colnames(print_table) = c("Iterations", "b", "fval", "Gradient", "Hessian", "Step")

knitr::kable(print_table, row.names = FALSE)

points(b, f(b), cex=1.5, pch=16)

# -> back to lecture notes


# ------------------------------
#  Simple example exponential regression model with randomly generated data
# ------------------------------

# Code this up and iterate 20 times...

n = 100
y = rnorm(n, 2, 2)  #  generate some sample data with mean 2
mean(y)

Q = function(b) {(1/(2*n)) * sum((y-exp(b))^2)}
g = function(b) {-exp(b)*(mean(y)-exp(b))}
A = function(b) {1/(exp(b)*(mean(y)-2*exp(b)))}  # notice this is the negative inverse Hessian

#  Ok, we have an objective function, a gradient, and a weighting matrix

#  Start at an initial guess and iterate
b0 = 1
for (i in 1:20) {
  if (i==1) {b=b0}
  cat("\n...\nGradient at b =", g(b), "\n")
  cat("Updating (negative inverted Hessian) at b =", A(b), "\n")
  bnext = b + A(b)*g(b)
  cat("Next guess b =", bnext, "\n")
  b = bnext
}

xgrid = seq(0.1, 1, 0.001)
yvalues = sapply(xgrid, Q)
ygrad = sapply(xgrid, g)
ymin = min(c(yvalues, ygrad))
ymax = max(c(yvalues, ygrad))
plot(xgrid, yvalues, ty="l", ylim=c(ymin,ymax), lwd=2)
lines(xgrid, ygrad)
abline(h=0, lty=3, col="blue")
abline(v=b, lty=3, col="blue")

# -> back to lecture notes


# ------------------------------
#  GMM Example
# ------------------------------

library("AER")
data("MASchools")


head(MASchools)

x1 = MASchools$stratio
x2 = MASchools$english
x3 = MASchools$income
x4 = MASchools$income^2
score = MASchools$score4
n = dim(MASchools)[1]

Jn=function(beta){
  e = score - x1*beta[1] - x2*beta[2] - x3*beta[3]
  g1 = (1/n*(t(x1)%*%e))
  g2 = (1/n*(t(x2)%*%e))
  g3 = (1/n*(t(x3)%*%e))
  g4 = (1/n*t(x4)%*%e) # let's use a square term in income
  g = c(g1,g2,g3,g4)
  z = as.numeric(n*t(g)%*%g)
  return(z)
}

#  Benchmark: Suggested R::optim with method 'BFGS'
##  The optimal point of the function Jn, starting at initial guess (0,0,0), is
sol=optim(par = c(0,0,0), Jn, method = 'BFGS')
beta_gmm=sol$par
beta_gmm

##  OLS to compare
x = cbind(x1, x2, x3)
y = score
beta_ols= solve(t(x)%*%x)%*%t(x)%*%y
beta_ols


#  Now try different minimization methods for Jn


# ------------------------------
#  GMM application, grid search
# ------------------------------

xb1 = seq(25,35,1)
xb2 = seq(-5,5,1)
xb3 = seq(0,10,1)

#  Construct a grid of all combinations of parameters
xb = expand.grid(xb1,xb2,xb3)
head(xb)
tail(xb)
dim(xb)


#  Evaluate Jn on all of them, multiply objective function with factor n
#  apply function Jn to each row (1) in xb. use apply(object, 2, function) to apply to columns
y_gridsearch = apply(xb, 1, Jn)

#  Was the Jn function correctly applied to our suggested grid?
xb[1,]
Jn(c(25,-5,0))
y_gridsearch[1]
sprintf("%0.0f", Jn(c(25,-5,0)))
sprintf("%0.0f", y_gridsearch[1])
#  Looks like this worked

#  So what did we find? Looks like an "interior" solution
output = cbind(xb, y_gridsearch)
pick = which(y_gridsearch==min(y_gridsearch))
pick
output[pick,]

summary(y_gridsearch)
sprintf("%0.0f", min(y_gridsearch))



# ------------------------------
#  Newton-Raphson
#  GMM with gradient methods
# ------------------------------

#  For Newton-Raphson (and other methods), need the Hessian matrix
#  First, calculate the gradient / Jacobian (d Jn / d beta) 
#  yes, by hand

gradient = function(beta) {
  u = score-x%*%beta
  Jnb1 = (1/n)*(
    2 * sum(x1*u) * sum(-x1*x1) +
      2 * sum(x2*u) * sum(-x1*x2) +
      2 * sum(x3*u) * sum(-x1*x3) +
      2 * sum(x4*u) * sum(-x1*x4)
  )
  Jnb2 = (1/n)*(
    2 * sum(x1*u) * sum(-x2*x1) +
      2 * sum(x2*u) * sum(-x2*x2) +
      2 * sum(x3*u) * sum(-x2*x3) +
      2 * sum(x4*u) * sum(-x2*x4)
  )
  Jnb3 = (1/n)*(
    2 * sum(x1*u) * sum(-x3*x1) +
      2 * sum(x2*u) * sum(-x3*x2) +
      2 * sum(x3*u) * sum(-x3*x3) +
      2 * sum(x4*u) * sum(-x3*x4)
  )
  grad = c(Jnb1, Jnb2, Jnb3)
  return(grad)
}

gradient(beta_gmm)
#  Remember that beta_gmm was found by minmizing Jn, so the gradient here should be near zero
gradient(beta_ols)



#  In the MAschools example, the Hessian is constant in beta (check!)

#  multiply ojective with n
h11 = (2/n)*(sum(-x1*x1)^2 + sum(-x1*x2)^2 + sum(-x1*x3)^2  + sum(-x1*x4)^2)
h12 = (2/n)*(
  sum(-x1*x1)*sum(-x2*x1) + 
    sum(-x1*x2)*sum(-x2*x2) + 
    sum(-x1*x3)*sum(-x2*x3)  + 
    sum(-x1*x4)*sum(-x2*x4)
)
h13 = (2/n)*(
  sum(-x3*x1)*sum(-x1*x1) + 
    sum(-x3*x2)*sum(-x1*x2) + 
    sum(-x3*x3)*sum(-x1*x3)  + 
    sum(-x3*x4)*sum(-x1*x4)
)
h22 = (2/n)*(sum(-x2*x1)^2 + sum(-x2*x2)^2 + sum(-x2*x3)^2  + sum(-x2*x4)^2)
h23 = (2/n)*(
  sum(-x3*x1)*sum(-x2*x1) + 
    sum(-x3*x2)*sum(-x2*x2) + 
    sum(-x3*x3)*sum(-x2*x3)  + 
    sum(-x3*x4)*sum(-x2*x4)
)
h33 = (2/n)*(sum(-x3*x1)^2 + sum(-x3*x2)^2 + sum(-x3*x3)^2  + sum(-x3*x4)^2)
h21 = h12
h31 = h13
h32 = h23

H = matrix(c(h11, h21, h31, h12, h22, h23, h31, h32, h33), 3, 3, byrow=FALSE)
# plausibility check for positive or negative definite?
as.numeric(rep(1,3) %*% H %*% rep(1,3)) 
# looks like H is positive definite, so in principle this should help, globally convex everywhere, good for minimizing


#  Start with some initial guess
#b = beta_ols
b = c(-10,10,100)
for (i in 1:10) {
  cat("\n# ----------------------------------------\n")  
  cat("# Iteration:", i, "\n")  
  cat("# ----------------------------------------\n")  
  cat("Parameter:", b, "\n") 
  fval = Jn(b)
  cat("Function value:", fval, "\n")  
  g = gradient(b)
  if (i==1) {res = c(i, b, fval, g)} else {res = rbind(res,c(i, b, fval, g))}
  A = -solve(H)
  step = A %*% g
  cat("Updating matrix:\n")  
  print(A)
  cat("Gradient:", formatC(g, format="e", digits=2), "\n")  
  cat("Step:", formatC(step, format="e", digits=2), "\n")  
  bnew = b + step
  cat("New value:", bnew, "\n")  
  b = bnew
}
res = data.frame(res)
names(res) = c("iteration", "b_st", "b_english", "b_income", "Jn_b", "g_b1", "g_b2", "g_b3")
knitr::kable(res, row.names = FALSE)

beta_gmm # looks good
#  Find the benhmark parameters in the second iteration

#  Notice that the Newton-Raphson method finds minima if you are in 
#  "convex territory", and maxima if you are in "concave territory"

# -> back to lecture notes


# ------------------------------
#  DFP method - works without Hessian matrix and without having to invert any matrices
#  Build the updating matrix A_t iteratively
#  (specification in CT textbook slightly incorrect)
#  Pos. def. updating A => (-A) means we decrease the objective function if added
#  https://optimization.cbe.cornell.edu/index.php?title=Quasi-Newton_methods
# ------------------------------

A = diag(3)
b = c(0, 0, 0)
res = c(Jn(b), b, gradient(b))
for (i in 1:20) {
  cat("\n\n# ----------------------------------------\n")  
  cat("# Iteration:", i, "\n")  
  cat("# ----------------------------------------\n")  
  cat("Parameter:", b, "\n") 
  fval = Jn(b)
  cat("Function value:", fval, "\n")  
  cat("Old updating matrix:\n")  
  print(A)
  g = gradient(b)
  cat("Gradient:", formatC(g, format="e", digits=2), "\n")  
  step = as.numeric(-A%*%g) # create vectors
  cat("Step:", formatC(step, format="e", digits=2), "\n")  
  bnew = b + step
  cat("New parameter:", bnew, "\n") 
  gnew = gradient(bnew)
  cat("New gradient:", gnew, "\n") 
  gdiff = as.numeric(gnew - g) # take differences in gradients
  cat("Gdiff:", gdiff, "\n")  
  denom1 = as.numeric(step %*% gdiff)
  denom2 = as.numeric(t(gdiff) %*% A %*% gdiff)
  cat("denom1:", denom1, "\n")  
  cat("denom2:", denom2, "\n")  
  Anew = A + outer(step,step)/denom1 - (A %*% outer(gdiff,gdiff) %*% A)/denom2
  cat("New updating matrix:\n")  
  print(Anew)
  Jnew = Jn(bnew)
  res.print = c(i, b, fval, g)
  if (i==1) {res = res.print} else {res = rbind(res, res.print)}
  A = Anew
  b = bnew
}
res = data.frame(res)
names(res) = c("iteration", "b_st", "b_english", "b_income", "Jn_b", 
               "grad_b1", "grad_b2", "grad_b3")
knitr::kable(res, row.names = FALSE)
beta_gmm
#  Need approx 9 iterations to find the benchmark betas


# ------------------------------
#  DFP with step size adjustment through line search
# ------------------------------

searchline = seq(0.01,2,0.01)
searchline

A = diag(3)
b = c(0, 0, 0)
res = c(Jn(b), b, gradient(b))
for (i in 1:10) {
  step = as.numeric(-A%*%gradient(b))
  # ------------------------------
  #  Try out all step sizes on the searchline, take the one that gives the lowest objective function value
  # ------------------------------
  bnewssa = sapply(searchline, function(lambda){b + lambda*step})
  bnewssa
  Qssa = apply(bnewssa, 2, Jn)
  Qssa
  step.size = searchline[Qssa==min(Qssa)]
  if (length(step.size)>1) {step.size=step.size[1]}
  step = step.size * step
  bnew  = b + step
  gdiff = as.numeric(gradient(bnew) - gradient(b)) # create vectors
  denom1 = as.numeric(step %*% gdiff)
  denom2 = as.numeric(t(gdiff) %*% A %*% gdiff)
  Anew = A + outer(step,step)/denom1 - (A %*% outer(gdiff,gdiff) %*% A)/denom2
  Jnew = Jn(bnew)
  res = rbind(res, c(bnew, Jnew, gradient(bnew)))
  A = Anew
  b = bnew
}
res = data.frame(res)
names(res) = c("b_st", "b_english", "b_income", "Jn_b", "g_b1", "g_b2", "g_b3")
rownames(res) = c()
print(res)
beta_gmm
#  Well, now we (maybe) saved one iteration


# ------------------------------
#  BFGS method 
#  also works without Hessian matrix + without inverting anything
#  (specification in CT textbook slightly incorrect)
#  https://optimization.cbe.cornell.edu/index.php?title=Quasi-Newton_methods
# ------------------------------

A = diag(3)
b = c(0, 0, 0)
res = c(Jn(b), b, gradient(b))
for (i in 1:10) {
  cat("\n\n# ----------------------------------------\n")  
  cat("# Iteration:", i, "\n")  
  cat("# ----------------------------------------\n")  
  cat("Parameter:", b, "\n") 
  fval = Jn(b)
  cat("Function value:", fval, "\n")  
  cat("\nOld updating matrix:\n")  
  print(A)
  g = gradient(b)
  cat("\nGradient:", formatC(g, format="e", digits=2), "\n")  
  step = as.numeric(-A%*%g) # create vectors
  cat("Step:", formatC(step, format="e", digits=2), "\n")  
  bnew = b + step
  cat("New parameter:", bnew, "\n") 
  gnew = gradient(bnew)
  cat("New gradient:", gnew, "\n") 
  gdiff = as.numeric(gnew - g) # take differences in gradients
  cat("Gdiff:", gdiff, "\n")  
  
  denom = as.numeric(step %*% gdiff)
  sand = diag(3) - outer(step, gdiff)/denom
  wich = diag(3) - outer(gdiff, step)/denom
  Anew = sand %*% A %*% wich + outer(step,step)/denom
  cat("\nNew updating matrix:\n")  
  print(Anew)
  Jnew = Jn(bnew)
  res.print = c(i, b, fval, g)
  if (i==1) {res = res.print} else {res = rbind(res, res.print)}
  A = Anew
  b = bnew
}
res = data.frame(res)
names(res) = c("iteration", "b_st", "b_english", "b_income", "Jn_b", 
               "grad_b1", "grad_b2", "grad_b3")
knitr::kable(res, row.names = FALSE)
beta_gmm
#  Also need approx 8 iterations to find the benchmark betas

