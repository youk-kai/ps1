# ----------------------------------------
# Use processed gridded weather data
# update_information: 
# https://sites.google.com/site/alexandrecandidoxavierufes/brazilian-daily-weather-gridded-data?authuser=0
# main_paper: 
# Xavier, A.C., Scanlon, B.R., King, C.W. and Alves, A.I. (2022), New Improved Brazilian Daily Weather Gridded Data (1961-2020). Int J Climatol. 
# Digital object identifier
# https://doi.org/10.1002/joc.7731
# Interpolate_method: IDW, power: 2; n_neighbor: 5; variation_with_height: 0.006
# Sophie Mathes
# Oct 07, 2024
# ----------------------------------------


library("tidyverse")

in.file = here::here("temperature.Rds")
out.pdf = here::here("Local constant kernel regression.pdf")
out.pdf2 = here::here("Local linear kernel regression.pdf")

data = readRDS(in.file) %>%
  dplyr::filter(year<=2023)

names(data)
summary(data)


#  How many observations for each integer year?
data$count = 1
obs_table = data %>% group_by(year) %>% summarize(count=sum(count))


# --------------------------------------------------
#  Kernel density estimator
# --------------------------------------------------

x = data$mean_tmax
min = floor(min(x))
max = ceiling(max(x))
n = length(data$mean_tmax)[1]
print(n)
h = 1
epanechnikov = function(z) {0.75*(1-z^2)*as.numeric(abs(z)<1)}  
f_hat = function(xg) {
  k_argument = (xg - x)/h
  k = epanechnikov(k_argument)
  s = sum(k)/(n*h)
  return(s)
}
xg = seq(min(x), max(x), 0.5)
y = sapply(xg, f_hat)
plot(xg, y, pch=16, xlab="Temperature (C)")
lines(xg, y)


# --------------------------------------------------
#  Pointwise averages
# --------------------------------------------------

min = min(data$year)
max = max(data$year)
x = seq(min+3, max-3)

g0 = c()
for (i in x) {
  temp = data %>% dplyr::filter(year==i)  
  g0 = c(g0, mean(temp$mean_tmax))
}
plot(x,g0,pch=16, main="Pointwise averages")
lines(x,g0)



# --------------------------------------------------
#  Moving average (bandwidth, not k neighbors)
#  k neighbors is not well defined for discrete variables
# --------------------------------------------------

g3 = c()
for (i in x) {
  temp = data %>% dplyr::filter(abs(year-i)<=3)  
  g3 = c(g3, mean(temp$mean_tmax))
}
plot(x, g3, pch=16, main="Moving average")
lines(x, g3)

#  Plot together
par(mfrow=c(2,1))
plot(x,g0,pch=16, main="Average")
lines(x,g0)
plot(x,g3,pch=16, main="Moving average h=3")
lines(x,g3)

par(mfrow=c(1,1))



# --------------------------------------------------
#  Local constant estimator 
#  / Nadaraya-Watson estimator with Epanechnikov kernel
# --------------------------------------------------

epanechnikov = function(z) {0.75*(1-z^2)*as.numeric(abs(z)<1)}  

x = data$year
y = data$mean_tmax
n = dim(data)[1]
kernel = function(xg) {
  k_argument = (xg - x)/h
  k = epanechnikov(k_argument)
  s = sum(k*y)/sum(k)
  return(s)
}

xg = seq(min(x), max(x), 1)
head(xg)
tail(xg)


par(mfrow=c(1,1))
#  Calculate kernel regression estimator for bandwidth 1
h=1
y1 = sapply(xg, kernel)

#  Calculate kernel regression estimator for bandwidth 2
h=2
y2 = sapply(xg, kernel)

#  Calculate kernel regression estimator for bandwidth 3
h=3
y3 = sapply(xg, kernel)

pdf(out.pdf)
plot(xg, y1, main="Local constant kernel regression", 
     ty="l", lty=3, ylab="Mean daily max temperature in Brazil", xlab="Year", lwd=2, col="red")
points(xg, y1, col="red", pch=2)
lines(xg, y2, lty=2, col="blue")
points(xg, y2, col="blue", pch=3)
lines(xg, y3, lty=1)
points(xg, y3, lty=2, col="black", pch=16)
legend("topleft", legend=c("h=1", "h=2", "h=3"), col=c("red", "blue", "black"), pch=c(2,3,16), lty=c(3,2,1), cex=0.8)
dev.off()


#  Notice what happens with discrete data
par(mfrow=c(2,1))
h = 0.9
yk = sapply(xg, kernel)
plot(xg, yk, pch=16, main="h=0.9")
h = 0.5
yk = sapply(xg, kernel)
plot(xg, yk, pch=16, main="h=0.5")
par(mfrow=c(1,1)) # set back to one plot

#  There are no observations in the 0.5-0.9 distance range....
#  The distances are only 0, 1, 2, 3,.... integers!


# --------------------------------------------------
#  Local linear example
# --------------------------------------------------

x = data$year
y = data$mean_tmax

head(x)
head(y)

h = 1
zmat = function(xg){cbind(rep(1, length(x)), x-xg)}
#  make sure to feed in one grid point at a time only
n = dim(data)[1]

# Dimension of K at x? beware

xg = 2002 # this is an example, can be any value 
k_argument = (xg - x)/h
k = sapply(k_argument, epanechnikov)
z = zmat(xg)
tzk = t(z) * k
tzk[,1:6]
head(z)
tzkz = tzk %*% z
tzky = tzk %*% y
beta = solve(tzkz) %*% tzky
beta

h = 1
beta_ll = function(xg0) {
  k_argument = (xg0 - x)/h
  k = sapply(k_argument,epanechnikov)
  z = zmat(xg0)
  tzk = t(z) * k
  tzkz = tzk %*% z
  tzky = tzk %*% y
  beta = solve(tzkz)%*% tzky
  return(beta)
}

xgrid = seq(min(x), max(x), 0.5)
beta = sapply(xgrid, beta_ll)
loclin = data.frame(xgrid, b0=beta[1,], b1=beta[2,])

pdf(out.pdf2)
plot(x, y, pch=16, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)),
     xlab="Year", ylab="Mean daily max temperature")
points(loclin$xgrid, loclin$b0, pch=15, col="blue")
nx = dim(loclin)[1]
for (i in 1:nx) {
  abline(loclin$b0[i]-loclin$xgrid[i]*loclin$b1[i], loclin$b1[i], 
         lty=3, col="blue")
}
dev.off()

