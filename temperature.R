### ------------------------------------------------------------
###  Load and process:
###  Brazilian Daily Weather Gridded Data (BR-DWGD)
###  Download from:
###   https://drive.google.com/drive/folders/11-qnvwojirAtaQxSE03N0_SUrbcsz44N
###  File: pr_Tmax_Tmin_NetCDF_Files.zip
###  Info:
###   https://sites.google.com/site/alexandrecandidoxavierufes/brazilian-daily-weather-gridded-data
###  Citation:
###   Xavier, A. C., Scanlon, B. R., King, C. W., & Alves, A. I. (2022). 
###   New improved Brazilian daily weather gridded data (1961–2020). 
###   International Journal of Climatology, 42(16), 8390–8404. https://doi.org/10.1002/joc.7731
###  Oct 7, 2024
###  Sophie Mathes
### ------------------------------------------------------------

library("ncdf4")
library("weathermetrics")
library("tidyverse")

in.file = here::here("data", "Tmax_20010101_20240320_BR-DWGD_UFES_UTEXAS_v_3.2.3.nc")
out.file = here::here("data", "temperature.Rds")

var = "Tmax"

#  Load data, comes in 3-dimensional array, sequence of matrices
data = nc_open(in.file)
data
length(data)
sapply(data, length)
names(data)
data$filename
data$writable
data$id
data$error
data$safemode
data$format
data$is_GMT
data$groups
data$fqgn2Rindex
data$ndims
data$natts
data$dim
data$unlimdimid
data$nvars
data$var

lon = ncvar_get(data, "longitude")
lon_units = ncatt_get(data, "longitude")
n_lon = length(lon)

lat = ncvar_get(data, "latitude")
lat_units = ncatt_get(data, "latitude")
n_lat = length(lat)

time = ncvar_get(data, "time")
n_time = length(time_units)
time_units = ncatt_get(data, "time", "units")
time_units

#  First date should be 2001-01-01
#  Divide by 24 because measured in hours since 1961-01-01 00:00
as.Date(time[1]/24, origin="1961-01-01")
#  Bingo!
#  Last date should be 2024-03-20, as specified in file name
as.Date(time[n_time]/24, origin="1961-01-01")


#  Select the temperature value array
temp = ncvar_get(data, "Tmax")
dim(temp)

valid_values = function(x){sum(!is.na(x))}


#  Access array: Longitude, latitude, time
temp1 = temp[,,1]
valid_values(temp1)
temp2 = temp[,,2]
valid_values(temp2)
temp3 = temp[,,3]
valid_values(temp3)
tempn = temp[,,8480]
valid_values(tempn)

sum(temp1>32, na.rm=TRUE)/valid_values(temp1)


#  Walk through each day, calculate the fraction of grid points with temp >32C
#  and calculate national average temperature
y_share = c()
x_time = 1:n_time
mean_tmax = c()
for (i in (x_time)) {
  tempi = temp[,,i]
  hot_days = sum(tempi>32, na.rm=TRUE)/valid_values(tempi)
  y_share = c(y_share, hot_days)
  mean_tmax = c(mean_tmax, mean(tempi, na.rm=TRUE))
  if (i%%1000==0) {
    print_date = as.Date(time[i]/24, origin="1961-01-01")
    cat("Share of points over 32C", hot_days, "on", as.character(print_date), "\n")
  }
}

x_date = as.Date(time/24, origin="1961-01-01")
plot(x_date, y_share, pch=16, cex=0.2, xlab="Date")

out = data.frame(x_time, x_date, y_share, mean_tmax)
out$month = as.integer(format(out$x_date, format="%m"))
out$year = as.integer(format(out$x_date, format="%Y"))
out$year_month = out$year * 100 + out$month

#  Notice that this is a national average
#  Now map grid points onto municipalities, and do this calculation by region 1,2,3,4,5...

saveRDS(out, out.file)
