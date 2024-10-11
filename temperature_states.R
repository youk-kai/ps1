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
library("geobr")  # https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html
library("sf")

in.file = here::here("data", "Tmax_20010101_20240320_BR-DWGD_UFES_UTEXAS_v_3.2.3.nc")
out.file = here::here("temperature_states.Rds")


### --------------------------------------------------
###  Find state geo-coordinates, save in WGS84 geocoordinates
### --------------------------------------------------
states = geobr::read_state(simplified=FALSE) # comes in crs "EPSG:4674", SIRGAS 2000
# Map to WGS64 coordinate system which is used by BR-DWGD 
states = st_transform(states, crs = "EPSG:4326") # the values look very similar at the 4-th decimal in both systems
n_states = dim(states)[1]
dim(states)



### --------------------------------------------------
###  Load data, comes in 3-dimensional array, sequence of matrices
### --------------------------------------------------
var = "Tmax"
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
n_time = length(time)
time_units = ncatt_get(data, "time", "units")
time_units

#  First date should be 2001-01-01
#  Divide by 24 because measured in hours since 1961-01-01 00:00
as.Date(time[1]/24, origin="1961-01-01")
#  Bingo!
#  Last date should be 2024-03-20, as specified in file name
as.Date(time[n_time]/24, origin="1961-01-01")



#  Set up dataset to check for each grid point in which municipality it lands
grid = expand.grid(lon=1:n_lon, lat=1:n_lat)
grid$v_lon = lon[grid$lon]
grid$v_lat = lat[grid$lat]

#  Select the temperature value array
tmax = ncvar_get(data, "Tmax")
dim(tmax)

#  For each day, fill up grid. Start with first day to identify grid points with NA values, eliminate
#  them for the future
t = 1
temp = tmax[,,t]
lookup_values = function(x, y) {z=temp[x,y]}
grid$tmax = mapply(lookup_values, grid$lon, grid$lat) # look up value in grid point
grid = dplyr::filter(grid, !is.na(tmax))  # dont bother looking up values of empty grid points (outside BR)
grid$date = as.Date(time[t]/24, origin="1961-01-01")
grid$state = 0
head(grid, n=5)
grid = sf::st_as_sf(
  grid, 
  coords = c("v_lon","v_lat"), 
  crs=st_crs("EPSG:4326")) # Alexandre Candido Xavier <alexandre.xavier@ufes.br> suggests to use epsg:4326. 
grid

#  For each state polygon, identify all grid points within the state polygon,
#  record the state in which each point falls
for (s in 1:n_states) {
  find_points = st_contains(states[s,], grid)
  grid$state[unlist(find_points)] = states$code_state[s]
}
grid

#  Now we have the grid set up, let's collect the information for every point in time
#  For each day, average across all points that lie in the same state
temp_per_day = function(t) {
  temp = tmax[,,t]
  lookup_values = function(x, y) {z=temp[x,y]} # need to have the function definition inside the larger function
  temp = mapply(lookup_values, grid$lon, grid$lat)
  temp = data.frame(state = grid$state, tmax=temp) %>%
    dplyr::filter(state>0) %>%
    group_by(state) %>%
    summarize(mean_tmax = mean(tmax))
  temp$date = time[t]
  return(temp)
}

out = lapply(1:n_time, temp_per_day)
out = do.call(rbind, out)

out$state = as.integer(out$state)
out$region = as.integer(floor(out$state/10)) # region is the first digit of the state code
out$date = as.Date(out$date/24, origin="1961-01-01") # set into human readable format
out$day = as.integer(format(out$date, format="%d"))
out$month = as.integer(format(out$date, format="%m"))
out$year = as.integer(format(out$date, format="%Y"))
head(out, n=5)

saveRDS(out, out.file)
