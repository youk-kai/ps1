library(dplyr)
library(tidyr)
library(fixest)
library(lubridate)
library(ks)
library(ggplot2)

#######

temperature_states <- readRDS('temperature_states.Rds')

region_state_counts <-
  temperature_states %>%
  group_by(region) %>%
  summarise(unique_states = n_distinct(state))

temperature_states <- temperature_states %>%
  mutate(region = case_when(region == 1 ~ 'North',
                            region == 2 ~ 'Northeast',
                            region == 3 ~ 'Southeast',
                            region == 4 ~ 'South',
                            region == 5 ~ 'Central-West' ))

# "dict" for region : state
temperature_states %>% group_by(region) %>% filter(state)

length(unique(temperature_states$state))

##################### (7)
#######

# For each day, average across all grid points
# to calculate the national average daily max temperature
nat2 <-
  temperature_states %>%
  group_by(year, month, day) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean))
nat2

# Calculate the number of days per year-month (2001-01, 2001-02, 2001-03,…)
# in which the average temperature exceeded 32C.
### v2 assumes "number of days above 32C" in item below
### is the number of days in this item
doom_nat2 <-
  nat2 %>%
  filter(avg_mean_tmax > 32) %>%
  count(month)
doom_nat2

# Regress the number of days above 32C on the calendar year, 
# using month fixed effects.
# (Hint: fixest::feols(y ~ x | FE, data=data), Unit of observation: month)
reg_nat2 <-
  doom_nat2 %>%
  ungroup(month)
feols(n ~ year | month, data=reg_nat2)


# Repeat the estimation for each region (North, Northeast,
# Southeast, South, Central West). (Hint: You need to select all grid points 
# that fall into one region. Unit of observation: month).
by_region2 <-
  temperature_states %>%
  group_by(region, year, month, day) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean)) 

doom_by_region2 <-
  by_region2 %>%
  filter(avg_mean_tmax > 32) %>%
  count(month)

reg_by_region2 <-
  doom_by_region2 %>%
  ungroup(year, month)
reg_by_region2

# duplicated(.) returns FALSE for all three

reg_by_region2 %>% group_map(~feols(n ~ year | month, data=.x))


##################### (8)

pmps <- #per month per state
  temperature_states %>%
  group_by(state, month, year) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean))

temperature_states$
  
  old_pmps <-
  pmps %>%
  filter(year > '2001' & year < '2004')
old_kde_result <- density(old_nat_mean$avg_mean_tmax, bw = 2, kernel = "gaussian")

new_pmps <-
  pmps %>%
  filter(year > '2018' & year < '2021')
new_kde_result <- density(new_nat_mean$avg_mean_tmax, bw = 2, kernel = "gaussian")

# # Plot the first KDE
# plot(old_kde_result, main = "Overlapping KDEs for mean_tmax", xlab = "mean_tmax", ylab = "Density", col = "blue")
# 
# # Add the second KDE on top of the first one
# lines(new_kde_result, col = "red")
# 
# # Add a legend to differentiate the two periods
# legend("topright", legend = c("Period 1", "Period 2"), col = c("blue", "red"), lty = 1)

# Extract KDE results into a data frame
old_kde_data <- data.frame(x = old_kde_result$x, y = old_kde_result$y, Period = "Period 1")
new_kde_data <- data.frame(x = new_kde_result$x, y = new_kde_result$y, Period = "Period 2")

# Combine the two KDE data frames
kde_data <- rbind(old_kde_data, new_kde_data)

ggplot(kde_data, aes(x = x, y = y, linetype = Period)) +
  geom_line(linewidth = .2) +  # Plot KDE lines
  labs(title = "Overlapping KDEs for mean_tmax",
       x = "mean_tmax",
       y = "Density") +
  scale_linetype_manual(values = c('dashed', 'solid')) + # Set colors
  theme(plot.title = element_text(hjust = 0.5)) 

#####################

install.packages("np")
library(np)

pypmps <- function(aregion){
  df <-
    temperature_states %>%
    filter(region == aregion) %>%
    group_by(state, month, year) %>%
    summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean))
  return(df)
}

# pypmps <- function(aregion){
#   df <-
#     temperature_states %>%
#     filter(region == aregion) %>%
#     group_by(state, month) %>%
#     summarize(avg_mean_tmax = mean(mean_tmax, na.rm = TRUE))
#   return(df)
# }

pypmps('South')
View(pypmps('South'))

regions <- unique(temperature_states$region)

npreg_input <- lapply(regions, pypmps)

npreg_input[[1]]

results <- lapply(npreg_input, function(df) {
  npreg(avg_mean_tmax ~ year, data = df)
})

names(results) <- regions

fitted_values <- data.frame()

# Loop through each region to extract fitted values
for (region in regions) {
  npreg_result <- results[[region]]
  
  # Create a data frame of fitted values
  region_data <- data.frame(
    year = npreg_result$eval,
    fitted = npreg_result$mean,
    region = region
  )
  
  # Combine with the main data frame
  fitted_values <- rbind(fitted_values, region_data)
}


# Assuming 'regions' has the names of the regions and 'results' contains the npreg objects
# Extract the fitted values and the corresponding year values for each region
fitted_data <- do.call(rbind, lapply(seq_along(results), function(i) {
  data.frame(
    year = results[[i]]$eval,  # Assuming 'x' contains the year values
    fitted_values = results[[i]]$mean,  # Assuming 'm' contains the fitted values
    region = regions[i]
  )
}))


ggplot(fitted_data, aes(x = year, y = fitted_values, linetype = region)) +
  geom_line(linewidth=.5) +
  labs(title = "Fitted npreg Results for Brazilian Regions",
       x = "Year",
       y = "Fitted Mean Temperature (Tmax)")
# scale_linetype_manual(breaks = c('North',
#                                  'Northeast',
#                                  'Central-West',
#                                  'Southeast',
#                                  'South'))



# results[[1]]$meAN
# 
# 
# results <- lapply(regions, function(region) {
#   # Get the summarized data for the region
#   summary_data <- pypmps(temperature_states, region)
#   
#   # Run npreg on the summarized data
#   npreg_result <- npreg(avg_mean_tmax ~ year, data = summary_data)
#   
#   return(npreg_result)
# })
# 
# results[['Southeast']]

# # Plot the first KDE
# plot(results[['South']], main = "Overlapping KDEs for mean_tmax", xlab = "year", ylab = "reg", col = "blue")
# 
# # Add the second KDE on top of the first one
# lines(x=pypmps('North'), y=results[['North']], col = "red")
# # Add a legend to differentiate the two periods
# legend("topright", legend = c("Period 1", "Period 2"), col = c("blue", "red"), lty = 1)
# 
# pypmps(temperature_states, 'North')
# pypmps('South')
# 
# npreg(avg_mean_tmax ~ year, data = pypmps(temperature_states, 'North'))
# 
# plot(results[['South']])
# 
# results[['North']]$mean
# 
# results[['South']]$x
# 
# plot(x=results[['North']]$eval, y=results[['North']]$mean)


# npreg_result$x
# 
# fitted_values
# 
# install.packages('ggplot2')
# library(ggplot2)
# 
# # Plot the results
# ggplot(fitted_values, aes(x = year, y = fitted, color = region)) +
#   geom_line() +
#   labs(title = "Fitted Values of Average Max Temperature by Region",
#        x = "Year",
#        y = "Fitted Average Max Temperature") +
#   theme_minimal() +
#   scale_color_discrete(name = "Region")
# 
# names(results) <- regions
# 
# # Access the results for each region
# results
# 
# # pypmps_south <- #per month per state
# #   temperature_states %>%
# #   filter(region == 'South') %>%
# #   group_by(year, state) %>%
# #   summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean))
# 
# model_south <- npreg(avg_mean_tmax ~ year, data = pypmps_south)
# 
# plot(model_south, main = paste("Local Constant Regression for", region),
#      xlab = "Year", ylab = "Average Temperature", type = "l")
# lines(new_kde_result, col = "red")
# legend("topright", legend = c("Period 1", "Period 2"), col = c("blue", "red"), lty = 1)
