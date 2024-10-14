library(dplyr)
library(tidyr)
# install.packages('fixest')
library(fixest)
# install.packages('lubridate')
library(lubridate)
# install.packages('ks')
library(ks)

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
                            region == 5 ~ 'Central West' ))

#######

nat <-
temperature_states %>%
  group_by(date) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean)) 
doom_nat <-
  nat %>%
  filter(avg_mean_tmax > 32)
print(sprintf('temp > 32 for %s days', nrow(doom_nat)))

reg_nat <- 
  doom_nat %>%
  mutate(year=sapply(date, year)) %>%
  mutate(month=sapply(date, month))

feols(avg_mean_tmax ~ year | month, data=reg_nat)

#######
  
by_region <-
  temperature_states %>%
  group_by(region, date) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean)) 
doom_by_region <-
  by_region %>%
  filter(avg_mean_tmax > 32)
print(doom_by_region %>% group_map(~nrow(.x)))

reg_by_region <-
  doom_by_region %>%
  ungroup() %>%
  mutate(year=sapply(date, year)) %>%
  mutate(month=sapply(date, month)) %>%
  group_by(region)

reg_by_region %>% group_map(~feols(avg_mean_tmax ~ year | month, data=.x))

#####################

pmps <- #per month per state
  temperature_states %>%
  group_by(month, state) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean))

old_nat_mean <-
  temperature_states %>%
  filter(date > '2001-01-01' & date < '2004-12-31') %>%
  group_by(month, state) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean))
# Estimate the KDE
old_kde_result <- density(old_nat_mean$avg_mean_tmax, bw = 2, kernel = "gaussian")

new_nat_mean <-
  temperature_states %>%
  filter(date > '2018-01-01' & date < '2021-12-31') %>%
  group_by(month, state) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean))
# Estimate the KDE
new_kde_result <- density(new_nat_mean$avg_mean_tmax, bw = 2, kernel = "gaussian")


# Plot the first KDE
plot(old_kde_result, main = "Overlapping KDEs for mean_tmax", xlab = "mean_tmax", ylab = "Density", col = "blue")

# Add the second KDE on top of the first one
lines(new_kde_result, col = "red")

# Add a legend to differentiate the two periods
legend("topright", legend = c("Period 1", "Period 2"), col = c("blue", "red"), lty = 1)

#####################

install.packages("np")
library(np)

pypmps <- function(aregion){
    df <-
      temperature_states %>%
      filter(region == aregion) %>%
      group_by(year, state) %>%
      summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean))
    return(df)
}

regions <- unique(temperature_states$region)

npreg_input <- lapply(regions, pypmps)

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
    year = npreg_result$x,
    fitted = npreg_result$mean,
    region = region
  )
  
  # Combine with the main data frame
  fitted_values <- rbind(fitted_values, region_data)
}

plot(results[['North']])

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
