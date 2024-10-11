%
%

library(dplyr)
install.packages('fixest')
library(fixest)
install.packages('lubridate')
library(lubridate)


temperature_states <- readRDS('temperature_states.Rds')
head(temperature_states)

table_avg_mean_tmax <- 
  temperature_states %>%
  group_by(date) %>%
  summarise_at(vars(mean_tmax), list(avg_mean_tmax = mean)) 

avg_mean_tmax %>%
  filter(avg_mean_tmax > 32)

nrow(avg_mean_tmax)
  
months <- lapply(table_avg_mean_tmax$date, month) %>% as.integer()
reg_data <- table_avg_mean_tmax %>% mutate(months=months)

tail(reg_data)

feols(avg_mean_tmax ~ date | months, data=reg_data)


df %>%
  group_by(col_to_group_by) %>%
  summarise_at(vars(col_to_aggregate), list(name = mean))
