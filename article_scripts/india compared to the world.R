# Load World Covid Data
library(tidyverse)
world_covid_data <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/world_covid_data_2020-06-05")
LastDate <- last(world_covid_data$date)

#total confirmed cases

total_confirmed_top10 <- world_covid_data %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_confirmed = sum(cases)) %>%
  arrange(-total_confirmed) %>%
  head(10)

top10_list <- total_confirmed_top10$country

#Summary Deaths
world_covid_data_wide <- world_covid_data %>%
  dplyr::filter(province == "") %>%
  tidyr::pivot_wider(names_from = type, values_from = cases) %>%
  dplyr::mutate(active = confirmed -(recovered + death)) %>%
  dplyr::select(Date, country, confirmed, active, death, recovered)

#Cumulative US Data
US_cum_data <- world_covid_data_wide %>% filter(country == "US") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

#Cumulative Brazil Data
Brazil_cum_data <- world_covid_data_wide %>% filter(country == "Brazil") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(US_cum_data, Brazil_cum_data)

rm(US_cum_data); rm(Brazil_cum_data)

#Cumulative Russia Data
Russia_cum_data <- world_covid_data_wide %>% filter(country == "Russia") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(final_wide_table, Russia_cum_data)

rm(Russia_cum_data)

#Cumulative UK Data
UK_cum_data <- world_covid_data_wide %>% filter(country == "United Kingdom") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(final_wide_table, UK_cum_data)

rm(UK_cum_data)

#Cumulative Spanish Data
Spain_cum_data <- world_covid_data_wide %>% filter(country == "Spain") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(final_wide_table, Spain_cum_data)

rm(Spain_cum_data)

#Cumulative Italian Data
Italy_cum_data <- world_covid_data_wide %>% filter(country == "Italy") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(final_wide_table, Italy_cum_data)

rm(Italy_cum_data)

#Cumulative French Data
France_cum_data <- world_covid_data_wide %>% filter(country == "France") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(final_wide_table, France_cum_data)

rm(France_cum_data)

#Cumulative Germany Data
Germany_cum_data <- world_covid_data_wide %>% filter(country == "Germany") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(final_wide_table, Germany_cum_data)

rm(Germany_cum_data)

#Cumulative Indian Data

India_cum_data <- world_covid_data_wide %>% filter(country == "India") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(final_wide_table, India_cum_data)

rm(India_cum_data)

#Cumulative Turkish Data
Turkey_cum_data <- world_covid_data_wide %>% filter(country == "Turkey") %>% 
  mutate(cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>%
  dplyr::filter(Date == LastDate)

final_wide_table <- bind_rows(final_wide_table, Turkey_cum_data)

final_wide_table <- final_wide_table %>% select(-Date)

final_wide_table$country[1] <- "United States"

