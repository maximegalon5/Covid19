#Table of Indian States
library(tidyverse); library(formattable)
india_state_data <- readRDS('C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/india_state_data_2020-06-06')


df <- india_state_data %>% dplyr::filter(State == "Maharashtra") %>% 
  dplyr::mutate(growth_rate = (Total_Cases / lag(Total_Cases))) %>%
  dplyr::mutate(day_lag = as.numeric(Date - lag(Date))) %>%
  dplyr::mutate(growth_rate = growth_rate - 1) %>%
  dplyr::mutate(Growth_Rate = growth_rate/day_lag) %>%
  dplyr::mutate(Doubling_Period = round(log(2)/(Growth_Rate), 0)) %>%
  dplyr::mutate(CFR = Deaths / Total_Cases) %>%
  tail(5) %>% dplyr::select(State, Date, `Total Cases` = Total_Cases, CFR, `Growth Rate` = Growth_Rate, `Doubling Period` = Doubling_Period)

State_name <- unique(df$State)

article_table2 <- df %>%
  formattable(list(align =c("l","l","c", "c", "c", "r"),
    State = FALSE,
    `Total Cases` = color_tile("white", "orange"),
    CFR = percent,
    `Growth Rate` = percent,
    `Doubling Period` = color_tile("white", "green")))

