#Download and Save World Data from JHU

url <- "https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv"

world_covid_data <- read.csv(url, header = TRUE)
library(lubridate); library(magrittr); library(tidyverse)

world_covid_data <- world_covid_data %>% mutate(Date = ymd(date))

filename <- paste0("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/world_covid_data_", Sys.Date(), sep = "")

saveRDS(world_covid_data, file = filename)