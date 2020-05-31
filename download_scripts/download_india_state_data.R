#Download MOHFW Data for Indian States from the web

library(tidyverse); library(rvest); library(lubridate);library(gsubfn); #library(rebus);

# load previous india_state_data

india_state_data_old <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/india_state_data")

# Download New Data
M_URL <- "https://www.mohfw.gov.in/"

mohfw <- read_html(M_URL)

#extract total tests
india_state_data_new <- mohfw %>% html_nodes("table") %>% magrittr::extract2(1) %>%
  html_table()

india_state_data_new <- india_state_data_new[1:35,]

india_state_data_new <- india_state_data_new %>% select(State = "Name of State / UT",
                                    Total_Cases = "Total Confirmed cases*",
                                    Discharged = "Cured/Discharged/Migrated*",
                                    Deaths = `Deaths**`) %>%
  mutate(Date = Sys.Date())

india_state_data_new$Total_Cases <- as.numeric(india_state_data_new$Total_Cases)
india_state_data_new$Discharged <- as.numeric(india_state_data_new$Discharged)
india_state_data_new$Deaths <- as.numeric(india_state_data_new$Deaths)

india_state_data_new$State[9] <- "Delhi NCR"
india_state_data_new$State[31] <- "Telangana"

india_state_data <- dplyr::union(india_state_data_old, india_state_data_new)


filename <- paste0("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/india_state_data_", Sys.Date(), sep = "")

saveRDS(india_state_data, file = filename)
saveRDS(india_state_data, "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/india_state_data")

rm(india_state_data_old); rm(india_state_data_new); rm(mohfw)