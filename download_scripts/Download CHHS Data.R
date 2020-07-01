#Download CHHS Data

library(tidyverse)

#CHHS Hospital Surge Data

Surge_URL <- "https://data.ca.gov/dataset/cbbfb307-ac91-47ec-95c0-f05684e06065/resource/ef6675e7-cd3a-4762-ba75-2ef78d6dc334/download/bed_surge.csv"

Surge_Data <- read.csv(Surge_URL, header = TRUE, 
                       colClasses = c("character", "character", "character", "numeric", 
                                      "numeric", "numeric", "Date"), sep = ",")

Surge_Data[is.na(Surge_Data)] <- 0

Surge_Data <- Surge_Data %>% filter(status != "TBD") %>% group_by(Date = date, County = county) %>%
  summarise(Surge_Beds = sum(beds_ready_to_accept_patients + beds_in_warm_status - patients_in_beds))

CA_Surge_Beds <- Surge_Data %>% group_by(Date) %>% summarise(Surge_Beds = sum(Surge_Beds))


#CHHS Hospital By County Data

Hosp_URL <- "https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv"

Hosp_Data <- read.csv(Hosp_URL, header = TRUE, 
                      colClasses = c("character", "Date", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"), sep = ",")

CA_Hosp_Beds <- Hosp_Data %>% group_by(Date = todays_date, County = county) %>% summarise(Hospitalized = hospitalized_covid_patients,
                                                                                 ICU = icu_covid_confirmed_patients + icu_suspected_covid_patients,
                                                                                 ICU_Available = icu_available_beds,
                                                                                 Available_beds = all_hospital_beds) %>%
  filter(!is.na(Date))

CA_Hosp_Beds[is.na(CA_Hosp_Beds)] <- 0

rm(Hosp_Data)

CA_State_Beds <- CA_Hosp_Beds %>% group_by(Date) %>% summarise(Hospitalized = sum(Hospitalized),
                                                               ICU = sum(ICU),
                                                               ICU_Available = sum(ICU_Available),
                                                               Available_beds = sum(Available_beds))

#CHHS Case Data
Case_URL <- "https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv"



CA_Case_Data <- read.csv(Case_URL, header = TRUE, 
                         colClasses = c("character", "numeric", "numeric", 
                                        "numeric", "numeric", "Date"), sep = ",") %>% select(Date = date, County = county,
                                                                                             `Total Confirmed` = totalcountconfirmed,
                                                                                             `Total Deaths` = totalcountdeaths,
                                                                                             `New Cases` = newcountconfirmed,
                                                                                             `New Deaths` = newcountdeaths)
#Join Data
California_Covid_County_Data <- full_join(CA_Hosp_Beds, Surge_Data, by = c("Date", "County"))

California_Covid_County_Data <- right_join(California_Covid_County_Data, CA_Case_Data, by = c("Date", "County")) %>% arrange(Date, County)

California_Covid_State_Data <- California_Covid_County_Data %>% group_by(Date) %>% summarise_if(is.numeric, sum, na.rm = TRUE)

saveRDS(California_Covid_County_Data, file = "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/CA_Covid_County_Data.Rds")

saveRDS(California_Covid_State_Data, file = "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/CA_Covid_State_Data.Rds")

#rm(list = ls())
