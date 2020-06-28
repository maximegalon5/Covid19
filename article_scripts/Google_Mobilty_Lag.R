#Google Mobility Data = Time Lag
#Load Packages
library(tidyverse); library(dygraphs); library(xts); library(lubridate)
library(RColorBrewer)

#Load Mobility Data
source('C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/download_scripts/Read_Google_Mobility_Data.R')

#Rename cols
colnames(Mobility_Report) <- c("Country", "State", "Sub-Region", "Date", "Retail-Recreation",
                                    "Grocery-Pharmacy",
                                    "Parks", 
                                    "Transit", 
                                    "Workplace",
                                    "Residential")

CA_Mobility <- Mobility_Report %>% filter(State == "California") %>% 
  dplyr::mutate(`Sub-Region` = replace_na(`Sub-Region`, "State"))

CA_dat <- CA_Mobility %>% filter(`Sub-Region` == "State") %>% 
  select(-c(Country, State, `Sub-Region`))

#Case Numbers From California (CHHS)
# last run on 6/24/2020
#URL <- "https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv"
#Covid_Data_Ca <- read.csv(URL,header = TRUE, sep = ",")

#saveRDS(Covid_Data_Ca, file = "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/CA_Covid_Cases.Rds")
Covid_Data_Ca <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/CA_Covid_Cases.Rds")

CA_Case_Data <- Covid_Data_Ca %>% group_by(Most.Recent.Date) %>% 
  summarise("Potential Cases" = sum(COVID.19.Positive.Patients + Suspected.COVID.19.Positive.Patients),
            "Potential ICU Cases" = sum(ICU.COVID.19.Positive.Patients + ICU.COVID.19.Suspected.Patients)) %>% 
  mutate(Date = mdy(Most.Recent.Date))

table_data <- CA_Case_Data 

plot_data_temp <- CA_Case_Data %>%
  select(Date, "Potential Cases")

plot_data <- full_join(CA_dat, plot_data_temp, by = c("Date","Date"))

plot_data <- xts(plot_data, order.by = plot_data$Date)

presAnnotation <- function(dygraph, x, text) {
  dygraph %>%
    dyAnnotation(x, text, attachAtBottom = TRUE, width = 80)
}

p <- dygraph(plot_data, main = "California Movement Restrictions vs Potential Cases") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2")) %>%
  dyOptions(drawGrid = FALSE, axisLineColor = "navy", fillGraph = TRUE, fillAlpha = 0.1) %>%
  dyAxis("y", label = "Percentage Change in Movement", valueRange = c(-80, 80)) %>%
  dyHighlight(highlightCircleSize = 2, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyLegend(width = 800) %>%
  dyEvent("2020-3-19", "California SiP", labelLoc = "bottom") %>%
  dyEvent("2020-3-16", "Bay Area SiP", labelLoc = "bottom") %>%
  dyEvent("2020-5-25", "Memorial Day", labelLoc = "bottom") %>%
  dyEvent("2020-5-29", "Lowest Potential Cases", labelLoc = "bottom") %>%
  dySeries("Potential Cases", axis = "y2") %>%
  dyAxis("y2", label = "Daily Covid19 Positive Cases", valueRange = c(-8000, 8000)) %>%
  dyShading(from = "2020-5-26", to = "2020-6-4", color = "#e6ffff") %>%
  dyShading(from = "2020-6-4", to = "2020-6-9", color = "#e6fff3") %>%
  presAnnotation("2020-6-4", text = "10+5 Days") %>%
  dyLimit(0, color = "red")
  
rm(CA_Mobility, CA_Case_Data, CA_dat, Mobility_Report, Covid_Data_Ca, plot_data, plot_data_temp)