#California Mobility Data = Time Lag
#Load Packages
library(tidyverse); library(dygraphs); library(xts); library(lubridate)
library(RColorBrewer); library(formattable)

#Load Mobility Data
setwd("~/GitHub")
Mobility_Report <- read_csv("~/GitHub/Covid19/data/google_mobility/Global_Mobility_Report.csv", 
                                   col_types = cols(X1 = col_skip(), date = col_datetime(format = "%Y-%m-%d"), 
                                                          sub_region_1 = col_character(), sub_region_2 = col_character()))

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

CA_dat$Date <- as.Date(CA_dat$Date)

#CHHS Data from Download CHHS Data.R Changes made to csv by CHHS on June 25th
Covid_Data_Ca <- readRDS("~/GitHub/Covid19/data/CA_Covid_State_Data.Rds")

table_data <- Covid_Data_Ca %>% select(Date, `Total Confirmed`, `New Cases`, `Total Hosp` = Hospitalized, ICU, ICU_Available) %>%
  mutate(`% ICU` = percent(ICU/ICU_Available), `New ICU` = ICU-lag(ICU)) %>% select(-ICU_Available)

plot_data_temp <- Covid_Data_Ca %>%
  select(Date, "New Cases")

plot_data <- full_join(CA_dat, plot_data_temp, by = c("Date" = "Date"))

plot_data <- xts(plot_data, order.by = plot_data$Date)

presAnnotation <- function(dygraph, x, text) {
  dygraph %>%
    dyAnnotation(x, text, attachAtBottom = TRUE, width = 60, height = 20)
}

p <- dygraph(plot_data, main = "California Movement Restrictions vs New Cases") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(8, "Dark2")) %>%
  dyOptions(drawGrid = FALSE, axisLineColor = "navy", fillGraph = TRUE, fillAlpha = 0.1) %>%
  dyAxis("y", label = "Percentage Change in Movement", valueRange = c(-80, 80)) %>%
  dyHighlight(highlightCircleSize = 2, 
              highlightSeriesBackgroundAlpha = 0.2) %>%
  dyLegend(show = "follow", labelsSeparateLines = TRUE, width = 150) %>%
  dyEvent("2020-3-19", "California SiP", labelLoc = "bottom") %>%
  dyEvent("2020-3-16", "Bay Area SiP", labelLoc = "bottom") %>%
  dyEvent("2020-5-25", "Memorial Day", labelLoc = "bottom") %>%
  dyEvent("2020-6-14", "LA Protests", labelLoc = "bottom") %>%
  dySeries("New Cases", axis = "y2") %>%
  dyAxis("y2", label = "Daily Covid19 Positive Cases", valueRange = c(-15000, 15000)) %>%
  dyShading(from = "2020-5-26", to = "2020-6-4", color = "#e6ffff") %>%
  dyShading(from = "2020-6-4", to = "2020-6-9", color = "#e6fff3") %>%
  dyShading(from = "2020-6-14", to = "2020-6-24", color = "#e6ffff") %>%
  presAnnotation("2020-6-3", text = "15 Days") %>%
  presAnnotation("2020-6-21", text = "10 Days") %>%
  dyLimit(0, color = "red")

rm(CA_Mobility, CA_dat, Mobility_Report, Covid_Data_Ca, plot_data, plot_data_temp)