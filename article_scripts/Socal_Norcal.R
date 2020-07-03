#Comparing So Cal and Nor Cal Mobility for the Month of June

# Compare India's lockdown with the rest of the world

library(tidyverse)
library(viridis)

source('C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/download_scripts/Read_Google_Mobility_Data.R')

test <- Mobility_Report %>% 
  dplyr::filter(date > "2020-06-01") %>%
  dplyr::filter(sub_region_1 == "California") %>%
  dplyr::group_by(sub_region_2) %>%
  dplyr::summarise(Retail_Recreation = round(mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE), 0),
                   Groceries_Pharmacy = round(mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE), 0),
                   Parks = round(mean(parks_percent_change_from_baseline, na.rm = TRUE),0),
                   Transit = round(mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),0),
                   Workplace = round(mean(workplaces_percent_change_from_baseline, na.rm = TRUE),0),
                   Residential = round(mean(residential_percent_change_from_baseline, na.rm = TRUE),0)) %>%
  dplyr::mutate(sub_region_2 = replace_na(sub_region_2, "California"))

#rm(Mobility_Report)

colnames(test) <- c("County","Retail-Recreation",
                               "Grocery-Pharmacy",
                               "Parks", 
                               "Transit", 
                               "Workplace",
                               "Residential")

test$Residential <- -test$Residential

test <- transform(test, Average_Mobility = round(rowMeans(test[,-1], na.rm = TRUE),0))

test <- test %>% arrange(Average_Mobility)

test$Residential <- -test$Residential

# Make Long

test_long <- test %>% 
  gather(Mobility_Type, Percent_Change, - County)

Bay_Area <- c("San Mateo County", "Santa Clara County", 
                "Alameda County", "San Francisco County", "Marin County", 
                  "Contra Costa County", "Napa County", "Solano County")

LA_Metro <- c("Los Angeles County", "Orange County")

Make_Summary <- function(Dataframe) {
  Dataframe %>%
  dplyr::summarise(Percent_Change = mean(Percent_Change))
}

test_long_BA <- test_long %>% dplyr::filter(County %in% Bay_Area) %>% group_by(Mobility_Type) %>% Make_Summary()
  

test_long_LA <- test_long %>% dplyr::filter(County %in% LA_Metro) %>% group_by(Mobility_Type) %>% Make_Summary()

Compare_plot <- test_long %>% drop_na()%>%
  ggplot(aes(x=Mobility_Type, y=Percent_Change, fill = Mobility_Type)) +
  geom_violin(alpha = 0.25) +
  geom_boxplot(color = "grey", linetype = "dotted", alpha = 0, size = 0.5) +
  geom_point(data = test_long_BA, aes(x = Mobility_Type, y = Percent_Change),
             color="Black", size = rel(2)) +
  geom_point(data = test_long_LA, aes(x = Mobility_Type, y = Percent_Change),
             color="Blue", size = rel(2), position = position_jitter(width = 0.05)) +
  theme_bw() +
  theme(
    plot.title = element_text(size=rel(1)),
    strip.text = element_text(size = rel(.8)),
    legend.position = "none") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Comparing Bay Area (BA) to Los Angeles Metro (LAM) California State Mobility - June",
       y = "Ave. June Mobility over Jan-Feb Base",
       x = "Mobility Categories",
       caption = "Google LLC: Google COVID-19 Community Mobility Reports 
       https://www.google.com/covid19/mobility
       Accessed: 1st July 2020",
       fill = "Mobility Type") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_discrete(breaks=c("Average_Mobility", "Grocery.Pharmacy", "Parks", "Residential", "Retail.Recreation",
                            "Transit", "Workplace"),
                   labels=c("Ave. Reduction in Mobility", "Groceries and Pharmacies", "Parks", "Residential", 
                            "Retail and Recreation", "Transit Stations", "Workplace"),
                   guide = guide_axis(n.dodge=3)) +
  geom_text(data = test_long_BA, aes(label = "BA"), nudge_x = -.25, size = rel(2), color = "Black") +
  geom_text(data = test_long_LA, aes(label = "LAM"), nudge_x = .25, size = rel(2), color = "Blue") +
  geom_text(data = test_long_BA, aes(label = Percent_Change), nudge_x = -.25, nudge_y = 5, size = rel(2), color = "Black") +
  geom_text(data = test_long_LA, aes(label = Percent_Change), nudge_x = .25, nudge_y = 5, size = rel(2), color = "Blue")
  
