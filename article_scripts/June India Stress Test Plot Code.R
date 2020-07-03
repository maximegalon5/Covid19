# Compare India's lockdown with the rest of the world

library(tidyverse)
library(viridis)

# Need wide data for clustering

#test <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/google_mobility/Global_Mobility_Report_accessed_5_27_2020.csv", 
                # col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                 #                 sub_region_1 = col_character(), 
                   #               sub_region_2 = col_character()))

test <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/Google_Mobility_Data_2020-06-01")

test <- test %>% 
  dplyr::filter(date > "2020-03-31") %>%
  dplyr::select(- c(sub_region_2, country_region_code)) %>%
  dplyr::group_by(country_region) %>%
  dplyr::summarise(Retail_Recreation = round(mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE), 0),
            Groceries_Pharmacy = round(mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE), 0),
            Parks = round(mean(parks_percent_change_from_baseline, na.rm = TRUE),0),
            Transit = round(mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),0),
            Workplace = round(mean(workplaces_percent_change_from_baseline, na.rm = TRUE),0),
            Residential = round(mean(residential_percent_change_from_baseline, na.rm = TRUE),0))

test$Residential <- -test$Residential
  
test <- transform(test, Average_Mobility = round(rowMeans(test[,-1], na.rm = TRUE),0))

test <- test %>% arrange(Average_Mobility)

test$Residential <- -test$Residential
# Make Long

test_long <- test %>% 
  gather(Mobility_Type, Percent_Change, - country_region)

test_long_india <- test_long %>% dplyr::filter(country_region == "India")

Compare_plot <- test_long %>%
  ggplot(aes(x=Mobility_Type, y=Percent_Change, fill = Mobility_Type)) +
  geom_violin() +
  geom_boxplot(color = "blueviolet", linetype = "dotted", alpha = 0, size = 0.5) +
  geom_point(data = test_long_india, aes(x = Mobility_Type, y = Percent_Change),
             color="white", size = rel(2)) +
  theme_bw() +
  theme(
    plot.title = element_text(size=rel(1)),
    strip.text = element_text(size = rel(.8)),
    legend.position = "none") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Comparing India's Lockdown to the Rest of The World",
       y = "Ave. Apr-May Change in Mobility over Jan-Feb Base",
       x = "Mobility Type",
       caption = "Google LLC: Google COVID-19 Community Mobility Reports 
       https://www.google.com/covid19/mobility
       Accessed: 1st June 2020",
       fill = "Mobility Type") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_discrete(breaks=c("Average_Mobility", "Groceries_Pharmacy", "Parks", "Residential", "Retail_Recreation",
                            "Transit", "Workplace"),
                   labels=c("Ave. Reduction in Mobility", "Groceries and Pharmacies", "Parks", "Residential", 
                            "Retail and Recreation", "Transit Stations", "Workplace"),
                   guide = guide_axis(n.dodge=3)) +
  geom_text(data = test_long_india, aes(label = "India"), nudge_y = 7, size = rel(2), color = "white")
