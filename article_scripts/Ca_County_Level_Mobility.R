#California Mobility

#load packages

library(tidyverse); library(lubridate); #library(plotly); #library(epitools)

Global_Mobility_Report <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/google_mobility/Global_Mobility_Report_accessed_5_27_2020.csv", 
                                   col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                                    sub_region_1 = col_character(), 
                                                    sub_region_2 = col_character()))

CA_mobility_tillMay <- Global_Mobility_Report %>% filter(sub_region_1 == "California") %>%
  select(- c(country_region_code, country_region))

colnames(CA_mobility_tillMay ) <- c("state", "county", "date", "retail_and_recreation_change",
                                "grocery_and_pharmacy_change",
                                "parks_change", 
                                "transit_stations_change", 
                                "workplaces_change",
                                "residential_change")

# Make tidy data long format

CA_Mobility_long <- CA_mobility_tillMay %>% 
  gather(Mobility_Type, Percent_Change, -c(state, county, date))

CA_Mobility_long <- CA_Mobility_long %>% group_by(state, county, date, Mobility_Type) %>% 
  dplyr::summarise(Change = round(mean(Percent_Change, na.rm = TRUE),2)) %>%
  dplyr::mutate(Time_Line = ifelse(date >= "2020-03-31", "April-May", "Feb-March"))

CA_Mobility_long$Time_Line <- as.factor(CA_Mobility_long$Time_Line)
CA_Mobility_long$Mobility_Type <- as.factor(CA_Mobility_long$Mobility_Type)
CA_Mobility_long$county <- as.factor(CA_Mobility_long$county)
  
BA_Mobility <- CA_Mobility_long %>% filter(county %in% c("San Mateo County", "Santa Clara County", 
                                                       "Alameda County", "San Francisco County", "Marin County", 
                                                       "Contra Costa County", "Napa County", "Solano County"))
#factor reorder
BA_Mobility$Time_Line <- fct_inorder(BA_Mobility$Time_Line)

BA_Plot <- BA_Mobility %>%
  ggplot(aes(x = date, y = Change, color = Mobility_Type)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(county ~ Time_Line, scales = "free", labeller = labeller(county = label_wrap_gen(10))) +
  labs(title = "Reduction in Movement within Bay Area Counties",
       y = "Percentage Change in Mobility over Jan-Feb Base",
       x = "Time Line",
       caption = "Google LLC: Google COVID-19 Community Mobility Reports 
       https://www.google.com/covid19/mobility
       Accessed: 27th May 2020",
       color = "Mobility Category") +
  guides(x = guide_axis(angle = 90)) +
  scale_color_discrete(labels = c("Grocery and Pharmacy", "Parks", "Residential", 
                                  "Retail and Recreation", "Transit Stations", "Workplace")) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = rel(.8))) +
  geom_hline(yintercept = 0, linetype = "dotted")

