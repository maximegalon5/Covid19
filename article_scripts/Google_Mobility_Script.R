# Google Mobility Data https://www.google.com/covid19/mobility/
# understand the Data - https://support.google.com/covid19-mobility/answer/9825414?hl=en&ref_topic=9822927
# Documentation and attribution - https://www.google.com/covid19/mobility/data_documentation.html?hl=en
# Load packages
library(tidyverse); library(plotly); #library(epitools)

# Load data
setwd("~/GitHub")
Global_Mobility_Report <- read_csv("~/GitHub/Covid19/data/google_mobility/Global_Mobility_Report_accessed_5_27_2020.csv", 
                                   col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                                    sub_region_1 = col_character(), 
                                                    sub_region_2 = col_character()))

# Create country list

# country_list <- unique(Global_Mobility_Report$country_region)

# Create US data subset

US_Mobility_Data <- Global_Mobility_Report %>% filter(country_region == "United States")

# remove duplicates?
#US_Mobility_Data <- distinct(US_Mobility_Data) %>% select(- c(sub_region_2, country_region_code))

US_Mobility_Data <- US_Mobility_Data %>% select(- sub_region_2) %>% select(- country_region_code)

# Change Col names (note percent change from baseline)

colnames(US_Mobility_Data) <- c("country", "state", "date", "retail_and_recreation_change",
                                "grocery_and_pharmacy_change",
                                "parks_change", 
                                "transit_stations_change", 
                                "workplaces_change",
                                "residential_change")
#how many NA's?

# Make tidy data long format

US_Mobility_Data_long <- US_Mobility_Data %>% 
  gather(Mobility_Type, Percent_Change, -c(date, state, country))

#write.csv(US_Mobility_Data_long,'US_Mobility_Data_long.csv')

# remove duplicates?
#US_Mobility_Data_long <- distinct(US_Mobility_Data_long)

US_Mobility_Data_long <- US_Mobility_Data_long %>% group_by(country, state, date, Mobility_Type) %>% 
  dplyr::summarise(Change = round(mean(Percent_Change, na.rm = TRUE),2)) %>%
  dplyr::mutate(Time_Line = ifelse(date >= "2020-03-31", "April-May", "Feb-March"))
  
US_Mobility_Data_long$Time_Line <- as.factor(US_Mobility_Data_long$Time_Line)
US_Mobility_Data_long$Mobility_Type <- as.factor(US_Mobility_Data_long$Mobility_Type)

#before April categorization
#test <- US_Mobility_Data_long
#test <- test %>% dplyr::mutate(Time_Line = ifelse(date >= "2020-03-31", "April-May", "March"))
#test$Time_Line <- as.factor(test$Time_Line)
#test$Mobility_Type <- as.factor(test$Mobility_Type)

# factor re order

US_Mobility_Data_long$Time_Line <- fct_inorder(US_Mobility_Data_long$Time_Line)
#plot_data <- test %>% filter(state == "California") # | state == "New York" | state == "Washington" | state == "Florida" |
                                                 #state == "Arizona" | state == "Illinois") %>%
CA_NY_Plot <- US_Mobility_Data_long %>%
   filter(state == "California" | state == "New York") %>%
   ggplot(aes(x = date, y = Change, color = Mobility_Type)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(state ~ Time_Line, scales = "free") +
  labs(title = "State of California Compared with NY",
       y = "Percentage Change in Mobility over Jan-Feb Base",
       x = "Time Line",
       caption = "Google LLC: Google COVID-19 Community Mobility Reports 
       https://www.google.com/covid19/mobility
       Accessed: 27th May 2020",
       color = "Mobility Category") +
   scale_color_discrete(labels = c("Grocery and Pharmacy", "Parks", "Residential", 
                                   "Retail and Recreation", "Transit Stations", "Workplace")) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = rel(.8))) +
  geom_hline(yintercept = 0, linetype = "dotted")



