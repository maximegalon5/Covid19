# India Google Mobility Data
# Load Packages
library(tidyverse);
# Load Global Data
Global_Mobility_Report <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/google_mobility/Global_Mobility_Report_accessed_5_27_2020.csv", 
                                   col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                                    sub_region_1 = col_character(), 
                                                    sub_region_2 = col_character()))
# Create India Mobility Subset
India_Mobility_Data <- Global_Mobility_Report %>% filter(country_region == "India") %>% 
  select(- sub_region_2) %>% 
  select(- country_region_code)

colnames(India_Mobility_Data) <- c("country", "state", "date", "retail_and_recreation_change",
                                "grocery_and_pharmacy_change",
                                "parks_change", 
                                "transit_stations_change", 
                                "workplaces_change",
                                "residential_change")

India_Mobility_Data_long <- India_Mobility_Data %>% 
  gather(Mobility_Type, Percent_Change, -c(date, state, country))



# remove duplicates?
#US_Mobility_Data_long <- distinct(US_Mobility_Data_long)

India_Mobility_Data_long <- India_Mobility_Data_long %>% group_by(country, state, date, Mobility_Type) %>% 
  dplyr::summarise(Change = round(mean(Percent_Change, na.rm = TRUE),2)) %>%
  dplyr::mutate(Time_Line = ifelse(date >= "2020-03-31", "April-May", "Feb-March"))

India_Mobility_Data_long$Time_Line <- as.factor(India_Mobility_Data_long$Time_Line)
India_Mobility_Data_long$Mobility_Type <- as.factor(India_Mobility_Data_long$Mobility_Type)

# factor re order

India_Mobility_Data_long$Time_Line <- fct_inorder(India_Mobility_Data_long$Time_Line)

#write india long 
#st <- Sys.Date()
#fname <- paste("India_Mobility_Long_",st, ".csv", sep = "")
#write.csv(India_Mobility_Data_long, fname)

plot2 <- India_Mobility_Data_long %>%
  filter(state == "Maharashtra" | state == "Delhi" | state == "Kerala") %>%
  ggplot(aes(x = date, y = Change, color = Mobility_Type)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(state ~ Time_Line, scales = "free_x") +
  labs(title = "States of Maharashtra and Delhi | Google Mobility Data",
       y = "Percentage Change in Mobility over Jan-Feb Base",
       x = "Time Line",
       caption = "Google LLC: Google COVID-19 Community Mobility Reports 
       https://www.google.com/covid19/mobility/
       Accessed: 27th May 2020",
       color = "Mobility Category") +
  scale_color_discrete(labels = c("Grocery and Pharmacy", "Parks", "Residential", 
                                  "Retail and Recreation", "Transit Stations", "Workplace")) +
  geom_hline(yintercept = 0, linetype = "dotted")

plot2