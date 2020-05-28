# India Google Mobility Data

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

#write.csv(US_Mobility_Data_long,'US_Mobility_Data_long.csv')

# remove duplicates?
#US_Mobility_Data_long <- distinct(US_Mobility_Data_long)

India_Mobility_Data_long <- India_Mobility_Data_long %>% group_by(country, state, date, Mobility_Type) %>% 
  dplyr::summarise(Change = round(mean(Percent_Change, na.rm = TRUE),2)) %>%
  dplyr::mutate(Time_Line = ifelse(date >= "2020-03-31", "April-May", "Feb-March"))

India_Mobility_Data_long$Time_Line <- as.factor(India_Mobility_Data_long$Time_Line)
India_Mobility_Data_long$Mobility_Type <- as.factor(India_Mobility_Data_long$Mobility_Type)

# factor re order

India_Mobility_Data_long$Time_Line <- fct_inorder(India_Mobility_Data_long$Time_Line)

plot2 <- India_Mobility_Data_long %>%
  filter(state == "Maharashtra" | state == "Delhi") %>%
  ggplot(aes(x = date, y = Change, color = Mobility_Type)) +
  geom_boxplot() +
  theme_bw() +
  facet_grid(state ~ Time_Line, scales = "free_x") +
  labs(title = "States of MH and DEL Google Mobility Data",
       y = "Percentage Change in Mobility over Jan-Feb Base",
       x = "Time Line",
       caption = "Google LLC: Google COVID-19 Community Mobility Reports 
       https://www.google.com/covid19/mobility/
       Accessed: 27th May 2020",
       color = "Mobility Category") +
  scale_color_discrete(labels = c("Grocery and Pharmacy", "Parks", "Residential", "Retail and Recreation", "Transit Stations", "Workplace"))

plot2