library(tidyverse); library(readr)

Global_Mobility_Report <- read_csv("data/google_mobility/Global_Mobility_Report_2020_7_13.csv", 
                                               col_types = cols(census_fips_code = col_skip(), 
                                                              country_region_code = col_skip(), 
                                                              iso_3166_2_code = col_skip()))




Swe_Mobility_Data <- Global_Mobility_Report %>% filter(country_region == "Sweden") %>% 
  dplyr::select(- sub_region_2) %>% filter(is.na(sub_region_1)) %>% select(-sub_region_1)

colnames(Swe_Mobility_Data) <- c("country", "date", "retail_and_recreation_change",
                                   "grocery_and_pharmacy_change",
                                   "parks_change", 
                                   "transit_stations_change", 
                                   "workplaces_change",
                                   "residential_change")

Swe_Mobility_Data_long <- Swe_Mobility_Data %>% 
  gather(Mobility_Type, Percent_Change, -c(date, country))

Swe_Mobility_Data_long %>% ggplot(aes(x = date, y = Percent_Change, color = Mobility_Type)) + geom_line() + theme_light()