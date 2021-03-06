library(readr)

Accessed_On <- "2020-07-13"
setwd("~/GitHub")
Mobility_Report <- read_csv("~/GitHub/Covid19/data/google_mobility/Global_Mobility_Report_2020_7_13.csv", 
                            col_types = cols(census_fips_code = col_skip(), 
                            country_region_code = col_skip(), 
                            date = col_date(format = "%Y-%m-%d"), 
                            iso_3166_2_code = col_skip(), sub_region_1 = col_character(), 
                            sub_region_2 = col_character()))

