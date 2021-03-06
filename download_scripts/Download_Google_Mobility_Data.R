library(readr)
        
URL <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=d58b419300b3a884"

Mobility_Report <- read_csv(URL, 
                            col_types = cols(census_fips_code = col_skip(), 
                                             country_region_code = col_skip(),
                                             metro_area = col_skip(),
                                             date = col_date(format = "%Y-%m-%d"), 
                                             iso_3166_2_code = col_skip(), sub_region_1 = col_character(), 
                                             sub_region_2 = col_character()))

setwd("~/GitHub")

write.csv(Mobility_Report, "~/GitHub/Covid19/data/google_mobility/Global_Mobility_Report.csv")