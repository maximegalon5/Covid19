library(readr)

Accessed_On <- "2020-07-13"

Mobility_Report <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/google_mobility/Global_Mobility_Report_2020_7_13.csv", 
                            col_types = cols(census_fips_code = col_skip(), 
                            country_region_code = col_skip(), 
                            date = col_date(format = "%Y-%m-%d"), 
                            iso_3166_2_code = col_skip(), sub_region_1 = col_character(), 
                            sub_region_2 = col_character()))

#URL <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=d58b419300b3a884"

#Mobility_Report <- read_csv(URL, 
                            #col_types = cols(census_fips_code = col_skip(), 
                                             #country_region_code = col_skip(), 
                                             #date = col_date(format = "%Y-%m-%d"), 
                                             #iso_3166_2_code = col_skip(), sub_region_1 = col_character(), 
                                             #sub_region_2 = col_character()))

#write.csv(Mobility_Report, "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/google_mobility/Global_Mobility_Report.csv")

