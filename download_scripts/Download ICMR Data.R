#Download ICMR Data from the web Note this is all in wide format

library(tidyverse); library(rvest); library(lubridate);library(gsubfn); library(parsedate) #library(rebus);

#Load saved df for India Testing Numbers
India_testing_numbers_old <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/India_testing_numbers.csv", 
                                  col_types = cols(X1 = col_skip()))

# Download New Data
URL <- "https://www.icmr.gov.in/"

icmr <- read_html(URL)

#extract total tests
total_tests <- icmr %>% html_nodes("h2") %>% magrittr::extract2(2) %>%
  html_text() %>% str_remove_all(",") %>% as.numeric()

# extract Date of latest numbers

as_of <- icmr %>% html_nodes("p") %>% magrittr::extract2(1) %>%
  html_text() %>% parsedate::parse_date()

# extract testing capacity

total_tests_pd <- icmr %>% html_nodes("h2") %>% magrittr::extract2(3) %>%
  html_text() %>% str_remove_all(",") %>% as.numeric()

# create temp df of parsed information

India_testing_numbers_new <- tibble(total_tests = total_tests, 
                                tests_per_day = total_tests_pd,
                                date = ymd(as_of))

# intersect with existing data

India_testing_numbers <- dplyr::union(India_testing_numbers_old, India_testing_numbers_new)

# update dataframe

write.csv(India_testing_numbers, 
          "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/India_testing_numbers.csv")

filename <- paste0("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/india_testing_data_", Sys.Date(), sep = "")

saveRDS(India_testing_numbers, file = filename)

# How tos
# https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/
# SelectorGadget - https://selectorgadget.com/
# rvest - https://rvest.tidyverse.org/reference/html_nodes.html
# extracting dates from strings - https://stackoverflow.com/questions/43405615/extract-date-from-given-string-in-r