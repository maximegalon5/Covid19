#Set up workspace
library(tidyverse)
#Get covid data from chhs
#URL <- "https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv"
#Covid_Data_Ca <- read.csv(URL,header = TRUE, sep = ",")
#str(Covid_Data_Ca)


#CHHS Hospital Surge Data

Surge_URL <- "https://data.ca.gov/dataset/cbbfb307-ac91-47ec-95c0-f05684e06065/resource/ef6675e7-cd3a-4762-ba75-2ef78d6dc334/download/bed_surge.csv"

#CHHS Hospital By County Data

Hosp_URL <- "https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv"

Case_URL <- "https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv"


#Bed_capacity <- read.csv(Surge_URL,header = TRUE, sep = ",")

#make sense of Bed_capacity data


CA_Case_Data <- read.csv(Case_URL, header = TRUE, 
                         colClasses = c("character", "numeric", "numeric", 
                                                                 "numeric", "numeric", "Date"), sep = ",")


#Make data tidy ie create HStatistic
tidy_ca_data <- CA_Case_Data %>% 
  select(County.Name = county, Date = date, Confirmed.Total = totalcountconfirmed, 
         Total.Deaths = totalcountdeaths, Daily.Positive = newcountconfirmed) %>%
  gather(HStatistic, HValue, -c(Date,County.Name))
str(tidy_ca_data)

#bay area tidy

tidy_ba <- tidy_ca_data %>% 
  filter(County.Name == c("San Mateo", "Santa Clara", "Alameda", "San Francisco", "Marin", "Contra Costa", "Napa", "Solano"))

#plot object
plot_object <- tidy_ba %>% 
  ggplot(aes(x = Date, y = HValue, color = HStatistic)) +
  geom_line() + scale_y_log10() + facet_grid(vars(HStatistic), vars(County.Name), 
                                             scales = "free_y", labeller = labeller(County.Name = label_wrap_gen(10))) +
  labs(title = "Covid Case Numbers and Fatalities in the Bay Area",
       x = "Date", y = "Statistic Value") +
  guides(x = guide_axis(angle = 90)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = rel(1)))
 
#plotly print plot object
#ggplotly(plot_object)

