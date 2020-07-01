#Set up workspace
library(tidyverse)

#CHHS Case Data
Case_URL <- "https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv"



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
  geom_line() + facet_grid(vars(HStatistic), vars(County.Name), 
                                             scales = "free_y", labeller = labeller(County.Name = label_wrap_gen(10))) +
  labs(title = "Covid Case Numbers and Fatalities in the Bay Area",
       x = "Date", y = "Statistic Value") +
  guides(x = guide_axis(angle = 90)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = rel(1)))
 
#plotly print plot object
#ggplotly(plot_object)

