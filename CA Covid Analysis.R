#Set up workspace
library(tidyverse)
#Get covid data from chhs
URL <- "https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv"
Covid_Data_Ca <- read.csv(URL,header = TRUE, sep = ",")
str(Covid_Data_Ca)

#Make data tidy ie create HStatistic
tidy_ca_data <- Covid_Data_Ca %>% 
  select(County.Name, Date = Most.Recent.Date, Confirmed.Total = Total.Count.Confirmed, Total.Deaths = Total.Count.Deaths, Daily.Positive = COVID.19.Positive.Patients) %>%
  gather(HStatistic, HValue, -c(Date,County.Name)) %>%
  mutate(Date = as.Date(Date, "%m/%d/%y"))
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

