#Get covid data from chhs
URL <- "https://data.chhs.ca.gov/dataset/6882c390-b2d7-4b9a-aefa-2068cee63e47/resource/6cd8d424-dfaa-4bdd-9410-a3d656e1176e/download/covid19data.csv"
Covid_Data_Ca <- read.csv(URL,header = TRUE, sep = ",")
str(Covid_Data_Ca)

#Make data tidy ie create HStatistic
tidy_ca_data <- Covid_Data_Ca %>% 
  select(County.Name, Most.Recent.Date,Total.Count.Confirmed, Total.Count.Deaths, COVID.19.Positive.Patients) %>%
  gather(HStatistic, HValue, -c(Most.Recent.Date,County.Name)) %>%
  mutate(Most.Recent.Date = as.Date(Most.Recent.Date, "%m/%d/%y"))
str(tidy_ca_data)

tidy_ca_data %>% 
  filter(County.Name == c("San Mateo", "Santa Clara", "Alameda", "San Francisco", "Marin", "Contra Costa", "Napa", "Solano")) %>%
  ggplot(aes(x = Most.Recent.Date, y = HValue, color = HStatistic)) +
  geom_line() + scale_y_log10() + facet_grid(. ~ County.Name) +
  theme_minimal()




