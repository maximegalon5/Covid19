#India cumulative data

#source global data

source('C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/download_scripts/download_JHU_data.R')

#create cum sums of type

df <- world_covid_data %>% pivot_wider(names_from = type, values_from = cases)

India_cum_data <- df %>% filter(country == "India") %>% 
  mutate(active = confirmed -(recovered + death),
         cum_confirmed = cumsum(confirmed), 
         cum_dead = cumsum(death), 
         cum_recovered = cumsum(recovered),
         cum_active = cumsum(active),
         growth_rate = (cum_confirmed / lag(cum_confirmed)) - 1,
         doubling_period = round(log(2)/(growth_rate), 0)) %>% 
  select(-c(date, lat, long, province, country))

rm(df)

India_cum_data %>% tail(10)

# make long

India_cum_data_long <- India_cum_data %>% pivot_longer(-Date, names_to = "HStatistic",
                                        values_to = "value")

#rm(India_cum_data)

plot_object <- India_cum_data_long %>% filter(HStatistic %in% c("cum_confirmed", "cum_dead", "cum_recovered")) %>%
  ggplot(aes(x = Date, y = value, color = HStatistic)) +
  geom_line() + scale_y_log10() + 
  theme_minimal() +
theme(legend.position = "bottom",
      strip.text = element_text(size = rel(1))) +
  labs(title = "Covid Case Numbers and Fatalities in India",
       x = "Date", y = "Case Numbers")
  
  

  
  
  
# Growth rate and doubling time (https://data.princeton.edu/eco572/grdt)