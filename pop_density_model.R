library(tidyverse); library(broom); library(GGally)

pop_density_covid <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/pop_density_covid_31_5_2020")

mobility_data <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/Google_Mobility_Data_2020-06-01")




mobility_data <- mobility_data %>% 
  dplyr::filter(date > "2020-03-31") %>%
  dplyr::select(- c(sub_region_2, country_region_code)) %>%
  dplyr::group_by(country_region) %>%
  dplyr::summarise(Retail_Recreation = round(mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE), 0),
                   Groceries_Pharmacy = round(mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE), 0),
                   Parks = round(mean(parks_percent_change_from_baseline, na.rm = TRUE),0),
                   Transit = round(mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),0),
                   Workplace = round(mean(workplaces_percent_change_from_baseline, na.rm = TRUE),0),
                   Residential = -round(mean(residential_percent_change_from_baseline, na.rm = TRUE),0))

mobility_data <- transform(mobility_data, Average_Mobility = round(rowMeans(mobility_data[,-1], na.rm = TRUE),0))

mobility_data <- mobility_data %>% arrange(Average_Mobility) %>% select(country_region, Average_Mobility)

model_data <- inner_join(pop_density_covid, mobility_data, by = (c("country" = "country_region")))

model_data <- model_data %>% select(country, pop_density, total_deaths_m, Obesity_Percent, Obesity_Percent_Group, Average_Mobility) %>% 
  filter(total_deaths_m > 1) %>%
  mutate(pop_density_scale = scale(pop_density), 
         total_deaths_m_scale = scale(total_deaths_m), 
         mobility_scale = scale(Average_Mobility),
         obesity_scale = scale(Obesity_Percent),
         index_study = pop_density_scale + obesity_scale)


model_data %>% select(-country) %>% GGally::ggpairs()







mod <- lm(total_deaths_m ~ pop_density_scale + mobility_scale + factor(Obesity_Percent_Group), data = model_data)

summary(mod)

augment(mod)

data_20 <- pop_density_covid %>% filter(Obesity_Percent > 20 & Obesity_Percent < 35)

mod <- lm(total_deaths_m ~ pop_density, data = data_20)

summary(mod)


#plot 1

model_data %>% ggplot(aes(total_deaths_m_scale, index_study)) +
  geom_point(aes(color = Obesity_Percent_Group)) +
  scale_x_log10() +
  scale_x_log10() +
  stat_smooth()

