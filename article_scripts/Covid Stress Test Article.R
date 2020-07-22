#Covid Stress Test Article
# Setup Work Space

library(tidyverse); library(lubridate); library(RColorBrewer); library(ggrepel); library(plotly)

# Load JHU World Covid Data from local source after downloading it from JHU

#source('C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/download_scripts/download_JHU_data.R')

#load_name <- paste0("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/world_covid_data_", Sys.Date(), sep = "")

#world_covid_data <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/world_covid_data_5_25_2020")

world_covid_data <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/world_covid_data_2020-05-31")

#Summary Deaths

total_deaths <- world_covid_data %>% 
 filter(type == "death") %>%
 group_by(country) %>%
summarise(total_deaths = sum(cases)) %>%
 arrange(-total_deaths)

total_deaths$country[1] <- "United States"

# Get and wrangle Obesity Data and convert it into a factor variable with 5% range

Obesity_Data <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/Obesity_Data.csv") # from the World Fact Book (https://www.cia.gov/library/publications/the-world-factbook/rankorder/2228rank.html)

#Round off numbers
Obesity_Data$Obesity_Percent <- round(Obesity_Data$`Obesity Percent`, 0)
Obesity_Data$Obesity_Percent_Group <- cut(Obesity_Data$Obesity_Percent, 
                                          c(0,5,10,15,20,25,30,35,40,45,50,55,60,65))
#Convert to Factors
Obesity_Data$Obesity_Percent_Group <- as.factor(Obesity_Data$Obesity_Percent_Group)

#Import Density of Population Data from the World bank - https://data.worldbank.org/indicator/SP.POP.TOTL
X2018_density_pop <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/2018_density_pop.csv", 
                              col_types = cols(`Country Code` = col_skip(), 
                                               `Series Code` = col_skip(), `Series Name` = col_skip()))

X2018_density_pop$`Country Name`[92] <- "Iran"
X2018_density_pop$`Country Name`[162] <- "Russia"


#Import Population Data (from the world bank - https://data.worldbank.org/indicator/EN.POP.DNST)

population_data <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/2018_population.csv", 
                            col_types = cols(`Country Code` = col_skip(), 
                                             `Series Code` = col_skip(), `Series Name` = col_skip()))

population_data$`Country Name`[92] <- "Iran"
population_data$`Country Name`[162] <- "Russia"

# sequential inner_joins to create the appropriate data set for plotting
# Total Deaths and Density of Popultion

df <- inner_join(total_deaths, X2018_density_pop, by =c("country" = "Country Name")) %>% 
  mutate(pop_density = round(as.numeric(`2018 [YR2018]`), 0)) %>%
  select(-`2018 [YR2018]`)


# df with Population Data  
df <- inner_join(df, population_data, by =c("country" = "Country Name")) %>% 
  mutate(population = round(as.numeric(`2018 [YR2018]`),0)) %>% 
  mutate(total_deaths_m = round(total_deaths / population * 1000000), 2) %>% select(-`2018 [YR2018]`)

# join obesity data

df <- inner_join(df, Obesity_Data, by = "country") %>% select(-`Obesity Percent`)

# format obesity_precent_group

df$Obesity_Percent_Group <- str_replace(df$Obesity_Percent_Group, "]", "")
df$Obesity_Percent_Group <- str_replace(df$Obesity_Percent_Group, "\\(", "")
df$Obesity_Percent_Group <- str_replace(df$Obesity_Percent_Group, ",", "-")

#saveRDS(df, file = "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/pop_density_covid_31_5_2020")

# plot data for countries having more that 400 deaths

plot1 <- df %>% filter(total_deaths >= 400) %>% 
  ggplot(aes(y = pop_density, x = total_deaths_m, color = Obesity_Percent_Group)) + 
  geom_text_repel(aes(label = country), size = rel(3.5), 
                  position = "jitter", show.legend = FALSE)  +
  geom_point() +
  labs(y = " Density of Population / sq.km (log scale)", x = "Total Number of Deaths per Million Population (log scale)", 
       title = "Deaths Due to Covid Per Million vs Density of Population", 
       subtitle = "Showing countries with more than 400 Deaths", 
       caption = "Country names of the same color have the same national obesity averages") +
  scale_y_log10() + 
  scale_x_log10() +
  theme_minimal() +
  scale_colour_discrete(name  ="Percentage of Obesity") +
  theme(legend.position = "bottom", title = element_text(size = rel(.9)),
        panel.border = element_rect(linetype = "solid", fill = NA,
                                    color = "grey80"))

# July Update

world_covid_data2 <- readRDS("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/backup/world_covid_data_2020-07-15")

#Summary Deaths

total_deaths2 <- world_covid_data2 %>% 
  filter(type == "death") %>%
  group_by(country) %>%
  summarise(total_deaths = sum(cases)) %>%
  arrange(-total_deaths)

total_deaths2$country[1] <- "United States"

df2 <- inner_join(total_deaths2, X2018_density_pop, by =c("country" = "Country Name")) %>% 
  mutate(pop_density = round(as.numeric(`2018 [YR2018]`), 0)) %>%
  select(-`2018 [YR2018]`)


# df with Population Data  
df2 <- inner_join(df2, population_data, by =c("country" = "Country Name")) %>% 
  mutate(population = round(as.numeric(`2018 [YR2018]`),0)) %>% 
  mutate(total_deaths_m = round(total_deaths / population * 1000000), 2) %>% select(-`2018 [YR2018]`)

# join obesity data

df2 <- inner_join(df2, Obesity_Data, by = "country") %>% select(-`Obesity Percent`)

# format obesity_precent_group

df2$Obesity_Percent_Group <- str_replace(df2$Obesity_Percent_Group, "]", "")
df2$Obesity_Percent_Group <- str_replace(df2$Obesity_Percent_Group, "\\(", "")
df2$Obesity_Percent_Group <- str_replace(df2$Obesity_Percent_Group, ",", "-")

df2 <- df2 %>% arrange(Obesity_Percent_Group)

# Add Mobility Data

library(readr)

Mobility_Report <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/google_mobility/Global_Mobility_Report_2020_7_13.csv", 
                            col_types = cols(census_fips_code = col_skip(), 
                                             country_region_code = col_skip(), 
                                             date = col_date(format = "%Y-%m-%d"), 
                                             iso_3166_2_code = col_skip(), sub_region_1 = col_character(), 
                                             sub_region_2 = col_character()))

Select_Mobility_Data <- Mobility_Report %>% dplyr::filter(is.na(sub_region_1)) %>% select(-sub_region_1, -sub_region_2) %>%
  select(country = country_region, date, stayed_home = residential_percent_change_from_baseline) %>% group_by(country) %>% 
  summarise(Ave_home_stay = round(mean(stayed_home),2))

df2 <- inner_join(df2, Select_Mobility_Data, by = "country")

# plot data for countries having more that 15 deaths per M - July

plot2 <- df2 %>% filter(total_deaths_m >= 150) %>% 
  ggplot(aes(y = pop_density, x = total_deaths_m, color = Obesity_Percent_Group)) + 
  geom_text_repel(aes(label = country), size = rel(3), 
                  nudge_y = -15, show.legend = FALSE)  +
  geom_text_repel(aes(label = Ave_home_stay), size = rel(3), 
                  nudge_y = 15, show.legend = FALSE) +
  geom_point() +
  ylim(-100,600) +
  labs(y = " Density of Population / sq.km", x = "Total Number of Deaths Per M Population", 
       title = "Deaths Due to Covid Per Million vs Density of Population - July 15th", 
       subtitle = "Countries > 150 Deaths Per M Population + Average increase in Residential Mobility Category", 
       caption = "Similarly colored countries have the same national obesity averages | Pneumonia kills ~ 150 / M Population") +
  theme_minimal() +
  scale_colour_discrete(name  ="Percentage of Obesity") +
  theme(legend.position = "bottom", title = element_text(size = rel(.9)),
        panel.border = element_rect(linetype = "solid", fill = NA,
                                    color = "grey80"))

