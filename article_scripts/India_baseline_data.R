#India Baseline data

#load data from the RBI and from MoHFW
state_india <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/India_States_Covid_5_26.csv")
state_india_pop_density <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/state_india_pop_density.csv")
state_india_population <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/india_state_population.csv")
#infant mortality data
Infant_mort <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/Infant_mort_states_india.csv")

Infant_mort$Infant_Mort_F <- cut(Infant_mort$Infant_Mortality, 
                                 c(0,10,15,20,25,30,35,40,45,50))
#Convert to Factors
Infant_mort$Infant_Mort_F <- as.factor(Infant_mort$Infant_Mort_F)

#inner join covid data to population
ind_df <- inner_join(state_india, state_india_population, by =c("State" = "State"))

#inner join ind_df to pop_density
ind_df <- inner_join(ind_df, state_india_pop_density, by =c("State" = "State"))

#inner join ind_df with infant mortality data
ind_df <- inner_join(ind_df, Infant_mort, by =c("State" = "State"))

ind_df$Infant_Mort_F <- str_replace(ind_df$Infant_Mort_F, "]", "")
ind_df$Infant_Mort_F <- str_replace(ind_df$Infant_Mort_F, "\\(", "")
ind_df$Infant_Mort_F <- str_replace(ind_df$Infant_Mort_F, ",", "-")

ind_df$State[9] <- "Delhi NCR"

ind_df$population <- ind_df$population *1000
ind_df$death_1000K <- round(((ind_df$Deaths / ind_df$population) * 1000000),4)

#saveRDS(ind_df, file = "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/india_baseline_data")

#source('C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/download_india_state_data.R')