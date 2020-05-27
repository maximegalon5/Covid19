# India Data

# RBI Density of Population Data

library(rvest); library(tidyverse); library(ggrepel); library(plotly)

#URL <- "https://www.mohfw.gov.in/"
#load data from the RBI and from MoHFW
state_india <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/India_States_Covid_5_26.csv")
state_india_pop_density <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/state_india_pop_density.csv")
state_india_population <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/india_state_population.csv")
#infant mortality data
Infant_mort <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/Infant_mort_states_india.csv")

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

ind_df$population <- ind_df$population *1000
ind_df$death_1000K <- round(((ind_df$Deaths / ind_df$population) * 1000000),4)

ind_df$State[9] <- "Delhi NCR"

#ggplot

plot1 <- ind_df %>%
  ggplot(aes(y = Pop_density, x = death_1000K, color = Infant_Mort_F)) + 
  geom_text_repel(aes(label = State), size = rel(3.5), 
                  position = "jitter", show.legend = FALSE)  +
  geom_point() +
  labs(y = "Density of Population / sq.km (log scale)", x = "Total Number of Deaths per Million Population", 
       title = "Deaths Due to Covid Per Million vs Density of Population", 
       subtitle = "Showing All States and Union Territories classified by Infant Mortality", 
       caption = "State or Union Territories of the same color have similar Infant Mortality Rates") +
    scale_y_log10() + 
  # scale_x_log10() +
  theme_minimal() +
  scale_colour_discrete(name  ="Infant Mortality Per 1000",
                        labels=c("Under 10", "10 - 15", "15 - 20", "20 - 25", "25 - 30", "30 - 35", 
                                 "35 - 40", "40 - 50", "45 - 50")) +
  theme(legend.position = "bottom", title = element_text(size = rel(.9)),
        panel.border = element_rect(linetype = "solid", fill = NA,
                                    color = "grey80"))

#plotly

fig <- ind_df %>%
  plot_ly(x = ~ death_1000K, y = ~ Pop_density,
          color = ~ Infant_Mort_F,
          colors = "Dark2",
          hoverinfo = "text",
          text = ~paste("Population Density/sq.km:", Pop_density, "<br>",
                        "Total Deaths Per Million:", death_1000K, "<br>",
                        "State/UT", State, "<br>",
                        "Infant Mortality per 1000", Infant_Mortality)) %>%
  add_markers(marker = list(size = 10)) %>%
  layout(title = "Interactive plot of Covid Deaths vs Density of Population",
         xaxis = list(title = "Total deaths per million"),
         yaxis = list(title = "Density of Population / sq.km (logscale)",
                      type = "log"),
         legend = list(title=list(text='Infant Mortality per 1000'))
  )

