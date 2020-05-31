# India Baseline Article Plot

# RBI Density of Population Data

library(rvest); library(tidyverse); library(ggrepel); library(plotly)

#load data from india_base_line.R

#source('C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/download_scripts/India_baseline_data.R')

ind_df <- readRDS(ind_df, file = "C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/india_baseline_data")

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

