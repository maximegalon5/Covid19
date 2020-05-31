#plotly

#save(df, file = "world_covid_data_5_25_2020.RData", compress = TRUE)
#df <- load("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/world_covid_data_5_25_2020.RData")


fig <- df %>%
  plot_ly(x = ~ total_deaths_m, y = ~ pop_density,
          color = ~ Obesity_Percent_Group,
          colors = "Dark2",
          hoverinfo = "text",
          text = ~paste("Population Density/sq.km:", pop_density, "<br>",
                        "Total Deaths Per Million:", total_deaths_m, "<br>",
                        "Country", country, "<br>",
                        "Obesity %", Obesity_Percent)) %>%
  add_markers(marker = list(size = 10)) %>%
  layout(title = "Interactive plot of Covid Deaths vs Density of Population",
         xaxis = list(title = "Total deaths per million (logscale)",
                      type = "log"),
         yaxis = list(title = "Density of Population / sq.km (logscale)",
                      type = "log"),
         legend = list(title=list(text='Average Obesity %'))
  )