#Get CA Cases and Mobility

#Set up workspace
library(tidyverse); library(lubridate); library(descr); library(corrplot); library(jtools)
library(ggpubr)
#CHHS Case Data
Case_URL <- "https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv"



CA_Case_Data <- read.csv(Case_URL, header = TRUE, 
                         colClasses = c("character", "numeric", "numeric", 
                                        "numeric", "numeric", "Date"), sep = ",")


#Create features
CA_Cases <- CA_Case_Data %>%
  mutate(Case_Growth = newcountconfirmed/lag(totalcountconfirmed) * 100) %>% mutate(Date = date, County = county) %>%
  dplyr::select(Date, County, Case_Growth) %>% na.omit()

#Using a function to create Growth Rate
#CA_Cases$Growth_Rate <- 0

#Create_Growth_Rate <- function(x) {
  #ifelse(x$County == lag(x$County), x$Growth_Rate <- x$newcountconfirmed/lag(x$totalcountconfirmed) * 100, NA)
#}

#CA_Cases$Growth_Rate <- Create_Growth_Rate(CA_Cases)

# Making sure County Numbers don't overlap

Check_County <- function(x) {
ifelse(x$County == lag(x$County), x$Case_Growth <- x$Case_Growth, NA)
}

CA_Cases$Case_Growth <- Check_County(CA_Cases)

###Make function for R_Growth

BA_Cases <- CA_Cases %>% filter(County %in% c("San Mateo", "Santa Clara", 
                                                         "Alameda", "San Francisco", "Marin", 
                                                         "Contra Costa", "Napa", "Solano")) %>%
  filter(Date > "2020-03-31")

#Creating a Lead of Case Rates by 10 days

BA_Cases$Lead10 <- 0

Create_Lead <- function(x) {
  ifelse(x$County == lead(x$County, 10), x$Lead10 <- lead(x$Case_Growth, 10), NA)
}

BA_Cases$Lead10 <- Create_Lead(BA_Cases)

BA_Cases <- BA_Cases %>% dplyr::select(-Case_Growth) %>% na.omit()

#CA Mobility data

#Load Mobility Data
Mobility_Report <- read_csv("C:/Users/Vivek/SkyDrive/Documents/GitHub/Covid19/data/google_mobility/Global_Mobility_Report.csv", 
                            col_types = cols(X1 = col_skip(), date = col_datetime(format = "%Y-%m-%d"), 
                                             sub_region_1 = col_character(), sub_region_2 = col_character()))

#Rename cols
colnames(Mobility_Report) <- c("Country", "State", "County", "Date", "Retail_Recreation",
                               "Grocery_Pharmacy",
                               "Parks", 
                               "Transit", 
                               "Workplace",
                               "Residential")

CA_Mobility <- Mobility_Report %>% filter(State == "California") %>% 
  dplyr::mutate(`County` = replace_na(`County`, "State")) %>% select(-Country, -State)

CA_Mobility$County <- str_replace(CA_Mobility$County, " County", "")

CA_Mobility$Date <- as.Date(CA_Mobility$Date)


CA_Data <- full_join(CA_Mobility, BA_Cases, by = c("Date", "County")) %>% na.omit() #%>% dplyr::select(-County, -Date)

#Alameda <- CA_Data %>% filter(County == "Alameda") %>% dplyr::mutate(R_Growth = Residential/lag(Residential) * 100) %>% na.omit()

#Correlation between Residential and Case Growth

ggscatter(Alameda, y = "Lead10", x = "Residential", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          ylab = "Case Growth", xlab = "Change in Residential Mobility")

#Cluster / Correlate Change in mobility to change in new cases + 10 days

test <- CA_Data %>% scale() %>% cor() %>% corrplot(order = "hclust", hclust.method = "average",
                                           method = "color", diag = F,
                                           tl.col = "gray20",
                                           sig.level = T, addCoef.col = "white",
                                           number.cex = .6, type = "upper",
                                           tl.cex = 0.8)

mod <- lm(Lead10 ~ Residential, data = Alameda) ; summary(mod)

plot(CA_Data)
abline(mod)

#Compare within Counties

#Compare with SoCal ie San Francisco with LA and San Diego and Bay Area with Greater LA

#Compare with Mandatory Mask Wearing - parallel slopes model


#GGPlot
Alameda %>% ggplot + geom_smooth(aes(x = Date, y = Residential), span = 0.25, color = "red") + 
  geom_smooth(aes(x = Date, y = Lead10), span = .25, color = "blue") + 
  geom_smooth(aes(x = Date, y = Workplace), span = .25, color = "green")
