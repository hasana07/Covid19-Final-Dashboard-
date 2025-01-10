library(flexdashboard)
# Main source code for BTC1855 Final
# Authors: Hasan Abdo, Angela Bakaj, Youssef Emam
# Date: Aug. 29th, 2024
# Note for reviewer: Please set the working directory to the "datasets" folder.
#import libraries
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(wbstats)
library(leaflet)
library(htmltools)
library(flexdashboard)

#making sure data in the api is the most recent version
#used for wb api queries 
new_cache <- wb_cache()

#Target dataset that we want for our visualizations (date range of 2020-2022)

# country | Population | Confirmed cases |Confirmed Cases per 100K | Deaths | Deaths per 100K| Indicator 1 | Indicator 2 | Indicator 3 | Indicator 4 | Indicator 5

#start by getting countries and cases
#Read covid19 data
covid <- read.csv("datasets/WHO-COVID-19-global-data-daily.csv", header = T)

#Create df with all unique countries and country codes

countries <- data.frame(Country_code = unique(covid$Country_code), Country = unique(covid$Country))

#Summarise the data
summary(covid)

#convert dates to date format
covid$Date_reported <- ymd(covid$Date_reported)

#Keep data within correct date range
covid <- covid %>% 
  filter(Date_reported >= "2020-01-1", Date_reported <= "2022-12-31") %>%
  
  #summarize each column by calculating cumulative values
  group_by(Country_code) %>% 
  summarise(confirmed_cases = sum(New_cases, na.rm  = T),
            deaths = sum(New_deaths, na.rm = T))



#Join each country to it's row to add names not just codes
covid <- left_join(covid, countries, by = join_by("Country_code" == "Country_code"))


#Now we have:
# country code | Cumulative Confirmed cases | Cumulative Deaths | country
# We will keep country code to join on since it is standardized
# Country names may be spelled differently (i.e with or without special characters)

#lets add population
#get data from WB using wbstats
populations <- wb_data(indicator = "SP.POP.TOTL", cache = new_cache) %>% 
  
  #summarise by the average population over the time period
  filter(date >= 2020, date <= 2023) %>%
  group_by(iso2c) %>% 
  summarise(population = as.integer(mean(SP.POP.TOTL, na.rm = T)))

# join population to the dataset
covid <- left_join(covid, populations, by = join_by("Country_code" == "iso2c"))


# now we have: 
# country code | Confirmed cases | Deaths | country | Population
# lets add confirmed cases per 100K
covid <- covid %>% 
  mutate(cases_per_100K = confirmed_cases/population*100000,
         deaths_per_100k = (deaths/population*100000))


#Now we will add indicators to the data frame

#indicators of GNI
gni_inds <- data.frame(wb_search(pattern ="GNI", cache = new_cache))

#Use current GNI per capita in USD
gni_data <- wb_data("NY.GNP.PCAP.CD", cache = new_cache)

options(scipen = 999)

#summarise each country by average DNI using atlas method
gni_data <- gni_data %>%
  filter(date >= 2020, date <= 2023) %>% 
  group_by(iso2c) %>% 
  summarise(GNI = (mean(NY.GNP.PCAP.CD, na.rm = T)))

#join to main dataset
covid <- left_join(covid, gni_data, join_by("Country_code" == "iso2c"))

# population density
pop_inds <- wb_search("EN.POP.DNST", cache = new_cache)

#Summarise each country by average population density
density_data <- wb_data("EN.POP.DNST", cache = new_cache) %>% 
  filter(date >= 2020, date <= 2023) %>% 
  group_by(iso2c) %>% 
  summarise(Population_density = (mean(EN.POP.DNST, na.rm = T)))

#join to main data
covid <- left_join(covid, density_data, join_by("Country_code" == "iso2c"))

# health indicator
health_inds <- wb_search("Health", cache = new_cache)
# SH.XPD.CHEX.PC.CD - Healthcare expenditure per capita (USD)

#Summarise each country by  Health care expenditure
health_data <- wb_data("SH.XPD.CHEX.PC.CD", cache = new_cache) %>% 
  filter(date >= 2020, date <= 2023) %>% 
  group_by(iso2c) %>% 
  summarise(Health_expenditure = (mean(SH.XPD.CHEX.PC.CD, na.rm = T)))

#join to main data
covid <- left_join(covid, health_data, join_by("Country_code" == "iso2c"))


#### Data processing ####
#Creating columns where we calculate prevalence, Case Fatality Rate, and Mortality Rate
#CFR = Case Fatality Rate
#MR= Mortality Rate

covid <- covid %>% 
  mutate(prevelance = (confirmed_cases/population)*100,
         CFR = deaths/confirmed_cases,
         MR = deaths/population)


library(corrplot)
library(Hmisc)
#setting up correlation variables that will be used the plot

selected_variables <- covid %>% 
  select(CFR, deaths, GNI, Population_density, Health_expenditure)

#create correlation matrix for Indictors
#computes correlation values and p-values for the data
cor_plot <- rcorr(as.matrix(selected_variables))

#clean the matrix to remove redundancy

cor_plot$r <- as.matrix(cor_plot$r[1:2, c(-1,-2)])
cor_plot$P <- as.matrix(cor_plot$P[1:2, c(-1,-2)])


#plot the matrix, hide insignificant results (i.e H0: pearson coefficient = 0)
corrplot(cor_plot$r, method = "shade", title = "indicator corplot", p.mat = cor_plot$P, sig.level = 0.05,tl.col = "black", tl.srt = 50, cl.pos = "b", cl.ratio = 0.7)

#top 10 cases per 100k
processing <- covid %>% 
  arrange(desc(cases_per_100K)) %>% 
  na.omit() %>% 
  head(10) 

#total cases per 100k is less than 10,000 
processing2 <- covid %>% 
  na.omit() %>% 
  filter(cases_per_100K < 10000)

#top 10 deaths per 100k
processing3 <- covid %>% 
  arrange(desc(deaths_per_100k)) %>% 
  na.omit() %>% 
  head(10) 


#total deaths per 100k is less than 250
processing4 <- covid %>% 
  na.omit() %>% 
  filter(deaths_per_100k < 250) 

#Based on the above queries, we selected the countries: France, Austria, Denmark, India, Congo, Egypt, Peru, Bulgaria, Georgia, Canada, Turkiye, Australia

covid$Country[covid$Country == "T�rkiye"] <- "Türkiye"

selected_countries <- c("Slovenia", "Austria", "Denmark", "India", "Congo", "Egypt", "Peru", "Bulgaria", "Georgia", "Canada", "Türkiye", "Australia")

covid_selected <- covid %>% 
  filter(Country %in% selected_countries)



##### Country Categories #####

#Starting off with the GNI categories, based on World Bank classifications for 2021-2022
#Found here: https://blogs.worldbank.org/en/opendata/new-world-bank-country-classifications-income-level-2021-2022

#The categories for population density were based off of thresholds found here: https://blogs.worldbank.org/en/sustainablecities/how-do-we-define-cities-towns-and-rural-areas

#Calculating quantiles as categories for Health Expenditure 
quantiles <- quantile(covid_selected$Health_expenditure, probs = c(0.25, 0.50, 0.75))


#Creating GNI, population density, and health expenditure thresholds based on the values reported above 
covid_thresholds <- covid_selected %>% 
  group_by(Country) %>% 
  mutate(income_group = case_when(
    GNI < 1045 ~ "Low Income",
    GNI >= 1046 & GNI <= 4095 ~ "Lower-Middle Income",
    GNI >= 4096 & GNI <= 12695 ~ "Upper-Middle Income",
    GNI > 12695 ~ "High Income"
  )) %>% 
  mutate(density_category = case_when(
    Population_density < 300 ~ "Low Population Density",
    Population_density >= 301 ~ "High Population Density"
  )) %>% 
  mutate(health_expenditure_category = case_when(
    Health_expenditure < quantiles[1] ~ "Low Expenditure",
    Health_expenditure >= quantiles[1] & Health_expenditure <= quantiles[3] ~ "Medium Expenditure",
    Health_expenditure > quantiles[3] ~ "High Expenditure"
  )) %>% 
  ungroup()


#converting the values to factors 
covid_thresholds$income_group <- factor(covid_thresholds$income_group, levels = c("High Income", "Upper-Middle Income", "Lower-Middle Income", "Low Income"))

covid_thresholds$density_category <- factor(covid_thresholds$density_category, levels = c("High Population Density", "Low Population Density"))


covid_thresholds$health_expenditure_category <- factor(covid_thresholds$health_expenditure_category, levels = c("High Expenditure", "Medium Expenditure", "Low Expenditure"))

#repeat process for df with all countries
covid <- covid %>% 
  group_by(Country) %>% 
  mutate(income_group = case_when(
    GNI < 1045 ~ "Low Income",
    GNI >= 1046 & GNI <= 4095 ~ "Lower-Middle Income",
    GNI >= 4096 & GNI <= 12695 ~ "Upper-Middle Income",
    GNI > 12695 ~ "High Income"
  )) %>% 
  mutate(density_category = case_when(
    Population_density < 300 ~ "Low Population Density",
    Population_density >= 301 ~ "High Population Density"
  )) %>% 
  mutate(health_expenditure_category = case_when(
    Health_expenditure < quantiles[1] ~ "Low Expenditure",
    Health_expenditure >= quantiles[1] & Health_expenditure <= quantiles[3] ~ "Medium Expenditure",
    Health_expenditure > quantiles[3] ~ "High Expenditure"
  )) %>% 
  ungroup()

#converting the values to factors 
covid$income_group <- factor(covid$income_group, levels = c("High Income", "Upper-Middle Income", "Lower-Middle Income", "Low Income"))

covid$density_category <- factor(covid$density_category, levels = c("High Population Density", "Low Population Density"))


covid$health_expenditure_category <- factor(covid$health_expenditure_category, levels = c("High Expenditure", "Medium Expenditure", "Low Expenditure"))


##### Plots #####

#summarize data, find mean CFR and SD
summary_data <- covid_thresholds %>%
  group_by(income_group) %>%
  summarise(
    mean_CFR = mean(CFR),
    sd_CFR = sd(CFR),
  )

#Plot data
ggplot(data = summary_data, aes(x = income_group, y = mean_CFR)) +
  geom_point(size = 3, color = "red") +
  geom_errorbar(aes(ymin = mean_CFR - sd_CFR, ymax = mean_CFR + sd_CFR), width = 0.2) +
  geom_line(mapping = aes(group = 1), linetype = "dashed")+
  theme_minimal() +
  labs(title = "Mean Case Fatality Rate by World Bank Gross National Income Categories",
       x = "GNI Income Category",
       y = "Mean CFR Value")


#create a model for anova
incomeModel <- lm(CFR~income_group, data = covid_thresholds)

#Test normality, underlying data is normal
shapiro.test(residuals(incomeModel))

# No statistically significant difference in mean CFR
summary(aov(CFR~income_group, data = covid_thresholds))


#Creating bar plot for CFR per country 
ggplot(covid_thresholds, aes(x= Country, y= CFR, fill = CFR)) +
  geom_col() +
  scale_fill_gradient(low = "skyblue", high = "purple4", name = NULL) +
  labs(title = "Case Fatality Rate by Country", 
       x = "Country", 
       y= "CFR") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "left",
        legend.title = element_text(hjust = 0.5, angle = 90),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "gainsboro"))

#Creating bar plot for MR per country 
ggplot(covid_thresholds, aes(x= Country, y= (MR*100), fill = (MR*100))) +
  geom_col() +
  scale_fill_gradient(low = "#96f3c9", high = "#205264", name = NULL) +
  labs(title = "Mortality Rate by Country", 
       x = "Country", 
       y= "Mortality Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "left",
        legend.title = element_text(hjust = 0.5, angle = 90),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "gainsboro"))

library(rnaturalearth)
library(rnaturalearthdata)
#create a dataset with countries, and join the income levels
world <- ne_countries(scale = "medium", returnclass = "sf")

#Rename the France iso_code
world$iso_a2[world$iso_a3_eh == "FRA"] <- "FR"
world$iso_a2[world$iso_a3_eh == "NOR"] <- "NO"


#Join the covid data data with the mapping data
world_withdata <- (left_join(world, covid_thresholds, by = join_by("iso_a2" == "Country_code")))

#create another version with data for all countries
world_withdata_all <- (left_join(world, covid, by = join_by("iso_a2" == "Country_code")))