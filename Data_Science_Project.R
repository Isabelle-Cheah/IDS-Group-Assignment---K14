library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
continents <- read.csv("continents-according-to-our-world-in-data.csv")
gdp <- read.csv("gdp-per-capita-worldbank.csv")
NEET<- read.csv("youth-not-in-education-employment-training.csv")

# shortening the column names for clarity 
gdp <- gdp %>%
  rename(GDP = GDP.per.capita..PPP..constant.2017.international...)
NEET <- NEET %>%
  rename(share_NEET = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population.)
colnames(NEET)

#joining datasets for question 1 and removing any antarctica data 
# distint removes duplicate rows
# use select to ignore certain columns such as year: all continents years are 2015 so can ignore this 
continents_cleaned <- continents %>%
  select(Entity, Continent) %>%  
  distinct() 
continents_gdp <- gdp %>%
  left_join(continents_cleaned, by = "Entity") %>%
  filter(Continent != "Antarctica")

#joining datasets for question 2
continents_NEET <- NEET %>%
  left_join(continents_cleaned, by = "Entity") %>%
  filter(!is.na(share_NEET), Continent != "Antarctica")

#Cleaning additional csv data
Unemployment_Rate <- read.csv("unemployment_rate.csv")
names(Unemployment_Rate) <- gsub("^X", "", names(Unemployment_Rate))
Unemployment_Rate_Long <- Unemployment_Rate %>%
  gather(Year, Unemployment_Rate, "1991":"2024") %>%
  rename(Entity = Country.Name, Code = Country.Code) %>%
  mutate(Year = as.integer(Year), Unemployment_Rate = as.numeric(Unemployment_Rate))

#combining unemployment rate data with continents data
UnemploymentRate_Continents <- Unemployment_Rate_Long %>%
  left_join(continents_cleaned, by = "Entity") %>%
  filter(Continent != "Antarctica")

#formula to calc growth rates for question 1 
gdp_growth_rate <- continents_gdp %>%
  arrange(Entity, Year) %>%
  group_by(Entity) %>%
  mutate(growth_rate = (GDP - lag(GDP)) / lag(GDP) * 100)

#to make data cleaner, I need to find the average growth rates per year for each continent, otherwise each individual country's growth rate will be plotted - messy graph
avg_continent_growth <- gdp_growth_rate %>%
  group_by(Continent, Year) %>%
  summarise(avg_growth = mean(growth_rate, na.rm = TRUE), .groups = "drop")

#line graph of gdp growth per continent 
g2 <- avg_continent_growth %>% 
  ggplot(aes(x = Year, y = avg_growth, colour = Continent, group = Continent)) +
  geom_line(size = 1) +
  facet_wrap(~Continent) +
  scale_colour_manual(values = c(
    "Africa" = "#60ad31",
    "Asia" = "#b83336",
    "Europe" = "#336db8",
    "North America" = "#de7d33",
    "South America" = "#f7dc43",
    "Oceania" = "#8c2fc2"
  )) +
  xlab("Year") +
  ylab("Average Growth Rate (%)") +
  ggtitle("Average GDP Per Capita Growth Rates by Continent") +
  theme_minimal() +
  theme(legend.position = "none")
print(g2)

#Average unemployment rate of each continent over time bar chart 
#need to first find the average unemployment rate for each continent
Avg_unemployment_continent <- UnemploymentRate_Continents %>%
  group_by(Continent) %>%
  summarise(avg_unemployment = mean(Unemployment_Rate, na.rm = TRUE))
g6 <- ggplot(Avg_unemployment_continent, aes(x = Continent, y = avg_unemployment, fill = Continent)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Average Unemployment Rate by Continent",
    x = "Continent",
    y = "Average Unemployment Rate (%)") +
  scale_fill_manual(values = c(
    "Africa" = "#60ad31",
    "Asia" = "#b83336",
    "Europe" = "#336db8",
    "North America" = "#de7d33",
    "South America" = "#f7dc43",
    "Oceania" = "#8c2fc2"
  )) +
  theme(legend.position = "none")
print(g6)

