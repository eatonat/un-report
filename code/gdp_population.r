## Setup section

library(tidyverse)
library(ggplot2)


## Day 1: plotting

gapminder_1997 <- read_csv("data/gapminder_1997.csv")
gapminder_1997

# Using ggplot to make a plot
ggplot(data = gapminder_1997) + 
  aes(x = gdpPercap, 
      y = lifeExp, 
      color = continent, 
      size = pop/1000000) + 
  labs(x = "GDP per Capita", 
       y = "Life Expectancy", 
       title = "Do People in Wealthy Countries Live Longer?", 
       size = "Population (in millions)", 
       color = "Continent") +
  geom_point() + 
  scale_color_brewer(palette = "RdYlBu") + 
  theme_minimal()

# Load in full gapminder dataset
gapminder_data <- read_csv("data/gapminder_data.csv")

ggplot(data = gapminder_data) +
  aes(x = year, 
      y = lifeExp, 
      color = continent, 
      group = country) + 
  geom_line(alpha = 0.5) +
  theme_classic()

ggplot(data = gapminder_data) +
  aes(x = year, 
      y = pop, 
      color = continent, 
      group = country) + 
  geom_line()

ggplot(data = gapminder_data) + 
  aes(x = continent,
      y = lifeExp) + 
  geom_boxplot()

ggplot(data = gapminder_data) + 
  aes(x = continent,
      y = lifeExp) + 
  geom_violin() +
# geom_jitter = random spacing on the x-axis to better identify trends
  geom_jitter(width = 0.1, alpha = 0.2)
ggsave("figures/continent_lifeExp.png")


## Day 2: Data manipulation and cleaning

gapminder_data <- read_csv("data/gapminder_data.csv")
gapminder_data

# Summarize, filter, and group_by

summarize(gapminder_data, avgLifeExp = mean(lifeExp))

summarize(gapminder_data, minLifeExp = min(lifeExp), maxLifeExp = max(lifeExp))

gapminder_data_lifeExp <- gapminder_data %>%
  summarize(avgLifeExp = mean(lifeExp), minLifeExp = min(lifeExp), maxLifeExp = max(lifeExp))

gapminder_data %>%
  filter(year == 2007) %>%
  summarise(avgLifeExp = mean(lifeExp))

gapminder_data %>%
  filter(year == 1962) %>%
  summarise(avgLifeExp = mean(lifeExp))

summarise(gapminder_data, earliestyr = min(year))

gapminder_data %>%
  filter(year == 1952) %>%
  summarise(avgGDPpercap = mean(gdpPercap))

gapminder_data %>%
  group_by(year) %>%
  summarize(avgLifeExp = mean(lifeExp))

gapminder_data %>%
  group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp))

# Mutate, select, pivot

gapminder_data %>%
  mutate(gdp = gdpPercap * pop / 1000000)

gapminder_data %>%
  mutate(popM = pop / 1000000)

gapminder_data %>%
  select(country, continent, year, lifeExp)

gapminder_data %>%
  select(ends_with("p"))

gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

gapminder_data_2007 <- gapminder_data %>%
  filter(year == 2007) %>%
  select(-year, -continent)


# Cleaning
co2_un_data <- read_csv("data/co2-un-data.csv",
                        skip = 2,
                        col_names = c("region", "country", "year", "series", 
                                      "value", "footnotes", "source"))

co2_un_data_clean <- co2_un_data %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                   "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) 
  

co2_emissions_2005 <- co2_un_data_clean %>%
  filter(year == 2005) %>%
  select(-year)

inner_join(gapminder_data_2007, co2_emissions_2005)

co2_emissions_2005 <- read_csv("data/co2-un-data.csv",
                        skip = 2,
                        col_names = c("region", "country", "year", "series", 
                                      "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, 
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela",
                          "Viet Nam" = "Vietnam"))

gapminder_data <- gapminder_data %>%
  mutate(country = recode(country, 
                          "Puerto Rico" = "United States"))

gapminder_co2 <- inner_join(gapminder_data, co2_emissions_2005, by = "country")

gapminder_co2 %>%
  group_by(continent) %>%
  summarize(avgLifeExp = mean(lifeExp))

gapminder_co2_americas <- gapminder_co2 %>%
  filter(continent == "Americas") %>%
  mutate(region = ifelse(country == "United States" | country == "Canada" | country == "Mexico", 
                         "north", "south")) 

gapminder_co2_2007 <- gapminder_co2 %>%
  filter(continent == "Americas" & year == 2007)

# Messing around

library(dplyr)
country <- gapminder_data %>%
  select(continent, country) %>%
  distinct(country, .keep_all = TRUE) 

country[order(country$continent),] %>% print(n = 142) 
