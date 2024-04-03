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
