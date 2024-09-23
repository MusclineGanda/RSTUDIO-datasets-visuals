### Preparations:
# Installing and loading required packages
install.packages(c("tidyverse", "gapminder", "ggridges"))
library(tidyverse)
library(gapminder)
library(ggridges)
library(ggplot2)


gapWider <- gapminder %>% 
  select(country,continent,year,gdpPercap) %>% 
  pivot_wider(
    names_from = year,
    values_from = gdpPercap)

gapLonger<- gapWider %>% 
  pivot_longer(3:ncol(gapWider),
               names_to = "Record_Year",
               values_to = "GDP"
  )

gapSummary<- gapminder %>% 
  drop_na(continent) %>% 
  group_by(continent) %>% 
  summarise(
    Lowest = min(gdpPercap),
    Highest = max(gdpPercap),
    Average = mean(gdpPercap),
    Total = sum(gdpPercap),
    Difference = max(gdpPercap) - min(gdpPercap),
    Variance = 100*(max(gdpPercap) - min(gdpPercap))/min(gdpPercap)
  ) %>% 
  arrange(Variance) %>% 
  view()


table(gapminder$year,gapminder$continent)

### gapminder dataset:

### 1. *Basic Histogram*:
### Distribution of life expectancy over the years.
ggplot(gapminder, aes(x=lifeExp)) +
  geom_histogram(binwidth=5, fill="steelblue") +
  labs(title="Histogram of Life Expectancy", x="Life Expectancy", y="Count") +
  theme_minimal()


### 2. *Bar Plot*:
###Average life expectancy per continent.
gapminder %>%
  group_by(continent) %>%
  summarise(avg_lifeExp=mean(lifeExp)) %>%
  ggplot(aes(x=continent, y=avg_lifeExp)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Average Life Expectancy by Continent") +
  theme_minimal()


### 3. *Scatter Plot*:
###GDP per capita vs. life expectancy, colored by continent.
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp, color=continent)) +
  geom_point(alpha=0.6) +
  scale_x_log10() +
  labs(title="Life Expectancy vs. GDP per Capita") +
  theme_minimal()

gapminder %>% 
  filter(continent %in% c('Africa','Europe')) %>% 
  filter(gdpPercap < 30000) %>% 
  ggplot(aes(x=gdpPercap,
             y=lifeExp,
             size=pop,
             color=year)
  )+
  geom_point(alpha=0.6)+
  facet_wrap(~continent)+
  labs(title="Life expectancy explained by GDPerCap",
       x="GDP per capita",
       y="Life expectancy")+
  theme_bw()


### 4. *Box Plot*:
###Life expectancy distribution by continent.
ggplot(gapminder, aes(x=continent, y=lifeExp)) +
  geom_boxplot(aes(fill=continent)) +
  labs(title="Life Expectancy Distribution by Continent") +
  theme_minimal()


### 5. *Density Plot*:
###Distribution of life expectancy by continent.
ggplot(gapminder, aes(x=lifeExp, fill=continent)) +
  geom_density(alpha=0.6) +
  labs(title="Density Plot of Life Expectancy by Continent") +
  theme_minimal()


### 6. *Violin Plot*:
###Distribution of life expectancy by continent.
ggplot(gapminder, aes(x=continent, y=lifeExp)) +
  geom_violin(aes(fill=continent)) +
  labs(title="Violin Plot of Life Expectancy by Continent") +
  theme_minimal()


### 7. *Facet Grid*:
###Life expectancy over years, faceted by continent.
ggplot(gapminder, aes(x=year, y=lifeExp)) +
  geom_line(aes(group=country)) +
  facet_wrap(~continent) +
  labs(title="Life Expectancy Over Years by Continent") +
  theme_minimal()

### 8. *Tile Plot*:
###Unfortunately, the gapminder dataset isn't suitable for tile 
###plots due to its structure. Tile plots are best for datasets 
###with two categorical variables and a continuous value derived from them.

### 9. *Ridge Plot*:
###Life expectancy distribution over the years.
ggplot(gapminder, aes(x=lifeExp, y=as.factor(year), fill=as.factor(year))) +
  geom_density_ridges() +
  labs(title="Ridge Plot of Life Expectancy Over Years") +
  theme_ridges()


### 10. *Bar Plot with Error Bars*:
###Average life expectancy per continent with standard error.
gapminder %>%
  group_by(continent) %>%
  summarise(mean_lifeExp = mean(lifeExp, na.rm=TRUE),
            se = sd(lifeExp, na.rm=TRUE) / sqrt(n())) %>%
  ggplot(aes(x=continent, y=mean_lifeExp)) +
  geom_bar(stat="identity", aes(fill=continent)) +
  geom_errorbar(aes(ymin=mean_lifeExp-se, ymax=mean_lifeExp+se), width=0.2) +
  labs(title="Average Life Expectancy by Continent with Error Bars") +
  theme_minimal()


### 11. *Stacked Area Chart*:
###Total population over years by continent.
gapminder %>%
  group_by(year, continent) %>%
  summarise(total_pop = sum(pop)) %>%
  ggplot(aes(x=year, y=total_pop, fill=continent)) +
  geom_area(alpha=0.6, position='stack') +
  labs(title="Total Population Over Years by Continent") +
  theme_minimal()