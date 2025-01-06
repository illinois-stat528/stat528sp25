
# Week 2: Tuesday

## talk about .Rproj and computer organization

## tidyverse 
#install.packages("tidyverse")
library(tidyverse)
#install.packages("Lahman")
library(Lahman)
#install.packages("lubridate")
library(lubridate)
#install.packages("palmerpenguins")
library(palmerpenguins)
#install.packages("ggridges") 
library(ggridges)

## tibble
as_tibble(iris)
as_tibble(penguins)
as_tibble(Batting)
as_tibble(People)

## dplyr 

### select
select(iris, Petal.Length)

### filter 
as_tibble(filter(iris, Petal.Length >= 1.7))

### arrange 
arrange(iris, desc(Petal.Length))

### mutate 
mutate(iris, Size = ifelse(Sepal.Width >= 3.4, "Big", "Small"))

### group_by/summarize 
summarise(iris, "mean.Sepal.Width" = mean(Sepal.Width), mean(Sepal.Length))
iris %>% 
  group_by(Species) %>% 
  summarise("mean.Sepal.Width" = mean(Sepal.Width), mean(Sepal.Length))

### left_join (see below)

### pipe operator (see below)


## readr
era_adjusted_HR = read_csv(file = "stat528sp24/notes/1-coding/era_adjusted_HR.csv")
era_adjusted_HR


## examples 

### career home runs (arranged from high to low; add final game)

career_HR = Batting %>% 
  group_by(playerID) %>% 
  summarise(HR = sum(HR), AB = sum(AB)) %>% 
  arrange(desc(HR))

final_year = People %>% 
  mutate(final_year = year(finalGame)) %>% 
  select(playerID, final_year)

career_HR = left_join(career_HR, final_year, by = "playerID")


## ggplot2 

### career home runs by final game 
ggplot(career_HR) + 
  aes(x = final_year, y = HR) + 
  geom_smooth() + 
  theme_classic()

ggplot(era_adjusted_HR) + 
  aes(x = year, y = HR) + 
  geom_smooth() + 
  theme_classic()

### career home runs vs era-adjusted home runs by final game
career_HR = Batting %>% 
  group_by(playerID) %>% 
  summarise(HR = sum(HR), final_year = max(yearID), AB = sum(AB)) %>% 
  filter(HR > 10, AB > 1000)

career_era_adjusted_HR = era_adjusted_HR %>% 
  group_by(playerID) %>% 
  summarise(HR = sum(HR), final_year = max(year), AB = sum(AB)) %>% 
  filter(HR > 10, AB > 1000)

?intersect
playerIDs = intersect(career_HR$playerID, 
                      career_era_adjusted_HR$playerID)

ggplot(career_HR %>% filter(playerID %in% playerIDs)) + 
  aes(x = final_year, y = HR) + 
  geom_point() + 
  geom_smooth()

ggplot(career_era_adjusted_HR %>% filter(playerID %in% playerIDs)) + 
  aes(x = final_year, y = HR) + 
  geom_point() + 
  geom_smooth()

?bind_rows
dat = bind_rows(career_HR %>% filter(playerID %in% playerIDs), 
                career_era_adjusted_HR %>% filter(playerID %in% playerIDs), 
                .id = "id") %>% 
  mutate(id = ifelse(id == 1, "original", "era-adjusted"))

ggplot(dat) +
  aes(x = final_year, y = HR, col = id) + 
  #geom_point(col = "black") + 
  geom_smooth()

ggplot(dat) +
  aes(x = final_year, y = HR, col = id) + 
  facet_wrap(facets = ~ id) + 
  geom_point(col = "black") + 
  geom_smooth()

### single season HR by position 1982-1993 (minimum 100 AB)
Fielding_small = Fielding %>% 
  select(playerID, yearID, POS, InnOuts) %>% 
  group_by(playerID, yearID) %>% 
  filter(InnOuts == max(InnOuts)) %>% 
  filter(POS != "P", !is.na(POS)) %>% 
  rename("primaryPOS" = POS) %>% 
  select(playerID, yearID, primaryPOS)

Batting %>% 
  filter(yearID >= 1982, yearID <= 1993, AB >= 100) %>% 
  select(playerID, yearID, HR, AB) %>% 
  left_join(Fielding_small) %>% 
  ggplot() + 
  aes(x = HR, y = primaryPOS, fill = primaryPOS) + 
  geom_density_ridges() 


#### Notes: 
## https://daviddalpiaz.org/posts/moneyball-in-r/

