
# Week 1: Thursday

## go through Batting data set in Lahman
## focus on home runs (HR)

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

## Batting data
?Lahman
?Batting

## tibble
Batting = as_tibble(Batting)
People = as_tibble(People)

## dplyr 

### select
select(Batting, HR)

### filter 
filter(Batting, yearID == 2013)

### arrange 
arrange(Batting, HR)
arrange(Batting, desc(HR))

### mutate 
mutate(Batting, is_30 = HR >= 30)
View(mutate(Batting, is_30 = HR >= 30))

### pipe operator

#### did a player hit at least 30 HR in their MLB stint?
Batting %>% 
  select(playerID, yearID, stint, HR) %>% 
  mutate(is_30 = HR >= 30) %>% 
  arrange(desc(HR))


### group_by/summarize 
summarise(Batting, meanHR = mean(HR), mean(HR))


#### average home runs per player/year combination
Batting %>% 
  group_by(playerID) %>% 
  summarise(meanHR = mean(HR)) %>% 
  arrange(desc(meanHR))

Batting %>% 
  group_by(playerID, yearID) %>% 
  summarise(HR = sum(HR)) %>% 
  group_by(playerID) %>% 
  summarise(meanHR = mean(HR)) %>% 
  arrange(desc(meanHR))

### left_join 

#### player names
foo = Batting %>% 
  group_by(playerID, yearID) %>% 
  summarise(HR = sum(HR)) %>% 
  group_by(playerID) %>% 
  summarise(meanHR = mean(HR)) %>% 
  arrange(desc(meanHR))

bar = People %>% 
  mutate(name = paste(nameFirst, nameLast, sep = " ")) %>% 
  select(playerID, name)

dat_avg_HR = foo %>% 
  left_join(bar, by = "playerID")

## readr

### data from: 
### Yan, Burgos Jr, Kinson, and Eck (2025) Comparing baseball players across eras 
### via novel Full House Modeling. Annals of Applied Statistics, forthcoming
season_eHR = read_csv(file = "stat528sp25/notes/1-coding/season_eHR.csv")
season_eHR

## write_csv
### create the above
#bat_season = read_csv(file = "~/website/era-adjustment-website/src/data/batters_adjusted.csv")
#foo = bat_season %>% 
#  select(playerID, year, HR, AB)
#foo
#write_csv(foo, file = "stat528sp25/notes/1-coding/season_eHR.csv")


## Examples 

### career home runs (arranged from high to low)
### add final year and career at bats
career_HR = Batting %>% 
  group_by(playerID) %>% 
  summarise(HR = sum(HR), AB = sum(AB), finalYear = max(yearID)) %>% 
  arrange(desc(HR))
  

### add player names
career_HR = Batting %>% 
  group_by(playerID) %>% 
  summarise(HR = sum(HR), AB = sum(AB), finalYear = max(yearID)) %>% 
  arrange(desc(HR)) 


### career home runs and career era-adjusted home runs (left_join)
career_eHR  = season_eHR %>% 
  group_by(playerID) %>% 
  summarise(eHR = sum(HR), eAB = sum(AB), eFinalYear = max(year)) %>% 
  arrange(desc(eHR)) 

datHR = career_HR %>% 
  left_join(career_eHR, by = "playerID")
datHR


## ggplot2 

### add aesthetic
### add geometry
### add a theme
### add a title and axes labels
ggplot(career_HR) + 
  aes(x = finalYear, y = HR) + 
  geom_point() + 
  geom_smooth() +
  theme_minimal() + 
  labs(title = "career home runs over time", 
       x = "final year",
       y = " home runs")

ggplot(career_eHR) + 
  aes(x = eFinalYear, y = eHR) + 
  geom_point() + 
  geom_smooth() +
  theme_minimal() + 
  labs(title = "Career era-adjusted home runs over time", 
       x = "final year",
       y = " home runs")

### career home runs vs era-adjusted home runs by final year (min 1000 AB)
career_HR %>% 
  filter(AB >= 1000) %>% 
  ggplot() + 
  aes(x = finalYear, y = HR) + 
  geom_point() + 
  geom_smooth() +
  theme_minimal() + 
  labs(title = "Career home runs over time", 
       subtitle = "minimum 1000 at bats",
       x = "final year",
       y = " home runs")

career_eHR %>% 
  filter(eAB >= 1000) %>% 
  ggplot() + 
  aes(x = eFinalYear, y = eHR) + 
  geom_point() + 
  geom_smooth() +
  theme_minimal() + 
  labs(title = "Career era-adjusted home runs over time", 
       subtitle = "minimum 1000 at bats",
       x = "final year",
       y = " home runs")


career_HR = career_HR %>% 
  filter(AB >= 1000) 

career_eHR = career_eHR %>% 
  filter(eAB >= 1000)

#### get common playerIDs in these data sets
?intersect
playerIDs = intersect(career_HR$playerID, 
                      career_eHR$playerID)

### continue with comparison of career home runs vs era-adjusted home runs

#### make larger data set with original and era-adjusted 
#### HR counts
?bind_rows
datHR_binded = bind_rows(career_HR %>% 
                           filter(playerID %in% playerIDs), 
                         career_eHR %>% 
                           filter(playerID %in% playerIDs) %>% 
                           rename(HR = eHR, AB = eAB, finalYear = eFinalYear), 
                         .id = "id") %>% 
  mutate(id = ifelse(id == 1, "observed", "era-adjusted"))

  
#### separate trend lines (col in aes)
ggplot(datHR_binded) + 
  aes(x = finalYear, y = HR, col = id) +
  geom_smooth() + 
  theme_minimal() + 
  labs(title = "Smoothed career home runs over time", 
       subtitle = "minimum 1000 at bats",
       x = "final year",
       y = " home runs")

#### better plot
ggplot(datHR_binded) + 
  aes(x = finalYear, y = HR, col = id) + 
  geom_smooth() + 
  annotate("text", x = c(2005.25), 
           y = c(88), 
           label = c("Steroid era"), 
           color = c("black"), 
           size = 2.25) + 
  annotate("rect", fill = "orange", alpha = 0.2, 
           xmin = 1995, xmax = 2015,
           ymin = -Inf, ymax = Inf) +
  labs(title = "Smoothed home run count by a player's final year (min 1000 ABs)", 
       subtitle = "Comparison of raw and era-adjusted counts") + 
  theme_minimal()

#### facet_wrap
ggplot(datHR_binded) +
  aes(x = finalYear, y = HR, col = id) + 
  geom_point(col = "black") + 
  geom_smooth() + 
  facet_wrap(~id) +
  labs(title = "Smoothed home run count by a player's final year (min 1000 ABs)", 
       subtitle = "Comparison of raw and era-adjusted counts") + 
  theme_minimal()



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
  select(playerID, yearID, HR) %>% 
  left_join(Fielding_small) %>% 
  filter(!is.na(primaryPOS)) %>% 
  ggplot() + 
  aes(x = HR, y = primaryPOS, fill = primaryPOS) + 
  geom_density_ridges() + 
  theme_minimal() + 
  labs(title = "Distribution of seasonal home run counts", 
       subtitle = "by primary position", 
       x = "home runs", 
       y = "primary position")

#### Notes: 
## https://daviddalpiaz.org/posts/moneyball-in-r/

