
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
filter(Batting, yearID == 2013)x

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


## Week 2: Tuesday

### left_join 

#### player names


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
### add player names
career_HR 

### career home runs and career era-adjusted home runs (left_join)
career_eHR

datHR = career_HR %>% 
  left_join(career_eHR, by = "playerID")
datHR


## ggplot2 

### add aesthetic
### add geometry
### add a theme
### add a title and axes labels


### career home runs vs era-adjusted home runs by final year (min 1000 AB)



#### get common playerIDs in these data sets
?intersect
playerIDs = intersect(career_HR$playerID, 
                      career_eHR$playerID)

### continue with comparison of career home runs vs era-adjusted home runs



#### make larger data set with original and era-adjusted 
#### HR counts
?bind_rows
datHR_binded = 
#mutate(id = ifelse(id == 1, "original", "era-adjusted"))


#### pivot
  
  
#### separate trend lines (col in aes)


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
ggplot(datHR) +
  aes(x = finalYear, y = HR, col = id) 



### single season HR by position 1982-1993 (minimum 100 AB)


#### Notes: 
## https://daviddalpiaz.org/posts/moneyball-in-r/

