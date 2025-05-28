library(worldfootballR)
library(tidyverse)


# create some kind of loop to get data from 21-25 season for each data

def <- fb_season_team_stats("ITA", "M", 2021, "1st", "defense")
standard <- fb_season_team_stats("ITA", "M", 2021, "1st", "standard")
misc <- fb_season_team_stats("ITA", "M", 2021, "1st", "misc")
league_table <- fb_season_team_stats("ITA", "M", 2021, "1st", "league_table") %>% 
  mutate(Notes = str_remove(Notes, "→ ")) 

league_table <- league_table %>% 
  mutate(Notes = str_remove(Notes, pattern = "via.*")) %>% 
  mutate(Notes = str_trim(Notes, side = "right")) %>% 
  mutate(Notes = as_factor(Notes))



ggplot(league_table, aes(x=Pts,y=GF,fill = Notes))+
  geom_label(aes(label = Squad))+
  theme_bw()

ggplot(league_table, aes(x=Pts,y=GD,fill = Notes))+
  geom_label(aes(label = Squad))+
  theme_bw()

ggplot(league_table, aes(x=Pts,y=Rk,fill = Notes))+
  geom_label(aes(label = Squad))+
  theme_bw()

