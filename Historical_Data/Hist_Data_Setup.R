library(worldfootballR)
library(tidyverse)


# create some kind of loop to get data from 21-25 season for each data

years <- 2021:2025

all_seasons_LT <- map_dfr(years, ~ fb_season_team_stats(country = "ITA",
                                                          gender = "M",
                                                          season_end_year = .x,
                                                          tier = "1st",
                                                          stat_type = "league_table")) %>% 
  mutate(Notes = str_remove(Notes, "→ ")) %>% 
  mutate(Notes = str_remove(Notes, pattern = "via.*")) %>% 
  mutate(Notes = str_trim(Notes, side = "right")) %>% 
  mutate(Notes = as_factor(Notes)) %>% 
  mutate(Notes = case_when(Notes == "Conference League" ~ "Europa Conference League",
                           Notes == "10-point deduction 1, Europa Conference League" ~ "Europa Conference League",
                           Notes == "Relegation tie-breaker" ~ "",
                           Notes == "Relegated, Relegation tie-breaker" ~ "Relegated",
                           TRUE ~ Notes))


# 	Conference League to 	Europa Conference League
#   Relegation tie-breaker to "" -fix Hellas Verona place to 17 (switch with Spezia)
#   Relegated, Relegation tie-breaker to Relegated

def <- fb_season_team_stats("ITA", "M", 2021, "1st", "defense")
standard <- fb_season_team_stats("ITA", "M", 2021, "1st", "standard")
misc <- fb_season_team_stats("ITA", "M", 2021, "1st", "misc")


ggplot(all_seasons_LT,aes(x=Season_End_Year,y=Rk,group = Squad,color=Squad))+
  geom_point()+
  geom_line()

ggplot(all_seasons_LT, aes(x=Pts,y=reorder(Pts,-Rk),fill = Notes))+
  geom_label(aes(label = Squad),position=position_jitter(width=1,height=1))+
  theme_bw()+
  facet_wrap(~Season_End_Year,scales = "free_y")

ggplot(league_table, aes(x=Pts,y=GF,fill = Notes))+
  geom_label(aes(label = Squad))+
  theme_bw()

ggplot(league_table, aes(x=Pts,y=GD,fill = Notes))+
  geom_label(aes(label = Squad))+
  theme_bw()

ggplot(league_table, aes(x=Pts,y=Rk,fill = Notes))+
  geom_label(aes(label = Squad))+
  theme_bw()

