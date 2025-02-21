library(worldfootballR)
library(tidyverse)
library(scales)

uStatMatchResults <- understat_league_match_results("Serie A", 2024)


## For atalanta we found the match rating, now we want match rating for all teams for each game.


all_serie_a <- uStatMatchResults %>% 
  mutate(home_team_mr = case_when(home_goals > away_goals ~ (uStatMatchResults$home_goals * (1-uStatMatchResults$forecast_win)+(uStatMatchResults$home_goals-uStatMatchResults$home_xG)),
                                  home_goals == away_goals ~ (uStatMatchResults$home_goals * uStatMatchResults$forecast_win)+(uStatMatchResults$home_goals-uStatMatchResults$home_xG),
                                  home_goals < away_goals~ (uStatMatchResults$home_goals * (.5-uStatMatchResults$forecast_win)+(uStatMatchResults$home_goals-uStatMatchResults$home_xG)),
                                  TRUE ~ 9999)) %>% 
  mutate(home_team_mr = rescale(home_team_mr)*10) %>% 
  mutate(away_team_mr = case_when(away_goals > home_goals ~ (uStatMatchResults$away_goals * (1-uStatMatchResults$forecast_win)+(uStatMatchResults$away_goals-uStatMatchResults$away_xG)),
                                  away_goals == home_goals ~ (uStatMatchResults$away_goals * uStatMatchResults$forecast_win)+(uStatMatchResults$away_goals-uStatMatchResults$away_xG),
                                  away_goals < home_goals~ (uStatMatchResults$away_goals * (.5-uStatMatchResults$forecast_win)+(uStatMatchResults$away_goals-uStatMatchResults$away_xG)),
                                  TRUE ~ 9999)) %>% 
  mutate(away_team_mr = rescale(away_team_mr)*10)


home_team_ratings <- all_serie_a %>% 
  group_by(home_team) %>% 
  summarise(mr_avg_1 = mean(home_team_mr)) %>% 
  rename(team = home_team)

away_teams_rating <- all_serie_a %>% 
  group_by(away_team) %>% 
  summarise(mr_avg_2 = mean(away_team_mr)) %>% 
  rename(team = away_team)
  

mr_table <- inner_join(home_team_ratings,away_teams_rating)

mr_table <- mr_table %>% mutate(total_mr = (mr_avg_1+mr_avg_2)/2) %>% arrange(desc(total_mr))


ggplot(data = all_serie_a, aes(y=home_goals,x=home_team_mr))+
  geom_point(size=7)+
  labs(title = "Win Probability and Rating",
       x= "Rating",
       y= "Pre Match Win Probability")+
  theme_bw()

ggplot(data = mr_table %>% 
         pivot_longer(
           cols = starts_with("mr"),
           names_to = "group",
           values_to = "mr"
         ), aes(x=group,y=mr) )+
  geom_boxplot()
