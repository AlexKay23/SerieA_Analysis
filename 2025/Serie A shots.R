## Serie A Shots


library(worldfootballR)
library(tidyverse)



understat_team_meta(team_name = "Atalanta")

ata_shots <- understat_team_season_shots("https://understat.com/team/Atalanta/2024")

ata_shots2 <- ata_shots %>% 
  mutate(ata_play = case_when(home_away == "h"|home_team == "Atalanta" ~ "nAtalanta",
                              home_away == "a"|home_team == "Atalanta" ~ "Atalanta",
                              TRUE ~"Atalanta")) %>% 
  mutate(opp = case_when(!away_team == "Atalanta"~away_team,
                         !home_team == "Atalanta"~ home_team))

aggregate(result ~ ata_play + minute, data = ata_shots2, function(x) length(x))
  




#serieaShots <- understat_league_season_shots(league = "Serie A",season_start_year = 2024)

ggplot(ata_shots2,aes(x=X,y=Y,color =  ata_play))+geom_point()+
  geom_segment(aes(x = 1, xend = 1, y = .25, yend = .75), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .95, y = .4, yend = .4), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .95, y = .6, yend = .6), color = "white", size = 1)+
  geom_segment(aes(x = .95, xend = .95, y = .4, yend = .6), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .85, y = .75, yend = .75), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .85, y = .25, yend = .25), color = "white", size = 1)+
  geom_segment(aes(x = .85, xend = .85, y = .75, yend = .25), color = "white", size = 1)+
  theme(panel.background = element_rect(fill = '#009A00', color = '#009A17'),
        panel.grid.major = element_line(color = '#00A619', linetype = "solid",linewidth = 18),
        panel.grid.minor = element_line(color = '#009A00', size = 18),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
  


ggplot(ata_shots2 %>% 
         filter(ata_play == "Atalanta") %>% 
         filter(result == "Goal"),
       aes(x=X,y=Y))+
  geom_point()+
  geom_segment(aes(x = 1, xend = 1, y = .25, yend = .75), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .95, y = .4, yend = .4), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .95, y = .6, yend = .6), color = "white", size = 1)+
  geom_segment(aes(x = .95, xend = .95, y = .4, yend = .6), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .85, y = .75, yend = .75), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .85, y = .25, yend = .25), color = "white", size = 1)+
  geom_segment(aes(x = .85, xend = .85, y = .75, yend = .25), color = "white", size = 1)+
  theme(panel.background = element_rect(fill = '#009A00', color = '#009A17'),
        panel.grid.major = element_line(color = '#00A619', linetype = "solid",linewidth = 18),
        panel.grid.minor = element_line(color = '#009A00', size = 18),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  facet_wrap(~situation)






ggplot(ata_shots2 %>% 
         filter(result == "Goal"),
       aes(x=X,y=Y,color=opp))+
  geom_point()+
  geom_segment(aes(x = 1, xend = 1, y = .25, yend = .75), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .95, y = .4, yend = .4), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .95, y = .6, yend = .6), color = "white", size = 1)+
  geom_segment(aes(x = .95, xend = .95, y = .4, yend = .6), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .85, y = .75, yend = .75), color = "white", size = 1)+
  geom_segment(aes(x = 1, xend = .85, y = .25, yend = .25), color = "white", size = 1)+
  geom_segment(aes(x = .85, xend = .85, y = .75, yend = .25), color = "white", size = 1)+
  theme(panel.background = element_rect(fill = '#009A00', color = '#009A17'),
        panel.grid.major = element_line(color = '#00A619', linetype = "solid",linewidth = 18),
        panel.grid.minor = element_line(color = '#009A00', size = 18),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  facet_wrap(~situation)




ggplot(ata_shots2 %>% 
          filter(result == "Goal"))+
  geom_bar(aes(x=X))+
  geom_bar(aes(y=Y))+
  xlim(0,1)+
  ylim(0,1)


