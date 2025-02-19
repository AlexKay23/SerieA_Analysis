library(worldfootballR)
library(tidyverse)


uStatMatchResults <- understat_league_match_results("Serie A", 2024)




uStatAtalanta <- uStatMatchResults %>% 
  filter(home_abbr == "ATA"|away_abbr== "ATA") %>% 
  mutate(ata_xg = case_when(home_abbr == "ATA" ~ home_xG,
                            away_abbr == "ATA" ~ away_xG,
                            TRUE ~ 999)) %>% 
  mutate(ata_goals = case_when(home_abbr == "ATA" ~ home_goals,
                               away_abbr == "ATA" ~ away_goals,
                               TRUE ~ 999)) %>% 
  mutate(ata_win_prob = case_when(home_abbr == "ATA" ~ forecast_win,
                                  away_abbr == "ATA" ~ forecast_loss,
                                  TRUE ~ 999)) %>% 
  mutate(opp_goals = case_when(!home_abbr == "ATA" ~ home_goals,
                               !away_abbr == "ATA" ~ away_goals,
                               TRUE ~ 999)) %>% 
  mutate(win_loss = case_when(ata_goals > opp_goals ~ "Win",
                              opp_goals > ata_goals ~ "Loss",
                              ata_goals == opp_goals ~ "Draw",
                              TRUE ~ "goof")) %>% 
  mutate(opp_xg= case_when(!home_abbr == "ATA" ~ home_xG,
                           !away_abbr == "ATA" ~ away_xG,
                           TRUE ~ 999)) %>% 
  arrange(datetime) %>% 
  mutate(match_number = row_number())



library(grid)
library(png)
library(ggpubr)

ataLogo <- "images/atalanta-seeklogo.png"

img <- png::readPNG(ataLogo)


im2 <- matrix(rgb(img[,,1],img[,,2],img[,,3], img[,,4] * 0.1), nrow=dim(img)[1])

ggplot(uStatAtalanta, aes(x = ata_xg, y = ata_goals, color = ata_win_prob)) +
  geom_point(size = 10) +
  scale_color_gradient(low = "red", high = "green") +
  theme_bw() +
  labs(title = "Atalanta Goals by Win Probability",
       y = "Goals",
       x = "Expected Goals",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com") +
  guides(color = guide_legend(title = "Win Probability"))+
  annotation_custom(rasterGrob(im2,
                               width = unit(.4,"npc"), 
                               height = unit(.8,"npc")), 
                    -Inf, Inf, -Inf, Inf)+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 14),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption = element_text(hjust=0,face = "bold"))



ggplot(uStatAtalanta, aes(x = match_number, color = win_loss, size = ata_win_prob)) +
  geom_point(aes(y = ata_goals)) +
  # geom_point(aes(y = ata_xg), color = "red", fill = "red") +  # Ensure this color mapping is outside aes()
  theme_bw() +
  labs(title = "Atalanta Match Form",
       y = "Goals",
       x = "Match Week",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com",
       color = "Match Result",
       size = "Predicted Win") +
  guides(color = guide_legend(override.aes = list(size = 6))) +  # Only adjust the size of points in the color legend
  scale_color_manual(values = c(Draw = "gold",
                                Loss = "black",
                                Win = "#1e71b8")) +
  scale_size_continuous(range = c(2, 12))+
  annotation_custom(rasterGrob(im2,
                               width = unit(.4, "npc"), 
                               height = unit(.8, "npc")), 
                    -Inf, Inf, -Inf, Inf) +
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "bold"))


# Create a dummy variable for the expected goals legend
uStatAtalanta$ExpectedGoals <- "Expected Goals"

ggplot(uStatAtalanta, aes(x = match_number)) +
  geom_segment(aes(x = match_number, xend = match_number, y = ata_goals, yend = ata_xg), color = "gray") +  # Add connecting bars
  geom_point(aes(y = ata_goals, color = win_loss), size = 9) +
  geom_point(aes(y = ata_xg, shape = ExpectedGoals), size = 5, color = "red", fill = "red") +  # Add a dummy shape for the legend
  theme_bw() +
  labs(title = "Atalanta Match Form",
       y = "Goals",
       x = "Match Week",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com",
       color = "Match Result",
       shape = "") +  # Add a shape legend title
  guides(color = guide_legend(override.aes = list(size = 5)),  # Adjust the size of points in the color legend
         shape = guide_legend(override.aes = list(size = 5, color = "red"))) +  # Adjust the size and color of points in the shape legend
  scale_color_manual(values = c(Draw = "gold",
                                Loss = "black",
                                Win = "#1e71b8")) +
  scale_shape_manual(values = c("Expected Goals" = 16)) +  # Assign shape to the dummy variable
  scale_size_continuous(range = c(2, 10)) +  # Adjust the range of sizes
  annotation_custom(rasterGrob(im2,
                               width = unit(.4, "npc"), 
                               height = unit(.8, "npc")), 
                    -Inf, Inf, -Inf, Inf) +
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "bold"))




ggplot(uStatAtalanta, aes(x=match_number))+
  geom_col(aes(y=opp_goals),fill= "red",alpha=0.6)+
  geom_col(aes(y=ata_goals),fill="#1e71b8",alpha=0.8)+
  geom_point(aes(y=ata_xg),color="darkblue",size=7)+
  geom_point(aes(y=opp_xg),color="darkred",size=7)







library(scales)


temp <- uStatAtalanta %>% 
  mutate(rating = case_when(win_loss == "Win" ~ (uStatAtalanta$ata_goals* (1-uStatAtalanta$ata_win_prob)+(uStatAtalanta$ata_goals-uStatAtalanta$ata_xg)),
                            win_loss == "Draw" ~ (uStatAtalanta$ata_goals* (.5-uStatAtalanta$ata_win_prob)+(uStatAtalanta$ata_goals-uStatAtalanta$ata_xg)),
                            win_loss == "Loss" ~ (uStatAtalanta$ata_goals*uStatAtalanta$ata_win_prob)+(uStatAtalanta$ata_goals-uStatAtalanta$ata_xg),
                            TRUE ~ 9999)) %>% 
  mutate(rating = rescale(rating)*10) %>% 
  mutate(opp = case_when(!home_abbr == "ATA" ~ home_abbr,
                         !away_abbr == "ATA" ~ away_abbr,
                         TRUE ~ "Fail"))

### plots testing the the rating


win_prob_model_test <-ggplot(data = temp, aes(y=ata_win_prob,x=rating))+
  geom_point(size=7)+
  labs(title = "Win Probability and Rating",
       x= "Rating",
       y= "Pre Match Win Probability")+
  theme_bw()

goal_impact_model <- ggplot(data = temp, aes(y=ata_goals,x=rating))+
  geom_point(size=7)+
  labs(title = "Goal Impact on Rating",
       x= "Rating",
       y= "Goals Scored")+
  theme_bw()

Opp_goal_impact_model <-ggplot(data = temp, aes(y=opp_goals,x=rating))+
  geom_point()+
  geom_point(size=7)+
  labs(title = "Opponet Goal Impact on Rating",
       x= "Rating",
       y= "Opponent Goals Scored")+
  theme_bw()

result_impact <- ggplot(data = temp, aes(y=win_loss, x=rating, color=ata_win_prob))+
  geom_point(size=10)+
  geom_boxplot()+
  labs(title = "Match Result Impact on Rating",
       x= "Rating",
       y= "Opponent Goals Scored")+
  theme_bw()


library(cowplot)

plot_grid(win_prob_model_test,result_impact,
          goal_impact_model,Opp_goal_impact_model,
          ncol = 2)


ggplot(data = temp, aes(y=rating,x=match_number))+
  geom_point(size=8,color="#1e71b8")+
  theme_bw()+
  labs(title = "Atalanta Match Ratings",
       y = "Match Rating",
       x = "Match Week",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com")+
  annotation_custom(rasterGrob(im2,
                               width = unit(.3, "npc"), 
                               height = unit(.8, "npc")), 
                    -Inf, Inf, -Inf, Inf) +
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "bold"))


ggplot(data = temp, aes(x=match_number))+
  geom_col(aes(y=ata_goals), fill="#1e71b8",alpha=0.8)+
  geom_col(aes(y=opp_goals),fill="gray")+
  geom_point(aes(y=rating),fill="black",shape=16,size=10)+
  theme_bw()


mean(temp$rating)










