library(worldfootballR)
library(tidyverse)



uStatMatchResults <- understat_league_match_results("Serie A", 2024)




uStatRoma <- uStatMatchResults %>% 
  filter(home_abbr == "ROM"|away_abbr== "ROM") %>% 
  mutate(ROM_xg = case_when(home_abbr == "ROM" ~ home_xG,
                            away_abbr == "ROM" ~ away_xG,
                            TRUE ~ 999)) %>% 
  mutate(ROM_goals = case_when(home_abbr == "ROM" ~ home_goals,
                               away_abbr == "ROM" ~ away_goals,
                               TRUE ~ 999)) %>% 
  mutate(ROM_win_prob = case_when(home_abbr == "ROM" ~ forecast_win,
                                  away_abbr == "ROM" ~ forecast_loss,
                                  TRUE ~ 999)) %>% 
  mutate(opp_goals = case_when(!home_abbr == "ROM" ~ home_goals,
                               !away_abbr == "ROM" ~ away_goals,
                               TRUE ~ 999)) %>% 
  mutate(win_loss = case_when(ROM_goals > opp_goals ~ "Win",
                              opp_goals > ROM_goals ~ "Loss",
                              ROM_goals == opp_goals ~ "Draw",
                              TRUE ~ "goof")) %>% 
  arrange(datetime) %>% 
  mutate(match_number = row_number())



library(grid)
library(png)
library(ggpubr)

ROMLogo <- "images/as-roma-football-club-seeklogo.png"

imgROMA <- png::readPNG(ROMLogo)


im2ROMA <- matrix(rgb(imgROMA[,,1],imgROMA[,,2],imgROMA[,,3], imgROMA[,,4] * 0.2), nrow=dim(imgROMA)[1])

ggplot(uStatRoma, aes(x = ROM_xg, y = ROM_goals, color = ROM_win_prob)) +
  geom_point(size = 10) +
  scale_color_gradient(low = "red", high = "green") +
  theme_bw() +
  labs(title = "Roma Goals by Win Probability",
       y = "Goals",
       x = "Expected Goals",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com") +
  guides(color = guide_legend(title = "Win Probability"))+
  annotation_custom(rasterGrob(im2ROMA,
                               width = unit(.8,"npc"), 
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



ggplot(uStatRoma, aes(x = match_number, color = win_loss, size = ROM_win_prob)) +
  geom_point(aes(y = ROM_goals)) +
  # geom_point(aes(y = ROM_xg), color = "red", fill = "red") +  # Ensure this color mapping is outside aes()
  theme_bw() +
  labs(title = "Roma Match Form",
       y = "Goals",
       x = "Match Week",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com",
       color = "Match Result",
       size = "Predicted Win %") +
  guides(color = guide_legend(override.aes = list(size = 6))) +  # Only adjust the size of points in the color legend
  scale_color_manual(values = c(Draw = "#f0bc42",
                                Loss = "#cacacc",
                                Win = "#8e1f2f")) +
  scale_size_continuous(range = c(2, 12))+
  annotation_custom(rasterGrob(im2ROMA,
                               width = unit(.8, "npc"), 
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
uStatRoma$ExpectedGoals <- "Expected Goals"

ggplot(uStatRoma, aes(x = match_number)) +
  geom_segment(aes(x = match_number, xend = match_number, y = ROM_goals, yend = ROM_xg), color = "gray") +  # Add connecting bars
  geom_point(aes(y = ROM_goals, color = win_loss), size = 9) +
  geom_point(aes(y = ROM_xg, shape = ExpectedGoals), size = 5, color = "black", fill = "black") +  # Add a dummy shape for the legend
  theme_bw() +
  labs(title = "Roma Match Form",
       y = "Goals",
       x = "Match Week",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com",
       color = "Match Result",
       shape = "") +  # Add a shape legend title
  guides(color = guide_legend(override.aes = list(size = 5)),  # Adjust the size of points in the color legend
         shape = guide_legend(override.aes = list(size = 5, color = "black"))) +  # Adjust the size and color of points in the shape legend
  scale_color_manual(values = c(Draw = "#f0bc42",
                                Loss = "#cacacc",
                                Win = "#8e1f2f")) +
  scale_shape_manual(values = c("Expected Goals" = 16)) +  # Assign shape to the dummy variable
  scale_size_continuous(range = c(2, 10)) +  # Adjust the range of sizes
  annotation_custom(rasterGrob(im2ROMA,
                               width = unit(.8, "npc"), 
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