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
  mutate(away_team_mr = rescale(away_team_mr)*10) %>% 
  mutate(home_result = case_when(home_goals > away_goals ~ "win",
                                 home_goals == away_goals ~ "draw",
                                 TRUE ~ "loss")) %>% 
  mutate(home_points = case_when(home_goals > away_goals ~ 3,
                                 home_goals == away_goals ~ 1,
                                 TRUE ~ 0)) %>% 
  mutate(away_result = case_when(away_goals > home_goals ~ "win",
                                 away_goals == home_goals ~ "draw",
                                 TRUE ~ "loss")) %>% 
  mutate(away_points = case_when(away_goals > home_goals ~ 3,
                                 away_goals == home_goals ~ 1,
                                 TRUE ~ 0))


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



team_names <- all_serie_a %>% 
  select(home_abbr) %>% 
  distinct()



nn <- all_serie_a %>% 
  pivot_longer(cols = c(6,9),
               names_to = "team_status",
               values_to = "teamHelper") %>% 
  mutate(goals = case_when(team_status == "away_team" ~ away_goals,
                            team_status == "home_team" ~ home_goals)) %>% 
  group_by(match_id) %>%
  mutate(win_loss = case_when(
    goals == max(goals) & goals != min(goals) ~ "Win",
    goals == min(goals) & goals != max(goals) ~ "Loss",
    goals == max(goals) & goals == min(goals) ~ "Draw"
  )) %>%
  ungroup() %>% 
  mutate(points= case_when(win_loss == "Win" ~ 3,
                           win_loss == "Loss" ~ 0,
                           TRUE ~ 1)) %>% 
  mutate(xg = case_when(team_status == "away_team" ~ away_xG,
                        team_status == "home_team" ~ home_xG)) %>% 
  mutate(goal_diff =  case_when(team_status == "away_team" ~ away_goals-home_goals,
                                team_status == "home_team" ~ home_goals - away_goals)) %>% 
  mutate(xg_diff = case_when(team_status == "away_team" ~ away_xG-home_xG,
                             team_status == "home_team" ~ home_xG - away_xG)) %>% 
  select(match_id,23:30) %>% 
  group_by(match_id) %>% 
  mutate(goals_against = case_when(
    team_status == "home_team" ~ goals[team_status == "away_team"],
    team_status == "away_team" ~ goals[team_status == "home_team"]
  )) %>%
  ungroup()



stats <- nn %>% 
  group_by(teamHelper) %>% 
  summarize(avg_goal_diff = mean(goal_diff, na.rm = TRUE),
            avg_goal = mean(goals, na.rm = TRUE),
            avg_xg = mean(xg,na.rm = TRUE),
            avg_xg_diff = mean(xg_diff,na.rm = TRUE),
            ppg = mean(points, na.rm = TRUE),
            points = sum(points,na.rm = TRUE),
            goals = sum(goals, na.rm = TRUE),
            goal_diff = sum(goal_diff,na.rm = TRUE),
            goals_against_tot = sum(goals_against)) %>%  
  mutate(teamHelper = case_when(teamHelper == "Parma Calcio 1913" ~ "Parma",
                                TRUE ~ teamHelper))



ggplot(stats, aes(x=ppg,y=avg_xg_diff,label=teamHelper))+geom_text()+
  labs(title = "Expected Goals Points Per Game",
       y = "xG Differental",
       x = "Points Per Game",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com")+
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "bold"),
        panel.background = element_rect(color = "black",fill = "white"))+
  geom_hline(yintercept = mean(stats$avg_xg_diff))+
  geom_vline(xintercept = mean(stats$ppg))


goals_plot <- ggplot(stats, aes(x=goals,y=goals_against_tot,label=teamHelper,color=points))+geom_text()+
  labs(title = "Goals By Goals Against",
       y = "Goal Agianst",
       x = "Goals",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com")+
  theme(axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "bold"),
        panel.background = element_rect(color = "black",fill = "white"))+
  geom_hline(yintercept = mean(stats$goals_against_tot))+
  geom_vline(xintercept = mean(stats$goals)) %>% 
  geom_hline(yintercept = quantile(stats$goals_against_tot, probs = 0.15),linetype="dashed",color="red")+
  geom_hline(yintercept = quantile(stats$goals_against_tot, probs = 0.85),linetype="dashed",color="red")

ggplot2::ggsave(plot = goals_plot,filename = "images/goals_plot.png")




ggplot(stats, aes(x=points,y=goal_diff,label=teamHelper))+geom_text()+
  geom_hline(yintercept = mean(stats$goal_diff))+
  geom_vline(xintercept = mean(stats$points))




library(gt)
library(gtExtras)

adv_stats_tbl <- stats %>% 
  arrange(desc(points)) %>% 
  gt() %>% 
  data_color(
    columns = 2:9, palette = c("red", "green")
  ) %>% 
  data_color(
    columns = 10, palette = c("green","red")
  ) %>% 
  fmt_number(columns = c(2:6),
             decimals = 2) %>% 
  cols_label(
    teamHelper = "Team",
    avg_goal_diff = "Avg Goal Diff",
    avg_goal = "Average Goal Per Game",
    avg_xg = "xG Per Game",
    avg_xg_diff = "xG Diff",
    ppg = "PPG",
    points = "Points",
    goals= "Goals",
    goal_diff = "Goal Diff",
    goals_against_tot = "Goals Against"
  ) %>% 
  cols_align(align = "center",
             columns = everything()) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_header(
    title = md("**Serie A 2024/25 Advanced Stats**")) %>% 
  tab_options(
    table.border.top.style = "none",
    footnotes.font.size = 12,
    table.margin.right = 50000
  ) %>% 
  tab_footnote(footnote = "Understat.com",
               placement = "left") %>% 
  cols_width(c(2:10) ~ px(90),
             teamHelper ~ 100)

adv_stats_tbl <- stats %>% 
  arrange(desc(points)) %>% 
  mutate(spacer = "") %>%  # Add an empty spacer column
  gt() %>% 
  data_color(
    columns = 2:9, palette = c("red", "green")
  ) %>% 
  data_color(
    columns = 10, palette = c("green", "red")
  ) %>% 
  fmt_number(columns = c(2:6),
             decimals = 2) %>% 
  cols_label(
    teamHelper = "Team",
    avg_goal_diff = "Avg Goal Diff",
    avg_goal = "Average Goal Per Game",
    avg_xg = "xG Per Game",
    avg_xg_diff = "xG Diff",
    ppg = "PPG",
    points = "Points",
    goals = "Goals",
    goal_diff = "Goal Diff",
    goals_against_tot = "Goals Against",
    spacer = ""  # Label for the spacer column
  ) %>% 
  cols_align(align = "center",
             columns = everything()) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_header(
    title = md("**Serie A 2024/25 Advanced Stats**")
  ) %>% 
  tab_options(
    table.border.top.style = "none",
    footnotes.font.size = 12
  ) %>% 
  tab_footnote(footnote = "Understat.com",
               placement = "left") %>% 
  cols_width(c(2:10) ~ px(90),
             teamHelper ~ 100,
             spacer ~ px(110))  # Set the width of the spacer column


gtsave(adv_stats_tbl,"images/adv_stats_tbl.png",expand = 10)

adv_stats_tbl



ggplot(stats, aes(x=goals,y=avg_xg_diff,label=teamHelper))+geom_text()+
  geom_hline(yintercept = mean(stats$avg_xg_diff))+
  geom_vline(xintercept = mean(stats$goals))



ggplot(stats, aes(x=ppg,y=avg_goal,label=teamHelper))+geom_text()+
  geom_hline(yintercept = mean(stats$avg_goal))+
  geom_vline(xintercept = mean(stats$ppg))





