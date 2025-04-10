source("2025/SerieA Team Match Ratings.R")

my_team2 <- nn %>% filter(teamHelper == "Roma")

top_4 <- mr_table %>% top_n(4) %>% 
  select(1)

t4_filter <- top_4$team


roma_xg_comp_plot <- ggplot() +
  stat_summary(data = nn, aes(x = datetime, y = xg, linetype = "Overall Mean"), 
               geom = "line", fun = "mean", color = "black", linewidth = 1,alpha = 0.2) +
  stat_summary(data = nn %>% filter(teamHelper %in% t4_filter), 
               aes(x = datetime, y = xg, linetype = "Top 4 Mean"), 
               geom = "line", fun = "mean", color = "red", linewidth = 1,alpha = 0.2) +
  scale_linetype_manual(values = c("Overall Mean" = "solid", "Top 4 Mean" = "dashed"),
                        name = "Lines") +
  geom_line(data = my_team2, aes(x = datetime, y = xg, linetype = "Selected Team"), 
            color = "#8e1f2f", linewidth = 1) +
  geom_point(data = my_team2, aes(x = datetime, y = xg, linetype = "Selected Team"), 
             color = "#8e1f2f", size=4)+
  scale_linetype_manual(values = c("Overall Mean" = "solid", 
                                   "Top 4 Mean" = "solid", 
                                   "Selected Team" = "solid"),
                        name = "Key") +
  labs(title = "Roma Expected Goals 2025",
       y = "xG ",
       x = "Date",
       subtitle = "Serie A 2024/25 Season",
       caption = "Source: Understat.com")+
  theme_bw()

ggsave(plot = roma_xg_comp_plot,filename = "images/roma_xg_comp_plot.png")


