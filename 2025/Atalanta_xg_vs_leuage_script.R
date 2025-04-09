
source("SerieA Team Match Ratings.R")

my_team <- nn %>% filter(teamHelper == "Atalanta")

top_4 <- mr_table %>% top_n(4) %>% 
  select(1)

t4_filter <- top_4$team


ggplot() +
  stat_summary(data = nn, aes(x = datetime, y = xg, linetype = "Overall Mean"), 
               geom = "line", fun = "mean", color = "black", linewidth = 1,alpha = 0.2) +
  stat_summary(data = nn %>% filter(teamHelper %in% t4_filter), 
               aes(x = datetime, y = xg, linetype = "Top 4 Mean"), 
               geom = "line", fun = "mean", color = "red", linewidth = 1,alpha = 0.2) +
  scale_linetype_manual(values = c("Overall Mean" = "solid", "Top 4 Mean" = "dashed"),
                        name = "Lines") +
  geom_line(data = my_team, aes(x = datetime, y = xg, linetype = "Selected Team"), 
            color = "#1e71b8", linewidth = 1) +
  geom_point(data = my_team, aes(x = datetime, y = xg, linetype = "Selected Team"), 
            color = "#1e71b8", size=4)+
  scale_linetype_manual(values = c("Overall Mean" = "solid", 
                                   "Top 4 Mean" = "solid", 
                                   "Selected Team" = "solid"),
                        name = "Key") +
  labs(title = "Atalanta Expected Goals 2025",
       y = "xG ",
       x = "Date",
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
        panel.background = element_rect(color = "black",fill = "white"),
        legend.key = element_blank())



