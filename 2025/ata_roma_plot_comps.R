source("2025/SerieA Team Match Ratings.R")

library(gghighlight)
library(cowplot)

xg_comp1 <- ggplot(nn, aes(x=datetime,y=xg,color = teamHelper))+
  geom_line(size=3)+
  geom_point(size=5)+
  gghighlight(teamHelper %in% c("Atalanta","Roma"))+
  scale_color_manual(values = c("Atalanta" = "#1e71b8", "Roma" = "#8e1f2f"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(title = "Expected Goals by Week 2024/2025",
       y = "xG ",
       x = "Date")


xg_comp2 <- ggplot(nn, aes(x=datetime,y=goals,color = teamHelper))+
  geom_line(size=3)+
  geom_point(size=5)+
  gghighlight(teamHelper %in% c("Atalanta","Roma"))+
  scale_color_manual(values = c("Atalanta" = "#1e71b8", "Roma" = "#8e1f2f"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(title = "Goals by Week 2024/2025",
       y = "# of Goals ",
       x = "Date",
       caption = "Source: Understat.com")


cbind_xg_g_plot <- plot_grid(xg_comp1,
          xg_comp2,
          ncol = 1)

ggsave(plot = cbind_xg_g_plot,filename = "images/ata_roma_xg_g_combined.png")