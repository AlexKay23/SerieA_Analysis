source("2025/SerieA Team Match Ratings.R")

library(gghighlight)

ggplot(nn, aes(x=datetime,y=xg,color = teamHelper))+
  geom_line(size=3)+
  geom_point(size=5)+
  gghighlight(teamHelper %in% c("Atalanta","Roma"))+
  theme_bw()


ggplot(nn, aes(x=datetime,y=goals,color = teamHelper))+
  geom_line(size=3)+
  geom_point(size=5)+
  gghighlight(teamHelper %in% c("Atalanta","Roma"))+
  theme_bw()

