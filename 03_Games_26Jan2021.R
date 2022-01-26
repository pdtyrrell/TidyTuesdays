library(tidyverse)
library(lubridate)

# Import Data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
tuesdata <- tidytuesdayR::tt_load(2022, week = 4)

ratings <- tuesdata$ratings
details <- tuesdata$details


# Has averge playing time decreased over time -----------------------------

combined <- ratings %>%
  left_join(details, by = "id") %>%
  select(year, name, rank, average, playingtime, maxplayers) %>%
  filter(year <2022 & year >1950) %>%
  filter(maxplayers < 16) %>%
  filter(playingtime >0 & playingtime <6*60) %>%
  mutate(Decade = year - year %% 10) %>%
  mutate(playintime = as.duration(ms(paste0(playingtime, ":00"))))

# Are shorter games more popular? -----------------------------
library(ggrepel)

topgames<-combined %>%
  group_by(Decade) %>%
  filter(average == max(average))

bottomgames <- combined %>%
  group_by(Decade) %>%
  filter(average == min(average))

out<-combined %>%
  ggplot(aes(y=average ,x = playintime)) +
  scale_x_time() +
  geom_point() + geom_smooth(method = "lm", col = "red") +
  cowplot::theme_cowplot(font_size =10) +
  ylim(c(0,10)) +
  geom_point(data = topgames, 
             aes(y=average ,
                 x = playintime),
             colour = "red")+
             
  geom_label_repel(data = topgames, size = 3,
                  aes(y=average ,
                      x = playintime, 
                      label = paste0(Decade, ": ", name)), 
                  box.padding = 1, 
                  min.segment.length = 0,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  segment.angle = 20,
                  hjust =0,
                  nudge_y      = 0.05,
                  direction    = "x",
                  arrow = arrow(length = unit(0.015, "npc"))
                  ) +
  labs(x = "Playing Time", y = "Average Rating", title = "Are shorter board games more fun?",
       subtitle = "In fact, longer games are generally more higher rated (red line). With the highest rated game per decade labelled.")
cowplot::save_plot("Shorter_Games.pdf", out, base_height = 7)

"pwoiopeo_32451235as peter" %>%
str_extract_all(.,"[:alpha:]+|[:space:]" )
