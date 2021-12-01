library(tidyverse)
library(sf)
library(rnaturalearth)
library(tidygeocoder)
library(gggibbous)

# Import the data ---------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-11-30')
matches <- tuesdata$matches

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')


# Get statistics ----------------------------------------------------------

mactsums<-matches %>%
  filter(str_detect(series, "tour")) %>% # only want tours not cups/world cups... misses some games
  select(team1, team2, winner, team1_away_or_home, ground_city, ground_country,series) %>%
  mutate(Home_Team = case_when(team1_away_or_home == "away" ~ team2,
                               team1_away_or_home == "home" ~ team1)) %>%
  mutate(Location = paste0(ground_city,", ", ground_country)) %>%
  select(Home_Team, winner, Location, series) %>%
  mutate(Home_win = if_else(Home_Team == winner, 1,0)) %>%
  group_by(Home_Team, Home_win, Location) %>%
  mutate(Total_Games = n()) %>%
  group_by(Home_Team, Location) %>%
  summarise(Total_Home_win = sum(Home_win), Total_Games= n()) %>%
  mutate(Percent_Home_Win = Total_Home_win/Total_Games) %>%
  filter(Home_Team != "Africa XI" & Home_Team != "Asia XI")


# Find these city -------------------------------------------------------

geolocated <- mactsums %>% 
  ungroup() %>%
  distinct(Location) %>%
  geocode(Location,method= 'osm', lat = latitude , long = longitude)

citmatch<-mactsums %>%
  left_join(geolocated) %>%
  filter(Home_Team  == "India") %>%
  filter(!str_detect(Location, "Canada"))

# Create a plot -----------------------------------------------------------


moonmap <- ggplot() +
  geom_polygon(
    data = map_data(
      "world", region = "India"),
    aes(group = group, x=long, y =lat),
    fill = "gray80"
  ) +
  geom_moon(data = citmatch,
    aes(x = longitude, y= latitude, ratio = Percent_Home_Win, size = Total_Games),
    right = FALSE, fill = "dark green", color = "dark green",
    key_glyph = draw_key_moon_left) +
  geom_moon(data = citmatch,
            aes(x = longitude, y= latitude, ratio = 1-Percent_Home_Win, size = Total_Games),
           fill = "dark red", color = "black") +
  cowplot::theme_map() +
  theme(
    legend.position = c(0.05, 0.05),
    legend.direction = "horizontal",
    legend.justification = c(0, 0)
  )

moonmap +
  scale_size("size", range = c(4,10), breaks = 2^(1:3)) +coord_equal()
  