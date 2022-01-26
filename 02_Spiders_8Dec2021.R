library(tidyverse)
library(sf)
library(rnaturalearth)
library(tidygeocoder)
library(gggibbous)
library(ggplot2)

# Import Data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-12-07')

tuesdata$spiders %>%
  ggplot(aes(x=year)) + geom_histogram() +
  cowplot::theme_cowplot() +
  labs(x= "Year", y = "Number", Title = "Year Spider Named")
