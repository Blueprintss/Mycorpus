library(flexdashboard)
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(tufte)
library(plotly)
library(compmus)

## 70s-20s
All_playlist <- readRDS(file = "data/All_playlist.RDS")

Arranged_playlists <- All_playlist %>%
  mutate(category = factor(category, levels = c(
    "70s", "80s", "90s", "00s", "10s", "20s" ))) %>%
  filter(loudness > -18) %>%
  arrange(category)

Plot_Arranged_playlists <- ggplot(Arranged_playlists, aes(category, loudness, text = paste(track.name))) +
  geom_boxplot() +

  # Theme
  theme_classic() +
  theme(axis.line.x = element_blank()
  ) +
  # Labs
  labs(title = "Loudness Through The Decades",
       caption = "Source: Spotify"
  ) +
  
  xlab("Decade") +
  
  ylab("Relative Loudness (dBFS)")


saveRDS(object = Plot_Arranged_playlists,file = "data/Boxplot-Arranged_playlists.RDS")

