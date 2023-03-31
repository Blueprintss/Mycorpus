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

Mean_Loudness_playlists <- Arranged_playlists %>%
  group_by(category) %>%
  mutate(mean_loudness = mean(loudness))

Plot_Mean_Loudness_playlists <- ggplot(Mean_Loudness_playlists, aes(category, mean_loudness)) +
  geom_point() +

# Theme
  theme_classic() +
  theme(axis.line.x = element_blank()
  ) +
  # Labs
  labs(title = "Mean Loudness Through The Decades",
       caption = "Source: Spotify"
  ) +
  
  xlab("Decade") +
  
  ylab("Mean Relative Loudness (dBFS)")


saveRDS(object = Plot_Mean_Loudness_playlists,file = "data/Plot-Mean_Loudness_playlist.RDS")




