
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

Mean_Energy_Loudness_playlists <- Arranged_playlists %>%
  group_by(category) %>%
  mutate(mean_energy = mean(energy), mean_loudnesss = mean(loudness))

Plot_Mean_Energy_Loudness_playlists <- ggplot(Mean_Energy_Loudness_playlists, aes(category, mean_energy, size = mean_loudnesss)) +
  geom_point() +
  
  # Theme
  theme_classic() +
  theme(axis.line.x = element_blank()
  ) +
  # Labs
  labs(title = "Mean Energy Through The Decades",
       caption = "Source: Spotify"
  ) +
  
  xlab("Playlists") +
  
  ylab("Mean Energy")



saveRDS(object = Plot_Mean_Energy_Loudness_playlists,file = "data/Plot-Mean_Energy_Loudness_playlists.RDS")




