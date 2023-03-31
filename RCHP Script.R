
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(tufte)
library(plotly)
library(compmus)



RHCP_Songs <- get_playlist_audio_features("", "761qISmudNnnQ99BvnU4wo?")

Arranged_Album_RHCP_Songs <- RHCP_Songs %>%
  arrange(track.album.release_date) %>%
  group_by(track.album.release_date) %>%
  summarise(mean_loudness_rhcp = mean(loudness)) %>%
  mutate(is_outlier = mean_loudness_rhcp == -6.520000) %>%
  filter(!is_outlier)

  
RHCP_Plot <- ggplot(Arranged_Album_RHCP_Songs, aes(track.album.release_date, mean_loudness_rhcp)) +
      geom_point() +

# Theme
theme_classic() +
  theme(axis.line.x = element_blank()
  ) +
  # Labs
  labs(title = "Loudness RHCP Through The Decades",
  ) +
  
  xlab("RHCP Album Release Dates ") +
  
  ylab("Mean Relative Loudness (dBFS)")

ggplotly(RHCP_Plot)

saveRDS(object = RHCP_Plot,file = "data/RHCP_Plot.RDS")
  
# Idee: plot mean rhcp samen met all out playlists.
# Outlier: 2022 after april
       