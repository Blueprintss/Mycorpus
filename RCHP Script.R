
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
  summarise(mean_loudness_rhcp = mean(loudness))

  
ggplot(Arranged_Album_RHCP_Songs, aes(track.album.release_date, mean_loudness_rhcp)) +
         geom_point()
      
# Idee: plot mean rhcp samen met all out playlists.
       