library(flexdashboard)
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(tufte)
library(plotly)
library(compmus)

seventies_playlist <- get_playlist_audio_features("", "37i9dQZF1DWTJ7xPn4vNaz?")
eighties_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4UtSsGT1Sbe?")
nineties_playlist <- get_playlist_audio_features("", "37i9dQZF1DXbTxeAdrVG2l")
zeros_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4o1oenSJRJd?")
tens_playlist <- get_playlist_audio_features("", "37i9dQZF1DX5Ejj0EkURtP?")
twenties_playlist <- get_playlist_audio_features("", "4vSTV61efRmetmaoz95Vet?")

RHCP_Songs <- get_playlist_audio_features("", "761qISmudNnnQ99BvnU4wo?")

All_playlist <-
  bind_rows(
    seventies_playlist |> mutate(category = "Seventies"),
    eighties_playlist |> mutate(category = "Eighties"),
    nineties_playlist |> mutate(category = "Nineties"),
    zeros_playlist |> mutate(category = "Zeros"),
    tens_playlist |> mutate(category = "Tens"),
    twenties_playlist |> mutate(category = "20s")
  )


Arranged_playlists <- All_playlist %>%
  filter(loudness > -18) %>%
  summarise(playlist_name)

Ar <-factor(All_playlist, levels = c("70s", "80s", "90s", "00s", "10s", "20s"))
  

Mean_Loudness_playlists <- Arranged_playlists %>%
  group_by(playlist_name) %>%
  summarise(mean_loudness = mean(loudness))

Mean_Energy_playlists <- Arranged_playlists %>%
  group_by(playlist_name) %>%
  summarise(mean_energy = mean(energy))

PLaylist_labs <- c("70s", "80s", "90s", "00s", "10s", "20s")

Arranged_Album_RHCP_Songs <- RHCP_Songs %>%
  arrange(track.album.release_date)


Plot_Arranged_playlists <- ggplot(Arranged_playlists, aes(Ar, loudness, text = paste(track.name))) +
  geom_boxplot() +
  scale_x_discrete(labels= PLaylist_labs) +
  # Theme
  theme_classic() +
  theme(axis.line.x = element_blank()
  ) +
  # Labs
  labs(title = "Loudness Through The Decades",
       caption = "Source: Spotify"
  ) +
  
  xlab("Playlists") +
  
  ylab("Relative Loudness in DB")

# plotly
ggplotly(Plot_Arranged_playlists)



  
  ### Chart B

Plot_Mean_Loudness_playlists <- ggplot(Mean_Loudness_playlists, aes(playlist_name, mean_loudness)) +
  geom_point() +
  scale_x_discrete(labels= PLaylist_labs) +
  # Theme
  theme_classic() +
  theme(axis.line.x = element_blank()
  ) +
  # Labs
  labs(title = "Mean Loudness Through The Decades",
       caption = "Source: Spotify"
  ) +
  
  xlab("Playlists") +
  
  ylab("Mean Relative Loudness in DB")

