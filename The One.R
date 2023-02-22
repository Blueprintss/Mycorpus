library(tidyverse)
library(spotifyr)
library(ggplot2)

seventies_playlist <- get_playlist_audio_features("", "37i9dQZF1DWTJ7xPn4vNaz?")
eighties_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4UtSsGT1Sbe?")
nineties_playlist <- get_playlist_audio_features("", "37i9dQZF1DXbTxeAdrVG2l")
zeros_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4o1oenSJRJd?")
tens_playlist <- get_playlist_audio_features("", "37i9dQZF1DX5Ejj0EkURtP?")

All_playlist <-
    bind_rows(
      seventies_playlist |> mutate(category = "Seventies"),
      eighties_playlist |> mutate(category = "Eighties"),
      nineties_playlist |> mutate(category = "Nineties"),
      zeros_playlist |> mutate(category = "Zeros"),
      tens_playlist |> mutate(category = "Tens")
    )

Arranged_playlists <- All_playlist %>%
  arrange(playlist_name)

PLaylist_labs <- c("Seventies", "Eighties", "Nineties", "Zeros", "Tens")

ggplot(Arranged_playlists, aes(playlist_name, loudness)) +
  geom_boxplot() +
  scale_x_discrete(limits=c("All Out 70s", "All Out 80s", "All Out 90s", "All Out 2000s", "All Out 2010s"),
                   labels= PLaylist_labs) +
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
