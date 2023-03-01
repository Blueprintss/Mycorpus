library(tidyverse)
library(spotifyr)
library(ggplot2)
library(tufte)
library(plotly)

seventies_playlist <- get_playlist_audio_features("", "37i9dQZF1DWTJ7xPn4vNaz?")
eighties_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4UtSsGT1Sbe?")
nineties_playlist <- get_playlist_audio_features("", "37i9dQZF1DXbTxeAdrVG2l")
zeros_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4o1oenSJRJd?")
tens_playlist <- get_playlist_audio_features("", "37i9dQZF1DX5Ejj0EkURtP?")
twenties_playlist <- get_playlist_audio_features("", "4vSTV61efRmetmaoz95Vet?")

All_playlist <-
  bind_rows(
    seventies_playlist |> mutate(category = "Seventies"),
    eighties_playlist |> mutate(category = "Eighties"),
    nineties_playlist |> mutate(category = "Nineties"),
    zeros_playlist |> mutate(category = "Zeros"),
    tens_playlist |> mutate(category = "Tens"),
    twenties_playlist |> mutate(category = "Twenties")
  )

Arranged_playlists <- All_playlist %>%
  arrange(playlist_name)

PLaylist_labs <- c("70s", "80s", "90s", "00s", "10s", "20s")

Plot_Arranged_playlists <- ggplot(Arranged_playlists, aes(playlist_name, loudness)) +
  geom_boxplot() +
  scale_x_discrete(limits=c("All Out 70s", "All Out 80s", "All Out 90s", "All Out 2000s", "All Out 2010s", "All Out 2020s"),
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

# plotly
ggplotly(Plot_Arranged_playlists)




## Plot 2

Mean_playlists <- Arranged_playlists %>%
  group_by(playlist_name) %>%
  summarise(mean_loudness = mean(loudness))

ggplot(Mean_playlists, aes(playlist_name, mean_loudness) +
         geom_point()

