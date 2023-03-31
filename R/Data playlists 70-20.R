library(flexdashboard)
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(tufte)
library(plotly)
library(compmus)

seventies_playlist <- get_playlist_audio_features("", "37i9dQZF1DWTJ7xPn4vNaz?")
eighties_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4UtSsGT1Sbe?")
nineties_playlist <- get_playlist_audio_features("", "37i9dQZF1DXbTxeAdrVG2l?")
zeros_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4o1oenSJRJd?")
tens_playlist <- get_playlist_audio_features("", "37i9dQZF1DX5Ejj0EkURtP?")
twenties_playlist <- get_playlist_audio_features("", "4vSTV61efRmetmaoz95Vet?")


All_playlist <-
  bind_rows(
    seventies_playlist |> mutate(category = "70s"),
    eighties_playlist |> mutate(category = "80s"),
    nineties_playlist |> mutate(category = "90s"),
    zeros_playlist |> mutate(category = "00s"),
    tens_playlist |> mutate(category = "10s"),
    twenties_playlist |> mutate(category = "20s")
  )

saveRDS(object = All_playlist,file = "data/All_playlist.RDS")

