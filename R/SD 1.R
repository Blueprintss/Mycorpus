library(flexdashboard)
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(tufte)
library(plotly)
library(compmus)

##SD1

All_playlist_analysis <- readRDS(file = "data/All_playlist_analysis.RDS")

Arranged_playlists_analysis <- All_playlist_analysis %>%
  arrange(playlist_name) %>%
  mutate(category = factor(category, levels = c(
    "70s", "80s", "90s", "00s", "10s", "20s" )))

SDA <- Arranged_playlists_analysis |>
  mutate(
    timbre =
      map(
        segments,
        compmus_summarise,
        timbre,
        method = "mean"
      )
  ) |>
  select(category, timbre) |>
  compmus_gather_timbre() |>
  ggplot(aes(x = basis, y = value, fill = category)) +
  geom_violin() +
  scale_fill_viridis_d() +
  labs(x = "Spotify Timbre Coefficients", y = "", fill = "Decade")

saveRDS(object = SDA,file = "data/SDA.RDS")

