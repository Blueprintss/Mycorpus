library(flexdashboard)
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(tufte)
library(plotly)
library(compmus)

##SD2

All_playlist_analysis <- readRDS(file = "data/All_playlist_analysis.RDS")

Arranged_playlists_analysis <- All_playlist_analysis %>%
  arrange(playlist_name) %>%
  mutate(category = factor(category, levels = c(
    "70s", "80s", "90s", "00s", "10s", "20s" )))

SDB <- Arranged_playlists_analysis |>
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) |>
  unnest(sections) |>
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = category,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal()+
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Decade",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  ) 

saveRDS(object = SDB,file = "data/SDB.RDS")

