library(flexdashboard)
library(tidyverse)
library(spotifyr)
library(ggplot2)
library(tufte)
library(plotly)
library(compmus)



Californication <-
  get_tidy_audio_analysis("34KTEhpPjq6IAgQg2yzJAL?") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

Plot_Californication <- Californication |>
  mutate(pitches = map(pitches, compmus_normalise, "chebyshev")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()


saveRDS(object = Plot_Californication,file = "data/Plot-Californication.RDS")