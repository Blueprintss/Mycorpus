# Breakout Session 2

library(tidyverse)
library(spotifyr)
library(compmus)

Californication <-
  get_tidy_audio_analysis("34KTEhpPjq6IAgQg2yzJAL?") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

Californication |>
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


