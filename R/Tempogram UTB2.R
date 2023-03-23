## Tempogram UTB2


library(tidyverse)
library(spotifyr)
library(compmus)

UTB <-
  get_tidy_audio_analysis("3d9DChrdc6BOeFsbrZ3Is0") |>
  select(segments) |>
  unnest(segments)

UTB2 <- UTB |>
  mutate(loudness_max_time = start + loudness_max_time) |>
  arrange(loudness_max_time) |>
  mutate(delta_loudness = loudness_max - lag(loudness_max)) |>
  ggplot(aes(x = loudness_max_time, y = pmax(0, delta_loudness))) +
  geom_line() +
  xlim(0, 60) +
  ylim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

saveRDS(object = UTB2,file = "data/Tempo_UTB2.RDS")