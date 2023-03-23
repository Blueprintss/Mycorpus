library(tidyverse)
library(spotifyr)
library(compmus)



Snow <-
  get_tidy_audio_analysis("2aibwv5hGXSgw7Yru8IYTO") |>
  select(segments) |>
  unnest(segments)

Snow2 <-Snow |>
  mutate(loudness_max_time = start + loudness_max_time) |>
  arrange(loudness_max_time) |>
  mutate(delta_loudness = loudness_max - lag(loudness_max)) |>
  ggplot(aes(x = loudness_max_time, y = pmax(0, delta_loudness))) +
  geom_line() +
  xlim(0, 60) +
  ylim(0, 30) +
  theme_minimal() +
  labs(x = "Time (s)", y = "Novelty")

saveRDS(object = Snow2,file = "data/Tempo_Snow2.RDS")