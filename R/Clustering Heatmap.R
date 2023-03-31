## Breakout session 5

library(tidyverse)
library(tidymodels)
library(ggdendro)
library(plotly)
library(heatmaply)
library(spotifyr)
library(compmus)

get_conf_mat <- function(fit) {
  outcome <- .get_tune_outcome_names(fit)
  fit |> 
    collect_predictions() |> 
    conf_mat(truth = outcome, estimate = .pred_class)
}  

get_pr <- function(fit) {
  fit |> 
    conf_mat_resampled() |> 
    group_by(Prediction) |> mutate(precision = Freq / sum(Freq)) |> 
    group_by(Truth) |> mutate(recall = Freq / sum(Freq)) |> 
    ungroup() |> filter(Prediction == Truth) |> 
    select(class = Prediction, precision, recall)
}


## Homework

seventies_playlist <- get_playlist_audio_features("", "37i9dQZF1DWTJ7xPn4vNaz?")|>
  slice(1:15) |>
  add_audio_analysis()
eighties_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4UtSsGT1Sbe?")  |>
  slice(1:15) |>
  add_audio_analysis()
nineties_playlist <- get_playlist_audio_features("", "37i9dQZF1DXbTxeAdrVG2l") |>
  slice(1:15) |>
  add_audio_analysis()
zeros_playlist <- get_playlist_audio_features("", "37i9dQZF1DX4o1oenSJRJd?") |>
  slice(1:15) |>
  add_audio_analysis()
tens_playlist <- get_playlist_audio_features("", "37i9dQZF1DX5Ejj0EkURtP?") |>
  slice(1:15) |>
  add_audio_analysis()
twenties_playlist <- get_playlist_audio_features("", "4vSTV61efRmetmaoz95Vet?") |>
  slice(1:15) |>
  add_audio_analysis()

All_playlist_analysis <-
  bind_rows(
    seventies_playlist |> mutate(playlist = "70s"),
    eighties_playlist |> mutate(playlist = "80s"),
    nineties_playlist |> mutate(playlist = "90s"),
    zeros_playlist |> mutate(playlist = "00s"),
    tens_playlist |> mutate(playlist = "10s"),
    twenties_playlist |> mutate(playlist = "20s")
  )

Arranged_playlists <- All_playlist_analysis %>%
  mutate(playlist = factor(playlist, levels = c(
    "70s", "80s", "90s", "00s", "10s", "20s" ))) %>%
  filter(loudness > -18) %>%
  mutate(
    segments = map2(segments, key, compmus_c_transpose),
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "mean", norm = "manhattan"
      ),
    timbre =
      map(
        segments,
        compmus_summarise, timbre,
        method = "mean"
      )
  ) |>
  mutate(pitches = map(pitches, compmus_normalise, "clr")) |>
  mutate_at(vars(pitches, timbre), map, bind_rows) |>
  unnest(cols = c(pitches, timbre))


Seventies_juice <-
  recipe(
    track.name ~
      loudness + c01,
    data = Seventies_years
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(Arranged_playlists |> mutate(track.name = str_trunc(track.name, 30))) |>
  juice() |>
  column_to_rownames("track.name")

Clustering_heat <- heatmaply(
  Seventies_juice,
  hclustfun = hclust,
  hclust_method = "average",  # Change for single, average, or complete linkage.
  dist_method = "euclidean"
)

Clustering_heat

saveRDS(object = Clustering_heat,file = "data/Cluster_heat.RDS")
