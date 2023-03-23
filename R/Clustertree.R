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


All_Analysis <- readRDS(file = "data/All_playlist_analysis.RDS")

All_Analysis |>
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
      energy +
      loudness +
      valence,
    data = All_Analysis
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(All_Analysis |> mutate(track.name = str_trunc(track.name, 20))) |>
  juice() |>
  column_to_rownames("track.name")


All_dist <- dist(Seventies_juice, method = "euclidean")

Clustering_tree <- All_dist |> 
  hclust(method = "average") |> # Try single, average, and complete.
  dendro_data() |>
  ggdendrogram()


saveRDS(object = Clustering_tree,file = "data/Clustertree.RDS")