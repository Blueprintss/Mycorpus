## Extra Clustertree

library(tidymodels)
library(ggdendro)

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

Arranged_playlistss <- All_playlist_analysis %>%
  mutate(playlist = factor(playlist, levels = c(
    "70s", "80s", "90s", "00s", "10s", "20s" ))) %>%
  filter(loudness > -18)

tracks_to_remove <- Arranged_playlistss |>
  count(track.name) |>
  filter(track.name == "A&W") %>%
  select(track.name)

Arranged_playlists <- Arranged_playlistss %>%
  anti_join(tracks_to_remove)

# Note that you are not allowed to have duplicate songs in the dataset! 



indie_juice <-
  recipe(
    track.name ~
      loudness,
    data = Arranged_playlists
  ) |>
  step_center(all_predictors()) |>
  step_scale(all_predictors()) |> 
  # step_range(all_predictors()) |> 
  prep(Arranged_playlists |> mutate(track.name = str_trunc(track.name, 20))) |>
  juice() |>
  column_to_rownames("track.name")

indie_dist <- dist(indie_juice, method = "euclidean")

data_for_indie_clustering <- indie_dist |> 
  hclust(method = "average") |> # average for a balanced tree!
  dendro_data()

playlist_data_for_join <- Arranged_playlists %>%
  select(track.name, playlist_name, playlist) %>%
  mutate(label = str_trunc(track.name, 20))

data_for_indie_clustering$labels <- data_for_indie_clustering$labels %>%
  left_join(playlist_data_for_join)

# Add factor so can use colouring! 
data_for_indie_clustering$labels$label <- factor(data_for_indie_clustering$labels$label)

##plot
Cluster_Colour <- data_for_indie_clustering |>
  ggdendrogram() +
  geom_text(data = label(data_for_indie_clustering), aes(x, y, 
                                                         label=label, 
                                                         hjust=0, 
                                                         colour=playlist), size=3) +
  coord_flip() + 
  scale_y_reverse(expand=c(0.2, 0)) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank()) +
  labs(title = "Playlist Clustering") +
  guides(
    colour = guide_legend(
      title = "Decade"
    )
  )

saveRDS(object = Cluster_Colour,file = "data/Cluster_Colour.RDS")