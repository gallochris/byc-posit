df <- duke_unc |>
  dplyr::rename(location = `...4`) |>
  dplyr::mutate(
    result = stringr::str_extract(Result, "^[WL]"),
    date_clean = stringr::str_extract(Date, "^\\d{4}-\\d{2}-\\d{2}"),
    date = lubridate::ymd(date_clean),
    winner = dplyr::if_else(result == "W", "UNC", "Duke")
  ) |>
  dplyr::arrange(date)

# Calculate streaks
streaks <- df |>
  dplyr::mutate(
    streak_change = winner != dplyr::lag(winner, default = dplyr::first(winner)),
    streak_id = cumsum(streak_change)
  ) |>
  dplyr::group_by(streak_id) |>
  dplyr::summarise(
    winner = dplyr::first(winner),
    streak_length = dplyr::n(),
    start_date = min(date),
    end_date = max(date),
    .groups = "drop"
  )

streaks



# Prepare streak data by decade with tiles for each game
tile_data <- df |>
  dplyr::mutate(
    decade = paste0(floor(lubridate::year(date) / 10) * 10, "s")
  ) |>
  dplyr::filter(decade != "1940s") |>
  # Order decades from most recent to oldest (2020s on top)
  dplyr::mutate(
    decade = forcats::fct_reorder(decade, lubridate::year(date), .desc = FALSE)
  ) |>
  dplyr::group_by(decade) |>
  dplyr::arrange(date) |>
  dplyr::mutate(game_num = dplyr::row_number()) |>
  dplyr::ungroup()

ggplot2::ggplot(tile_data, ggplot2::aes(x = game_num, y = decade, fill = winner)) +
  ggplot2::geom_tile(color = "white", linewidth = 0.5, height = 0.6) +
  ggplot2::scale_fill_manual(
    values = c("Duke" = "#012169", "UNC" = "#56a0d3"),
    name = ""
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 30, 10),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title = "Duke vs UNC: Game-by-Game Results",
    subtitle = "Each tile = one game, ordered chronologically left to right",
    x = "Game Number (within decade)",
    y = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "#2d3436", color = NA),
    panel.background = ggplot2::element_rect(fill = "#2d3436", color = NA),
    panel.grid = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(color = "white", size = 11),
    axis.title.x = ggplot2::element_text(color = "white", size = 10),
    plot.title = ggplot2::element_text(color = "white", face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = ggplot2::element_text(color = "#b2bec3", hjust = 0.5, size = 11),
    legend.text = ggplot2::element_text(color = "white"),
    legend.position = "top"
  )


### 
dlogs |> 
  dplyr::group_by(player) |> 
  dplyr::filter(dplyr::n_distinct(year) > 1) |> 
  dplyr::group_by(player, year) |> 
  dplyr::summarize(games_played = dplyr::n(), .groups = "drop")


# Print summary
cat("\n=== Yearly Win Summary (2000-2025) ===\n\n")
print(yearly_wins)

cat("\n\nPlot saved to: /mnt/user-data/outputs/duke_unc_tally_plot.png\n")