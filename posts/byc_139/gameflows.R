score_data <- pbp_hou |>
  dplyr::select(Game_Seconds, Home_Score, Away_Score) |>
  dplyr::distinct() |>
  dplyr::rename(secs_elapsed = Game_Seconds) |>
  tidyr::pivot_longer(
    cols = c(Home_Score, Away_Score),
    names_to = "team",
    values_to = "score"
  ) |>
  dplyr::mutate(
    team = dplyr::case_when(
      team == "Home_Score" ~ "Houston",
      team == "Away_Score" ~ "Florida"
    )
  )

# Field Goal Attempt data
fga_data <- pbp_hou |>
  dplyr::filter(Event_Type %in% c("Three Point Jumper", "Two Point Jumper", "Layup", "Hook", "Dunk")) |>
  dplyr::mutate(team = dplyr::case_when(
    Event_Team == home_team ~ home_team,
    Event_Team == away_team ~ away_team
  )) |>
  dplyr::group_by(team) |>
  dplyr::arrange(Game_Seconds, .by_group = TRUE) |>
  dplyr::mutate(fga_count = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::rename(secs_elapsed = Game_Seconds)


# Foul data
foul_data <- pbp_hou |>
  dplyr::filter(Event_Type == "Commits Foul") |>
  dplyr::mutate(team = dplyr::case_when(
    Event_Team == home_team ~ home_team,
    Event_Team == away_team ~ away_team
  )) |>
  dplyr::group_by(team) |>
  dplyr::arrange(Game_Seconds, .by_group = TRUE) |>
  dplyr::mutate(foul_count = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::rename(secs_elapsed = Game_Seconds)

# Turnovers
to_data <- pbp_hou |>
  dplyr::filter(Event_Type == "Turnover") |>
  dplyr::mutate(team = dplyr::case_when(
    Event_Team == home_team ~ home_team,
    Event_Team == away_team ~ away_team
  )) |>
  dplyr::group_by(team) |>
  dplyr::arrange(Game_Seconds, .by_group = TRUE) |>
  dplyr::mutate(foul_count = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::rename(secs_elapsed = Game_Seconds)



home_team <- "Houston"
away_team <- "Florida"
home_col <- "#C8102E"
away_col <- "#0021A5"


score_plot <- ggplot2::ggplot(score_data, ggplot2::aes(x = secs_elapsed / 60, y = score, color = team)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::labs(
    x = "Minutes",
    y = "",
    color = "",
    title = "Score",
  ) +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
    axis.title = ggplot2::element_text(size = 14),
    plot.caption = ggplot2::element_text(size = 8),
    legend.position = "none"
  ) +
  ggplot2::scale_color_manual(
    values = c(Houston = home_col, Florida = away_col)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(score_data$secs_elapsed, na.rm = TRUE) / 60, 5)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 65, 5)
  ) +
  ggplot2::geom_vline(xintercept = c(10, 20, 30),
                      linetype = "dashed",
                      color = "#333333") +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y =65,
    team = "Florida",
    height = .055
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x =40.9,
    y =61,
    team = "Houston",
    height = .055,
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 65,
    color = away_col,
    label = "65",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 61,
    color = home_col,
    label = "63",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 24.9,
    y = 28,
    color = away_col,
    label = "30",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 24.9,
    y = 45.5,
    color = home_col,
    label = "42",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) 

#--------------------------FGA
fga_plot <- ggplot2::ggplot(score_data, ggplot2::aes(x = secs_elapsed / 60, y = score, color = team)) +
  ggplot2::geom_line(size = .75, alpha = 0.3) +
  ggplot2::labs(
    x = "Minutes",
    y = "",
    color = "",
    title = "Field goal attempts (shots)",
  ) +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
    axis.title = ggplot2::element_text(size = 14),
    plot.caption = ggplot2::element_text(size = 8),
    legend.position = "none"
  ) +
  ggplot2::scale_color_manual(
    values = c(Houston = home_col, Florida = away_col)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(score_data$secs_elapsed, na.rm = TRUE) / 60, 5)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 70, 5)
  ) +
  ggplot2::geom_vline(xintercept = c(10, 20, 30),
                      linetype = "dashed",
                      color = "#333333") +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y =65,
    team = "Florida",
    height = .055
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x =40.9,
    y =61,
    team = "Houston",
    height = .055,
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 65,
    color = away_col,
    label = "65",
    size = 4,
    fontface = 'bold',
    family = 'mono',
    alpha = 0.3,
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 61,
    color = home_col,
    label = "63",
    size = 4,
    fontface = 'bold',
    family = 'mono',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 40,
    y = 53,
    color = away_col,
    label = "53",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 39.5,
    y = 69,
    color = home_col,
    label = "69",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::geom_step(
    data = fga_data,
    ggplot2::aes(x = secs_elapsed / 60, y = fga_count, color = team),
    size = 1
  ) 

#### fouls 
foul_plot <- ggplot2::ggplot(score_data, ggplot2::aes(x = secs_elapsed / 60, y = score, color = team)) +
  ggplot2::geom_line(size = .75, alpha = 0.3) +
  ggplot2::labs(
    x = "Minutes",
    y = "",
    color = "",
    title = "Fouls",
  ) +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
    axis.title = ggplot2::element_text(size = 14),
    plot.caption = ggplot2::element_text(size = 8),
    legend.position = "none"
  ) +
  ggplot2::scale_color_manual(
    values = c(Houston = home_col, Florida = away_col)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(score_data$secs_elapsed, na.rm = TRUE) / 60, 5)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 65, 5)
  ) +
  ggplot2::geom_vline(xintercept = c(10, 20, 30),
                      linetype = "dashed",
                      color = "#333333") +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y =65,
    team = "Florida",
    height = .055
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x =40.9,
    y =61,
    team = "Houston",
    height = .055,
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 65,
    color = away_col,
    label = "65",
    size = 4,
    fontface = 'bold',
    family = 'mono',
    alpha = 0.3,
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 61,
    color = home_col,
    label = "63",
    size = 4,
    fontface = 'bold',
    family = 'mono',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 38.5,
    y = 11,
    color = away_col,
    label = "11",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 40.5,
    y = 19,
    color = home_col,
    label = "19",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::geom_step(
    data = foul_data,
    ggplot2::aes(x = secs_elapsed / 60, y = foul_count, color = team),
    size = 1
  )

# turnovers
to_plot <- ggplot2::ggplot(score_data, ggplot2::aes(x = secs_elapsed / 60, y = score, color = team)) +
  ggplot2::geom_line(size = .75, alpha = 0.3) +
  ggplot2::labs(
    x = "Minutes",
    y = "",
    color = "",
    title = "Turnovers",
  ) +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
    axis.title = ggplot2::element_text(size = 14),
    plot.caption = ggplot2::element_text(size = 8),
    legend.position = "none"
  ) +
  ggplot2::scale_color_manual(
    values = c(Houston = home_col, Florida = away_col)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(score_data$secs_elapsed, na.rm = TRUE) / 60, 5)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 65, 5)
  ) +
  ggplot2::geom_vline(xintercept = c(10, 20, 30),
                      linetype = "dashed",
                      color = "#333333") +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y =65,
    team = "Florida",
    height = .055
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x =40.9,
    y =61,
    team = "Houston",
    height = .055,
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 65,
    color = away_col,
    label = "65",
    size = 4,
    fontface = 'bold',
    family = 'mono',
    alpha = 0.3,
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 61,
    color = home_col,
    label = "63",
    size = 4,
    fontface = 'bold',
    family = 'mono',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 31.5,
    y = 13,
    color = away_col,
    label = "13",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 40.5,
    y = 9.5,
    color = home_col,
    label = "9",
    size = 4,
    fontface = 'bold',
    family = 'mono',
  ) +
  ggplot2::geom_step(
    data = to_data,
    ggplot2::aes(x = secs_elapsed / 60, y = foul_count, color = team),
    size = 1
  )
  

plot_grid_main <- cowplot::plot_grid(
  score_plot, fga_plot,
  foul_plot, to_plot,
  ncol = 2, align = "hv"
)

# Add a title above the grid
plot_with_title <- cowplot::plot_grid(
  cowplot::ggdraw() + cowplot::draw_label("Florida 65, Houston 63  \n2025 National Championship | April 7, 2025", fontface = 'bold', size = 16, hjust = 0.5),
  plot_grid_main,
  ncol = 1,
  rel_heights = c(0.1, 1)  # Title is 10% height
)

# Add a footer (caption) below the whole thing
final_plot <- cowplot::plot_grid(
  plot_with_title,
  cowplot::ggdraw() + cowplot::draw_label("data via bigballR, stats.ncaa.org | Viz by Chris at Bless your Chart", 
                                          size = 10, hjust = 0.5, fontfamily = "mono"),
  ncol = 1,
  rel_heights = c(1, 0.05)  # Footer is 5% height
)




