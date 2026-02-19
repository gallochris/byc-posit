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

# Create a base theme that we'll reuse for each plot
my_theme <- function() {
  hrbrthemes::theme_ipsum(
    base_family = "Roboto Condensed",
    grid = "Y",
    plot_title_size = 16,
    subtitle_size = 12,
    axis_title_size = 14,
    base_size = 12
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.title = ggplot2::element_text(),
      plot.caption = ggplot2::element_text(size = 8),
      legend.position = "none",
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = "white")
    )
}

# Score Plot
score_plot <- ggplot2::ggplot(score_data, ggplot2::aes(x = secs_elapsed / 60, y = score, color = team)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::labs(
    x = "Minutes",
    y = "",
    color = "",
    title = "Score"
  ) +
  my_theme() +
  ggplot2::scale_color_manual(
    values = c(Houston = home_col, Florida = away_col)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(score_data$secs_elapsed, na.rm = TRUE) / 60, 5)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 70, 10), limits = c(0, 70)
  ) +
  ggplot2::geom_vline(xintercept = c(10, 20, 30),
                      linetype = "dashed",
                      color = "#acacac") +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y = 65,
    team = "Florida",
    height = .055
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y = 61,
    team = "Houston",
    height = .055
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 65,
    color = away_col,
    label = "65",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed'
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 61,
    color = home_col,
    label = "63",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed'
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 24.9,
    y = 27.6,
    color = away_col,
    label = "30",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed'
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 24.9,
    y = 46,
    color = home_col,
    label = "42",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed'
  ) +
  ggplot2::annotate(
    geom = "label",
    x = 37,
    y = 13.5,
    color = "#333333",
    fill = away_col,
    label = "Florida outscored  \nHouston 23-15  \nover last 10 minutes",
    size = 3,
    fontface = 'bold',
    family = 'Roboto condensed',
    alpha = 0.3
  ) 

# Field Goal Attempts Plot
fga_plot <- ggplot2::ggplot(score_data, ggplot2::aes(x = secs_elapsed / 60, y = score, color = team)) +
  ggplot2::geom_line(size = .75, alpha = 0.3) +
  ggplot2::labs(
    x = "Minutes",
    y = "",
    color = "",
    title = "Field goal attempts"
  ) +
  my_theme() +
  ggplot2::scale_color_manual(
    values = c(Houston = home_col, Florida = away_col)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(score_data$secs_elapsed, na.rm = TRUE) / 60, 5)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 70, 10),  limits = c(0, 70)
  ) +
  ggplot2::geom_vline(xintercept = c(10, 20, 30),
                      linetype = "dashed",
                      color = "#acacac") +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y = 65,
    team = "Florida",
    height = .055
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y = 61,
    team = "Houston",
    height = .055
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 65,
    color = away_col,
    label = "65",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 61,
    color = home_col,
    label = "63",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed',
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
    family = 'Roboto Condensed'
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 39.9,
    y = 69,
    color = home_col,
    label = "69",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed'
  ) +
  ggplot2::geom_step(
    data = fga_data,
    ggplot2::aes(x = secs_elapsed / 60, y = fga_count, color = team),
    size = 1
  ) +
  ggplot2::annotate(
    geom = "label",
    x = 5,
    y = 40,
    color = "#333333",
    fill = home_col,
    label = "Houston did not attempt  \na shot on its final  \n3 possessions",
    size = 3,
    fontface = 'bold',
    family = 'Roboto condensed',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "curve",
    color = home_col,
    x = 5.5,
    y = 50,
    xend = 35,
    yend = 68,
    curvature = -.2,
    linewidth = 0.8,
    arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm")),
    alpha = 0.3
  )

# Fouls Plot
foul_plot <- ggplot2::ggplot(score_data, ggplot2::aes(x = secs_elapsed / 60, y = score, color = team)) +
  ggplot2::geom_line(size = .75, alpha = 0.3) +
  ggplot2::labs(
    x = "Minutes",
    y = "",
    color = "",
    title = "Fouls"
  ) +
  my_theme() +
  ggplot2::scale_color_manual(
    values = c(Houston = home_col, Florida = away_col)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(score_data$secs_elapsed, na.rm = TRUE) / 60, 5)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 70, 10),  limits = c(0, 70)
  ) +
  ggplot2::geom_vline(xintercept = c(10, 20, 30),
                      linetype = "dashed",
                      color = "#acacac") +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y = 65,
    team = "Florida",
    height = .055
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y = 61,
    team = "Houston",
    height = .055
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 65,
    color = away_col,
    label = "65",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 61,
    color = home_col,
    label = "63",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 39.5,
    y = 11,
    color = away_col,
    label = "11",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed'
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 40.9,
    y = 19,
    color = home_col,
    label = "19",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed'
  ) +
  ggplot2::geom_step(
    data = foul_data,
    ggplot2::aes(x = secs_elapsed / 60, y = foul_count, color = team),
    size = 1
  ) +
  ggplot2::annotate(
    geom = "label",
    x = 5,
    y = 50,
    color = "#333333",
    fill = away_col,
    label = "1st half  \nHouston 3, Florida 1  \n2nd half  \nHouston 16, Florida 8",
    size = 3,
    fontface = 'bold',
    family = 'Roboto condensed',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "curve",
    color = away_col,
    x = 11,
    y = 50,
    xend = 25,
    yend = 11.5,
    curvature = -.4,
    linewidth = 0.8,
    arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm")),
    alpha = 0.3
  )

# Turnovers Plot
to_plot <- ggplot2::ggplot(score_data, ggplot2::aes(x = secs_elapsed / 60, y = score, color = team)) +
  ggplot2::geom_line(size = .75, alpha = 0.3) +
  ggplot2::labs(
    x = "Minutes",
    y = "",
    color = "",
    title = "Turnovers"
  ) +
  my_theme() +
  ggplot2::scale_color_manual(
    values = c(Houston = home_col, Florida = away_col)
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, max(score_data$secs_elapsed, na.rm = TRUE) / 60, 5)
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 70, 10),  limits = c(0, 70)
  ) +
  ggplot2::geom_vline(xintercept = c(10, 20, 30),
                      linetype = "dashed",
                      color = "#acacac") +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y = 65,
    team = "Florida",
    height = .055
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 40.9,
    y = 61,
    team = "Houston",
    height = .055
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 65,
    color = away_col,
    label = "65",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed',
    alpha = 0.3
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 42.5,
    y = 61,
    color = home_col,
    label = "63",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed',
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
    family = 'Roboto Condensed'
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 40.9,
    y = 9.5,
    color = home_col,
    label = "9",
    size = 4,
    fontface = 'bold',
    family = 'Roboto Condensed'
  ) +
  ggplot2::geom_step(
    data = to_data,
    ggplot2::aes(x = secs_elapsed / 60, y = foul_count, color = team),
    size = 1
  ) +
  ggplot2::annotate(
    geom = "label",
    x = 37,
    y = 30,
    color = "#333333",
    fill = home_col,
    label = "Houston  \n3 turnovers on \nfinal 3 possessions",
    size = 3,
    fontface = 'bold',
    family = 'Roboto condensed',
    alpha = 0.3
  ) 

# Create the grid layout with cowplot
plot_grid_main <- cowplot::plot_grid(
  score_plot, fga_plot,
  foul_plot, to_plot,
  ncol = 2, align = "hv"
)

# Add a title using the same font family as hrbrthemes::theme_ipsum
title_text <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Florida 65, Houston 63  \n2025 National Championship | April 7, 2025", 
    fontface = 'bold', 
    size = 16, 
    hjust = 0.5,
    fontfamily = "Roboto Condensed"
  )

# Add the title above the grid
plot_with_title <- cowplot::plot_grid(
  title_text,
  plot_grid_main,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# Create footer with the same font family
footer_text <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "data via bigballR, stats.ncaa.org | Viz by Chris at Bless your Chart", 
    size = 10, 
    hjust = 0.5, 
    fontfamily = "Roboto Condensed",
  )

# Add the footer below everything
final_plot <- cowplot::plot_grid(
  plot_with_title,
  footer_text,
  ncol = 1,
  rel_heights = c(1, 0.05)  # Footer is 5% height
)

# To display the plot
final_plot

# If you need to save it:
ggsave("florida_houston_championship.png", final_plot, width = 12, height = 9.5, dpi = 600, bg = "white")
