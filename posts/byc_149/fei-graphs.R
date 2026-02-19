df_zones <- fball |> 
  dplyr::mutate(
    zone = dplyr::case_when(
      start >= 50 ~ "Own Territory (50-99)",
      start >= 1 ~ "Opponent Territory (1-49)"
    )
  ) |> 
  tidyr::pivot_longer(
    cols = c(TD, FGA, Punt, Down, Fum, Int, Saf, Half),
    names_to = "outcome",
    values_to = "percentage"
  ) |> 
  dplyr::mutate(
    outcome = dplyr::case_when(
      outcome == "TD" ~ "Touchdown",
      outcome == "FGA" ~ "Field Goal Attempt",
      outcome == "Punt" ~ "Punt", 
      outcome == "Down" ~ "Turnover on Downs",
      outcome == "Fum" ~ "Fumble",
      outcome == "Int" ~ "Interception",
      outcome == "Saf" ~ "Safety",
      outcome == "Half" ~ "End of Half",
      outcome == "Half" ~ "End of Half"
    ),
    outcome = factor(outcome, levels = c("Touchdown", "Field Goal Attempt", "Punt", 
                                         "Turnover on Downs", "Fumble", "Interception", 
                                         "Safety", "End of Half"))
  )

ggplot2::ggplot(df_zones, ggplot2::aes(x = start, y = percentage, color = outcome)) +
  ggplot2::geom_line(size = 0.8, alpha = 0.8) +
  ggplot2::geom_point(size = 1.2, alpha = 0.9) +
  ggplot2::facet_wrap(~outcome, scales = "fixed", ncol = 4, nrow = 2) +
  ggplot2::scale_x_reverse(
    name = "Starting Field Position (Yards from End Zone)",
    breaks = seq(10, 90, 20)
  ) +
  ggplot2::scale_y_continuous(
    name = "Percentage of Drives",
    labels = scales::percent_format(accuracy = 1)
  ) +
  ggplot2::scale_color_brewer(type = "qual", palette = "Set2") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    strip.text = ggplot2::element_text(face = "bold", size = 10),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = 9),
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    plot.subtitle = ggplot2::element_text(size = 11, color = "gray40")
  ) +
  ggplot2::labs(
    title = "College Football Drive Outcomes by Starting Field Position",
    subtitle = "How drive outcomes vary across the field (99 = own 1-yard line, 1 = opponent 1-yard line)",
    caption = "Data shows percentage of drives ending in each outcome type"
  )

### 

p1 <- ggplot2::ggplot(df_zones, ggplot2::aes(x = start, y = percentage, fill = outcome)) +
  ggplot2::geom_area(alpha = 0.8, color = "white", linewidth = 0.5) +
  ggplot2::scale_x_reverse(
    name = "Starting Field Position (Yards from End Zone)",
    breaks = seq(10, 90, 20),
    labels = function(x) paste0(x, " yds")
  ) +
  ggplot2::scale_y_continuous(
    name = "Percentage of Drives",
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  ggplot2::scale_fill_brewer(
    type = "qual", 
    palette = "Set3",
    name = "Drive Outcome"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = ggplot2::element_text(size = 9),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, color = "gray40", hjust = 0.5),
    plot.caption = ggplot2::element_text(size = 9, color = "gray50", hjust = 1)
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
  ggplot2::labs(
    title = "College Football Drive Outcomes by Starting Field Position",
    subtitle = "Stacked area showing outcome composition across the field",
    caption = "Note: 99 = own 1-yard line, 1 = opponent 1-yard line"
  )

