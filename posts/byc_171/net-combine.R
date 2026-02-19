# Combine both seasons with normalized day number
combined_data <- dplyr::bind_rows(
  acc_data |> 
    dplyr::mutate(
      season = "25-26",
      day_num = as.numeric(date - min(date))
    ),
  last_acc_data |> 
    dplyr::mutate(
      season = "24-25",
      day_num = as.numeric(date - min(date))
    )
)

# Calculate shared y limits
combined_net_range <- range(combined_data$net)
shared_y_limits <- c(min(365, combined_net_range[2] + 10), max(1, combined_net_range[1] - 10))

# Create facet labels with both seasons
# Create facet labels with both seasons (with HTML for color)
facet_labels <- combined_data |>
  dplyr::group_by(team, season) |>
  dplyr::summarize(
    first_net = dplyr::first(net),
    last_net = dplyr::last(net),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = season,
    values_from = c(first_net, last_net),
    names_sep = "_"
  ) |>
  dplyr::mutate(
    label = paste0(
      team, "<br>",
      "'25-26: ", `first_net_25-26`, " -> ", `last_net_25-26`, "<br>",
      "<span style='color:#999999;'>'24-25: ", `first_net_24-25`, " -> ", `last_net_24-25`, "</span>"
    )
  ) |>
  dplyr::select(team, label) |>
  tibble::deframe()

# Get max day for x-axis
max_day <- max(combined_data$day_num)

combined_data |> 
  ggplot2::ggplot(ggplot2::aes(x = day_num, y = net, color = team, linetype = season)) +
  ggplot2::geom_line(ggplot2::aes(linewidth = season)) +
  ggplot2::facet_wrap(~team, nrow = 3, labeller = ggplot2::as_labeller(facet_labels)) +
  ggplot2::scale_y_reverse(
    limits = shared_y_limits,
    expand = ggplot2::expansion(mult = c(0.05, 0.05))
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(0, max_day / 2, max_day),
    labels = c("Nov 30", "", "Jan 30"),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  ggplot2::scale_linetype_manual(values = c("24-25" = "dotted", "25-26" = "solid")) +
  ggplot2::scale_linewidth_manual(values = c("24-25" = 0.6, "25-26" = 1.2)) +
  cfbplotR::scale_color_cfb() +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(
    title = "What a difference a year makes for the ACC  \nNET ranking over first few months of last two seasons",
    subtitle = "Average NET ranking today: 62.2 | Last season? 98.3",
    x = NULL,
    y = "Rank",
    caption = "data via NCAA | Viz by Chris at Bless your chart  \nSolid lines shows 2025-26 ranking and dotted lines 2024-25"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 20, family = "Roboto Condensed"),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, family = "Roboto Condensed"),
    strip.text = ggtext::element_markdown(
      face = "bold",
      size = 9,
      family = "Roboto Condensed",
      lineheight = 1.2
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(2, "lines"),
    axis.text.x = ggplot2::element_text(size = 9, family = "Roboto Condensed"),
    axis.text.y = ggplot2::element_text(size = 9, family = "Roboto Condensed"),
    plot.caption = ggplot2::element_text(face = "plain", size = 8, family = "Roboto Condensed"),
    legend.position = "none"
  ) -> net_plot
