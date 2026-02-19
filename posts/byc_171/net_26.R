# Get conference data with facet ordering by most recent NET rank
acc_data <- net |> 
  dplyr::arrange(date) |> 
  dplyr::mutate(team = dplyr::if_else(team == "Miami (FL)", "Miami", team)) |> 
  dplyr::filter(conf == "ACC") |> 
  dplyr::mutate(team = forcats::fct_reorder(team, net, .fun = dplyr::last))

# Calculate conference-specific y limits with padding
net_range <- range(acc_data$net)
y_limits <- c(min(365, net_range[2] + 10), max(1, net_range[1] - 10))

# Create facet labels with first → last NET
facet_labels <- acc_data |>
  dplyr::group_by(team) |>
  dplyr::summarize(
    first_net = dplyr::first(net),
    last_net = dplyr::last(net),
    .groups = "drop"
  ) |>
  dplyr::mutate(label = paste0(team, "  \n(", first_net, "--> ", last_net, ")")) |>
  dplyr::select(team, label) |>
  tibble::deframe()

# Points for first/last dates (no text labels now)
labels <- acc_data |>
  dplyr::group_by(team) |>
  dplyr::filter(date == min(date) | date == max(date)) |>
  dplyr::ungroup()

acc_data |> 
  ggplot2::ggplot(ggplot2::aes(x = date, y = net, color = team)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::geom_point(data = labels,
                      size = 2.5) +
  ggplot2::facet_wrap(~team, nrow = 3, labeller = ggplot2::as_labeller(facet_labels)) +
  ggplot2::scale_y_reverse(
    limits = y_limits,
    expand = ggplot2::expansion(mult = c(0.05, 0.05))
  ) +
  ggplot2::scale_x_date(
    breaks = date_breaks,
    labels = c("Nov 30", "", "Jan 30"),
    expand = ggplot2::expansion(mult = c(0.02, 0.02))
  ) +
  cfbplotR::scale_color_cfb() +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(
    title = "2025-26 ACC teams NET rankings so far",
    subtitle = "Shows how each ACC's NET ranking has changed since November 30.",
    x = NULL,
    y = "Rank",
    caption = "data via NCAA  \nData through Jan.29 games | Viz by Chris at Bless your chart"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      size = 20,
      family = "Roboto Condensed"
    ),
    plot.subtitle = ggtext::element_markdown(
      hjust = 0.5,
      size = 9.5,
      lineheight = 1.5,
      family = "Roboto Condensed"
    ),
    strip.text = ggplot2::element_text(
      face = "bold",
      size = 11,
      family = "Roboto Condensed"
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(2.5, "lines"),
    axis.text.x = ggplot2::element_text(size = 11, family = "Roboto Condensed"),
    axis.text.y = ggplot2::element_text(size = 11, family = "Roboto Condensed"),
    plot.caption = ggplot2::element_text(
      face = "plain",
      size = 8,
      family = "Roboto Condensed"
    ),
    legend.position = "none"
  ) 
