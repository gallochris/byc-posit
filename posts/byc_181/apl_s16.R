# ── 1. Calculate National Stats & Percentiles ────────────────────────────────
national_stats <- apl |>
  dplyr::mutate(
    # Reversing: Smallest APL (Fastest) now = 1.0 (99th percentile)
    pct_off = 1 - dplyr::percent_rank(APL_Off),
    pct_def = 1 - dplyr::percent_rank(APL_Def)
  )

# Calculate DI Average
di_avg_total <- mean(c(national_stats$APL_Off, national_stats$APL_Def), na.rm = TRUE)

# ── 2. Define team order ──────────────────────────────────────────────────────
region_order <- tibble::tibble(
  TeamName = c("Connecticut", "Illinois", "Arizona", "Michigan"),
  sort_key = c(4, 3, 2, 1)
)


# ── 3. Build plot_data with Formatted Strings ─────────────────────────────────
plot_data <- national_stats |>
  dplyr::filter(TeamName %in% region_order$TeamName) |>
  dplyr::mutate(
    # Force one decimal place even for .0 values
    APL_Off_str = sprintf("%.1f", APL_Off),
    APL_Def_str = sprintf("%.1f", APL_Def)
  ) |>
  dplyr::left_join(region_order, by = "TeamName") |>
  dplyr::mutate(team_chr = TeamName)

# ── 4. Build spacer row ───────────────────────────────────────────────────────
spacers <- tibble::tibble(
  TeamName = "_spacer_",
  sort_key = 2.5,
  # sits between Illinois (3) and Arizona (2)
  APL_Off  = NA_real_,
  APL_Def  = NA_real_,
  team_chr = NA_character_
)

# ── 5. Combine and build labels (Raw Values under Logos) ──────────────────────
combined <- dplyr::bind_rows(plot_data, spacers) |>
  dplyr::mutate(
    axis_label = dplyr::case_when(
      stringr::str_starts(TeamName, "_spacer_") ~ "",
      TRUE ~ sprintf(
        "<b>%s</b><br><span style='font-size:8.5pt; color:grey40'>%s / %ss</span>",
        TeamName,
        APL_Off_str,
        APL_Def_str
      )
    ),
    TeamName = forcats::fct_reorder(TeamName, sort_key),
    team_chr = as.character(TeamName)
  )

# ── 6. Annotations & Helpers ──────────────────────────────────────────────────
region_labels <- tibble::tibble(
  TeamName = factor(
    c("_spacer_E", "_spacer_S", "_spacer_W", "_spacer_M"),
    levels = levels(combined$TeamName)
  ),
  label = c("EAST", "SOUTH", "WEST", "MIDWEST"),
  x = 14.1
)

region_dividers <- tibble::tibble(TeamName = factor(
  c("_spacer_E", "_spacer_S", "_spacer_W", "_spacer_M"),
  levels = levels(combined$TeamName)
))

logo_data <- combined |> dplyr::filter(!stringr::str_starts(TeamName, "_spacer_"))

# ── 7. Plot ───────────────────────────────────────────────────────────────────
time_plot <- combined |>
  ggplot2::ggplot(ggplot2::aes(y = TeamName)) +
  # Vertical line for DI Average
  ggplot2::geom_vline(
    xintercept = di_avg_total,
    linetype = "dashed",
    color = "grey70",
    linewidth = 0.6
  ) +
  # Vertical Annotation in center
  ggplot2::annotate(
    "text",
    x = di_avg_total - 0.15,
    y = 5.5,
    label = "D-I AVG",
    family = "Roboto Condensed",
    size = 3.2,
    color = "grey50",
    fontface = "bold",
    angle = 90
  ) +
  # Dumbbell segments
  ggplot2::geom_segment(
    ggplot2::aes(
      x = APL_Def,
      xend = APL_Off,
      yend = TeamName,
      color = team_chr
    ),
    linewidth = 1.2,
    alpha = 0.8,
    na.rm = TRUE
  ) +
  ggplot2::geom_point(ggplot2::aes(x = APL_Def, color = team_chr),
                      size = 3.5,
                      na.rm = TRUE) +
  ggplot2::geom_point(ggplot2::aes(x = APL_Off, color = team_chr),
                      size = 3.5,
                      na.rm = TRUE) +
  # O/D Labels
  ggplot2::geom_text(
    ggplot2::aes(x = APL_Off, label = "O", color = team_chr),
    vjust = -1.1,
    size = 4.2,
    fontface = "bold",
    family = "Roboto Condensed",
    na.rm = TRUE
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = APL_Def, label = "D", color = team_chr),
    vjust = 1.8,
    size = 4.2,
    fontface = "bold",
    family = "Roboto Condensed",
    na.rm = TRUE
  ) +  # Logos
  cbbplotR::geom_cbb_teams(data = logo_data,
                           ggplot2::aes(x = 13.9, y = TeamName, team = team_chr),
                           width = 0.0475) +
  # Dividers and Region Names
  cbbplotR::scale_color_cbb_teams() +
  ggplot2::scale_y_discrete(
    labels = setNames(combined$axis_label, as.character(combined$TeamName)),
    expand = ggplot2::expansion(add = c(1.0, 1.2))
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(14, 21, 1),
    labels = function(x)
      sprintf("%.1f", x),
    # Formats X-axis ticks as 16.0, 17.0, etc.
    limits = c(13.8, 21)
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::annotation_custom(
    grid::textGrob(
      "APL (Seconds)\nOffense / Defense",
      x = grid::unit(-0.06, "npc"),
      y = grid::unit(1.02, "npc"),
      hjust = 0.5,
      vjust = 0,
      gp = grid::gpar(
        fontsize = 8,
        fontface = "bold",
        col = "#333333",
        fontfamily = "Roboto Condensed",
        lineheight = 0.9
      )
    )
  ) +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(
    title = "How Final Four teams <br>try to <em>control</em> the clock",
    subtitle = "Average possession length in seconds on offense and defense<br>",
    x = "Seconds",
    y = "",
    caption = "Data via kenpom.com <br>Viz by Chris at Bless your chart"
  ) +
  ggplot2::theme(
    plot.title = ggtext::element_markdown(
      hjust = 0.5,
      size = 20,
      family = "Roboto Condensed"
    ),
    plot.subtitle = ggtext::element_markdown(
      hjust = 0.5,
      size = 12.5,
      lineheight = 1.3,
      family = "Roboto Condensed"
    ),
    plot.caption = ggtext::element_markdown(
      lineheight = 1.3,
      family = "Roboto Condensed",
      face = "plain"
    ),
    legend.position = "none",
    plot.margin = ggplot2::margin(10, 30, 10, 40),
    axis.text.x = ggplot2::element_text(
      size = 11,
      family = "Roboto Condensed",
      face = "bold"
    ),
    axis.text.y = ggtext::element_markdown(
      size = 9.5,
      family = "Roboto Condensed",
      lineheight = 1.1
    )
  )

ggplot2::ggsave(
  "final_four_tempo.png",
  time_plot,
  height = 11,
  width = 8,
  units = "in",
  bg = "white"
)
