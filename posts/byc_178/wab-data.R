wab_data <- readr::read_csv("http://wabwatch.com/data/wab_by_game.csv")

rankings_df <- wab_data |>
  dplyr::group_by(team) |>
  dplyr::summarise(total_wab = sum(wab, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(total_wab)) |>
  dplyr::mutate(wab_rank = dplyr::row_number())

plot_wab_comparison <- function(df, team_a, team_b, rankings_df) {
  
  # Compute W-L record from data
  get_record <- function(team_name) {
    df |>
      dplyr::filter(team == team_name, !is.na(result)) |>
      dplyr::summarise(
        wins   = sum(result == "W"),
        losses = sum(result == "L")
      ) |>
      glue::glue_data("{wins}-{losses}")
  }
  
  get_label <- function(team_name) {
    r      <- rankings_df |> dplyr::filter(team == team_name)
    total  <- sprintf("%+.2f", r$total_wab)
    rank   <- r$wab_rank
    record <- get_record(team_name)
    # Two-line facet label: "Team (W-L)\nWAB: +X.XX | Rank: #N"
    glue::glue("{team_name} ({record})\nWAB: {total}  |  Rank: #{rank}")
  }
  
  label_a <- get_label(team_a)
  label_b <- get_label(team_b)
  
  today_str <- format(Sys.Date(), "%B %d, %Y")
  
  df_filtered <- df |>
    dplyr::filter(wab != 0) |>
    dplyr::filter(team %in% c(team_a, team_b)) |>
    dplyr::mutate(
      team_label = dplyr::case_when(
        team == team_a ~ label_a,
        team == team_b ~ label_b
      ),
      team_label = factor(team_label, levels = c(label_a, label_b))
    ) |>
    dplyr::group_by(team_label) |>
    dplyr::arrange(bub_win_prob, .by_group = TRUE) |>
    dplyr::mutate(
      opp_label = dplyr::case_when(
        location == "A" ~ paste0("at ", opp),
        location == "N" ~ paste0("(N) ", opp),
        TRUE            ~ opp
      ),
      wab_label = sprintf("%+.2f", wab)
    ) |>
    dplyr::ungroup()
  
  ggplot2::ggplot(df_filtered, ggplot2::aes(
    x    = wab,
    y    = tidytext::reorder_within(opp_label, -bub_win_prob, team_label),
    fill = result
  )) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, color = "#888888", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = wab_label,
        hjust = dplyr::if_else(wab >= 0, -0.15, 1.15)
      ),
      size     = 2.8,
      color    = "#333333",
      fontface = "bold"
    ) +
    tidytext::scale_y_reordered() +
    ggplot2::scale_fill_manual(values = c("W" = "#66BB6A", "L" = "#EF9A9A")) +
    ggplot2::facet_wrap(
      ~ team_label,
      scales = "free_y",
      ncol   = 2
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.15, 0.15))
    ) +
    ggplot2::labs(
      title    = glue::glue("WAB Comparison | {today_str}"),
      subtitle = "Games sorted hardest to easiest. Red is a loss and green is a win. Non-DI games excluded.",
      x        = "WAB",
      y        = NULL,
      caption  = glue::glue(
        "WAB computed using Torvik ratings <br>Data via barttorvik.com <br>Viz by Chris at Bless your Chart"
      )
    ) +
    hrbrthemes::theme_ipsum_rc(grid = "XY") +
    ggplot2::theme(
      plot.title      = ggtext::element_markdown(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle   = ggtext::element_markdown(hjust = 0.5, size = 10, margin = ggplot2::margin(b = 15)),
      plot.caption    = ggtext::element_markdown(),
      strip.background = ggplot2::element_rect(fill = "gray92", color = NA),
      
      # --- UPDATED SECTION ---
      strip.text      = ggplot2::element_text(
        size = 13,                # Increased from 9
        face = "bold", 
        hjust = 0.5,              # Centers the text horizontally
        margin = margin(t = 5, b = 5) # Adds a little padding top and bottom
      ),
      # -----------------------
      
      panel.spacing   = ggplot2::unit(1.5, "lines"),
      axis.title.x    = ggplot2::element_text(size = 9, color = "gray30", hjust = 0.5),
      axis.title.y    = ggplot2::element_text(size = 9, color = "gray30", hjust = 0.5),
      legend.position = "none"
    )
}

# Usage:
p <- plot_wab_comparison(wab_data, "Miami OH", "Auburn", rankings_df)
ggplot2::ggsave("wab_miami_auburn.png", p,
                width = 14, height = 10, dpi = 150, bg = "white")