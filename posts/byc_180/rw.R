
# --- Data pulls ---
unc_ff <- kp_get_team_history(
  team_id = 197, years = 2004:2021, endpoint = "four-factors"
)

unc_misc <- kp_get_team_history(
  team_id = 197, years = 2004:2021, endpoint = "misc-stats"
)

unc_pd <- kp_get_team_history(
  team_id = 197, years = 2004:2026, endpoint = "point-dist")

unc_hist <- kp_get_team_history(team_id = 197, years = 2017:2026) |>
  dplyr::select(Season, TeamName, Seed, Coach, Wins, Losses, AdjEM, RankAdjEM)

unc_final <- unc_hist |>
  dplyr::left_join(unc_ff)

# --- Tournament rounds ---
tournament_rounds <- tibble::tibble(
  Season = factor(2017:2026, levels = 2017:2026),
  Round  = c("Champions", "R32", "S16", "Missed", "R64",
             "Runner-up", "Missed", "S16", "R64", "R64")
)

# --- Config ---
radar_vars <- c(
  "RankeFG_Pct", "RankTO_Pct", "RankOR_Pct", "RankFT_Rate",
  "RankDeFG_Pct", "RankDTO_Pct", "RankDOR_Pct", "RankDFT_Rate"
)

spoke_short <- c(
  RankeFG_Pct  = "eFG%",
  RankTO_Pct   = "TO%",
  RankOR_Pct   = "OR%",
  RankFT_Rate  = "FTR",
  RankDeFG_Pct = "DeFG%",
  RankDTO_Pct  = "DTO%",
  RankDOR_Pct  = "DOR%",
  RankDFT_Rate = "DFTR"
)

field_size <- tibble::tibble(
  Season  = factor(2017:2026, levels = 2017:2026),
  n_teams = c(351, 351, 353, 353, 357, 358, 363, 362, 364, 365)
)

# --- Prep — keep raw ranks before normalizing ---
unc_radar_raw <- unc_final |>
  dplyr::mutate(Season = factor(Season, levels = 2017:2026)) |>
  dplyr::mutate(
    Coach = dplyr::if_else(Coach == "Roy Williams", "Roy Williams | .685 (120-55)",
                           "Hubert Davis | .698 (125-54)")
  ) |>
  dplyr::left_join(field_size, by = "Season") |>
  dplyr::left_join(tournament_rounds, by = "Season") |>
  dplyr::mutate(
    seed_label = dplyr::if_else(Seed == 0, "X", as.character(Seed)),
    facet_label = paste0(
      "<b>", Season, " (", Wins, "-", Losses, ") | ", Round, " | Seed: ", seed_label, "</b><br>",
      "AdjT: #", RankAdjTempo, " (", round(AdjTempo, 1), ") <br>",
      "AdjO: #", RankAdjOE, " (", round(AdjOE, 1), ") <br>",
      "AdjD: #", RankAdjDE, " (", round(AdjDE, 1), ")"
    ),
    Coach = factor(Coach, levels = c("Roy Williams | .685 (120-55)", "Hubert Davis | .698 (125-54)"))
  )

# --- Pull raw ranks into long table for spoke tip labels ---
raw_ranks_long <- unc_radar_raw |>
  dplyr::select(Season, Coach, facet_label, dplyr::all_of(radar_vars)) |>
  tidyr::pivot_longer(
    cols      = dplyr::all_of(radar_vars),
    names_to  = "metric",
    values_to = "raw_rank"
  ) |>
  dplyr::mutate(metric = factor(metric, levels = radar_vars))

# --- Normalize ---
unc_radar <- unc_radar_raw |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(radar_vars),
      ~ (n_teams + 1 - .x) / n_teams
    )
  )

# --- Named labeller for Season strips ---
season_labels <- unc_radar |>
  dplyr::distinct(Season, facet_label) |>
  dplyr::arrange(Season) |>
  tibble::deframe()

# --- Reshape normalized to long ---
radar_long <- unc_radar |>
  dplyr::select(Season, Coach, facet_label, dplyr::all_of(radar_vars)) |>
  tidyr::pivot_longer(
    cols      = dplyr::all_of(radar_vars),
    names_to  = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(metric = factor(metric, levels = radar_vars))

n_metrics <- length(radar_vars)

# --- Radar polygon coordinates ---
radar_coords <- radar_long |>
  dplyr::mutate(
    angle = (as.integer(metric) - 1) * 2 * pi / n_metrics - pi / 2,
    x     = value * cos(angle),
    y     = value * sin(angle)
  )

# --- Close each polygon ---
radar_closed <- radar_coords |>
  dplyr::group_by(Season, Coach, facet_label) |>
  dplyr::group_modify(~ dplyr::bind_rows(.x, dplyr::slice(.x, 1))) |>
  dplyr::ungroup()

# --- Spoke label positions with raw rank ---
spoke_labels <- radar_long |>
  dplyr::distinct(Season, Coach, facet_label, metric) |>
  dplyr::left_join(raw_ranks_long, by = c("Season", "Coach", "facet_label", "metric")) |>
  dplyr::mutate(
    angle = (as.integer(metric) - 1) * 2 * pi / n_metrics - pi / 2,
    x     = 1.28 * cos(angle),
    y     = 1.28 * sin(angle),
    label = paste0(spoke_short[as.character(metric)], "\n#", raw_rank)
  )

# --- Grid rings ---
grid_rings <- tidyr::expand_grid(
  r     = c(0.25, 0.5, 0.75, 1.0),
  theta = seq(0, 2 * pi, length.out = 200)
) |>
  dplyr::mutate(x = r * cos(theta), y = r * sin(theta))

# --- Plot ---
ggplot2::ggplot() +
  # Grid rings
  ggplot2::geom_path(
    data = grid_rings,
    ggplot2::aes(x, y, group = r),
    colour = "grey80", linewidth = 0.4
  ) +
  # Spokes
  ggplot2::geom_segment(
    data = spoke_labels,
    ggplot2::aes(x = 0, y = 0, xend = cos(angle), yend = sin(angle)),
    colour = "grey75", linewidth = 0.3
  ) +
  # Radar polygon
  ggplot2::geom_polygon(
    data = radar_closed,
    ggplot2::aes(x, y, group = Season),
    fill = "#7BAFD4", alpha = 0.35, colour = "#005B99", linewidth = 0.7
  ) +
  # Vertex dots
  ggplot2::geom_point(
    data = radar_coords,
    ggplot2::aes(x, y),
    colour = "#005B99", size = 1.4
  ) +
  # Spoke tip labels
  ggplot2::geom_text(
    data = spoke_labels,
    ggplot2::aes(x, y, label = label),
    size       = 2.2,
    hjust      = "outward",
    vjust      = "outward",
    lineheight = 0.9,
    family     = "Roboto Condensed"
  ) +
  # Facets
  ggh4x::facet_nested_wrap(
    ~ Coach + Season,
    nrow      = 2,
    nest_line = TRUE,
    strip     = ggh4x::strip_nested(
      text_x = list(
        ggplot2::element_text(face = "bold", size = 9, colour = "white", family = "Roboto Condensed"),
        ggtext::element_markdown(size = 7, lineheight = 1.3, colour = "grey20", family = "Roboto Condensed")
      ),
      background_x = list(
        ggplot2::element_rect(fill = "#7BAFD4", colour = "#005B99"),
        ggplot2::element_rect(fill = "grey96", colour = "grey80")
      ),
      by_layer_x = TRUE
    ),
    labeller = ggplot2::labeller(Season = season_labels)
  ) +
  ggplot2::coord_fixed(
    xlim = c(-1.6, 1.6),
    ylim = c(-1.6, 1.6),
    clip = "off"
  ) +
  ggplot2::labs(
    title   = "This is Carolina Basketball . . . over the last 10 seasons<br>",
    subtitle = "Radar chart, each spoke is a national rank for the four factors and <b>further from center is better</b>.<br> Four factors on offense and defense: eFG% (shooting), TO% (turnovers), DR/OR% (rebounds), and FTR (Free Throw Rate).",
    caption = "data via kenpom.com through March 19, 2026 games | Viz by Chris at Bless your chart"
  ) +
  hrbrthemes::theme_ipsum(
    grid        = FALSE,
    axis        = FALSE,
    ticks       = FALSE,
    base_family = "Roboto Condensed",
    plot_margin = ggplot2::margin(10, 10, 10, 10)
  ) +
  ggplot2::theme(
    plot.title    = ggtext::element_markdown(face = "bold", size = 14, family = "Roboto Condensed"),
    plot.subtitle = ggtext::element_markdown(size = 10, colour = "grey40", family = "Roboto Condensed"),
    plot.caption  = ggplot2::element_text(size = 8, colour = "grey50", family = "Roboto Condensed"),
    axis.text.x   = ggplot2::element_blank(),
    axis.text.y   = ggplot2::element_blank(),
    axis.title.x  = ggplot2::element_blank(),
    axis.title.y  = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(1.2, "lines")
  )
