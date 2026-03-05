kp_get_apl_history <- function(years = 2022:2026,
                               delay = 0.5,
                               token = NULL) {
  years |>
    purrr::map(\(yr) {
      message(sprintf("Fetching ratings for %d...", yr))
      
      result <- tryCatch(
        .kp_api_call("ratings", params = list(y = yr), token = token),
        error = function(e) {
          warning(sprintf("Failed to fetch %d: %s", yr, e$message))
          NULL
        }
      )
      
      if (!is.null(result)) Sys.sleep(delay)
      result
    }) |>
    purrr::compact() |>
    dplyr::bind_rows()
}


apl_data <- kp_get_apl_history() 

overtime <- kp_get_ratings_archive_2026()

### 
kp_get_ratings_archive_2026 <- function(delay = 0.5,
                                        token = NULL) {
  dates <- seq.Date(
    from = as.Date("2025-11-03"),
    to   = Sys.Date(),
    by   = "day"
  ) |>
    format("%Y-%m-%d")
  
  dates |>
    purrr::map(\(d) {
      message(sprintf("Fetching archive for %s...", d))
      
      result <- tryCatch(
        .kp_api_call("archive", params = list(d = d), token = token),
        error = function(e) {
          warning(sprintf("Failed to fetch %s: %s", d, e$message))
          NULL
        }
      )
      
      if (!is.null(result)) {
        result <- result |> dplyr::mutate(date = d)
        Sys.sleep(delay)
      }
      
      result
    }) |>
    purrr::compact() |>
    dplyr::bind_rows()
}
overtime <- kp_get_ratings_archive_2026()

kp_get_ratings_archive_2026 <- function(delay = 0.5,
token = NULL) {
dates <- seq.Date(
from = as.Date("2025-11-03"),
to   = Sys.Date(),
by   = "day"
) |>
format("%Y-%m-%d")
dates |>
purrr::map(\(d) {
message(sprintf("Fetching archive for %s...", d))
result <- tryCatch(
.kp_api_call("archive", params = list(d = d), token = token),
error = function(e) {
warning(sprintf("Failed to fetch %s: %s", d, e$message))
NULL
}
)
if (!is.null(result)) {
result <- result |> dplyr::mutate(date = d)
Sys.sleep(delay)
}
result
}) |>
purrr::compact() |>
dplyr::bind_rows()
}


overtime |>
  dplyr::filter(TeamName == "Duke") |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = AdjTempo)) +
  ggplot2::geom_line()

#### 


highlight_teams <- c("Duke", "Michigan", "Florida", "Arizona",
                     "Illinois", "Houston", "Purdue", "Michigan St.",
                     "Iowa St.", "Connecticut")

apl_long <- apl_data |>
  dplyr::filter(Season == 2026) |>
  tidyr::pivot_longer(
    cols = c(APL_Off, APL_Def),
    names_to = "metric",
    values_to = "value"
  )

apl_long |>
  ggplot2::ggplot(ggplot2::aes(x = metric, y = value)) +
  ggbeeswarm::geom_beeswarm(alpha = 0.15, size = 1.5, color = "grey60") +
  ggbeeswarm::geom_beeswarm(
    data = apl_long |> dplyr::filter(TeamName %in% highlight_teams),
    ggplot2::aes(color = TeamName),
    alpha = 0.9, size = 2.5
  ) +
  ggplot2::stat_summary(fun = median, geom = "crossbar", width = 0.4, color = "black", linewidth = 0.4) +
  ggplot2::theme_minimal()


# dbell 

highlight_teams <- c("Duke", "Michigan", "Florida", "Arizona",
                     "Illinois", "Houston", "Purdue", "Michigan St.",
                     "Iowa St.", "Connecticut")

plot_data <- apl_data |>
  dplyr::filter(Season == 2026, TeamName %in% highlight_teams) |>
  dplyr::mutate(
    APL_Off = round(APL_Off, 1),
    APL_Def = round(APL_Def, 1)
  ) |>
  dplyr::arrange(dplyr::desc(AdjEM)) |>
  dplyr::mutate(
    axis_label = sprintf(
      "<b>%s</b><br><span style='font-size:9pt; color:grey40'>%s / %s</span>",
      TeamName, RankAPL_Off, RankAPL_Def
    ),
    TeamName = forcats::fct_reorder(TeamName, AdjEM),
    team_chr = as.character(TeamName)
  )

logo_data <- plot_data

plot_data |>
  ggplot2::ggplot(ggplot2::aes(y = TeamName)) +
  ggplot2::geom_segment(
    ggplot2::aes(x = APL_Def, xend = APL_Off, yend = TeamName, color = team_chr),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(ggplot2::aes(x = APL_Def, color = team_chr), size = 3) +
  ggplot2::geom_point(ggplot2::aes(x = APL_Off, color = team_chr), size = 3) +
  ggplot2::geom_text(
    ggplot2::aes(x = APL_Off, label = "O", color = team_chr),
    hjust = 0.5, vjust = -1.1, size = 4.6, fontface = "bold",
    family = "Roboto Condensed"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = APL_Def, label = "D", color = team_chr),
    hjust = 0.5, vjust = 1.8, size = 4.6, fontface = "bold",
    family = "Roboto Condensed"
  ) +
  cbbplotR::geom_cbb_teams(
    data = logo_data,
    ggplot2::aes(x = 14, y = TeamName, team = team_chr),
    width = 0.05
  ) +
  cbbplotR::scale_color_cbb_teams() +
  ggplot2::scale_y_discrete(
    labels = setNames(plot_data$axis_label, as.character(plot_data$TeamName))
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(15, 20, 1),
    expand = ggplot2::expansion(add = c(0.5, 0.5))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::annotation_custom(
    grid::textGrob(
      "D-I rank\nOffense / Defense",
      x = grid::unit(-0.06, "npc"),  # closer to panel left edge
      y = grid::unit(1.02, "npc"),   # just above top of panel
      hjust = 0.5, vjust = 0,
      gp = grid::gpar(
        fontsize = 8.5,
        fontface = "bold",
        col = "#333333",
        fontfamily = "Roboto Condensed",
        lineheight = 0.9
      )
    )
  ) +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(
    title = "Some teams play <em>fast</em> <br> Others let the <em>opponent</em> play fast",
    subtitle = "Average possession length in seconds on offense and defense<br>",
    x = "Seconds",
    y = "",
    caption = "data via kenpom.com through March 4 games  \nViz by Chris at Bless your chart"
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
      lineheight = 1.5,
      family = "Roboto Condensed"
    ),
    legend.position = "none",
    plot.margin = ggplot2::margin(5, 30, 5, 40),
    axis.text.x = ggplot2::element_text(
      size = 12,
      family = "Roboto Condensed",
      face = "bold"
    ),
    axis.text.y = ggtext::element_markdown(
      size = 10,
      family = "Roboto Condensed",
      hjust = 0.5,
      lineheight = 1.3
    ),
    plot.caption = ggplot2::element_text(
      face = "plain",
      size = 10,
      family = "Roboto Condensed"
    )
  )
