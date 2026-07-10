# =============================================================================
# Men's College Basketball Foul & Bonus Analysis
# =============================================================================
#
# Produces two charts
#   1. tbl_bonus_pct  — GT table: percentage of games where at least one team reaches
#                        single / double bonus in each half, by season
#   2. plot_game_tile — Tile chart: bonus state across 40 minutes for
#                        Vanderbilt vs Alabama, January 7, 2026 (88 combined FTs)
#
# Bonus rules (NCAA men's, two-half format):
#   0–6   team fouls  →  no free throws  (the principal)
#   7–9   team fouls  →  one-and-one     (the interest)
#   10+   team fouls  →  automatic 2 FTs (compound interest)
#   Fouls reset at halftime; OT fouls carry over from H2.
#
# =============================================================================
# SETUP — install these packages before running if needed:
#
#   install.packages(c(
#     "arrow", "dplyr", "tidyr", "stringr", "purrr",
#     "fs", "tibble", "ggplot2", "ggthemes", "scales",
#     "gt", "gtUtils", "glue", "piggyback"
#   ))
#
# =============================================================================

# ---- Configuration ----------------------------------------------------------

# Seasons to load — 2023:2026 gives four seasons (2022-23 through 2025-26).
# Foul type classification is reliable from 2022 onward.
seasons <- 2023:2026

# Columns we need from each play-by-play parquet file.
pbp_cols <- c(
  "ID",
  "Date",
  "Half_Status",
  "Game_Seconds",
  "Time",
  "Home",
  "Away",
  "Event_Team",
  "Event_Type",
  "Event_Description"
)

# ---- Data source ------------------------------------------------------------
#
# Data files (pbp_2023 … pbp_2026) are hosted as GitHub release assets.
# On first run, piggyback downloads each file into cache_dir and reuses it
# on every subsequent run — no re-downloading needed.
#
# To switch to local files:  set data_dir to your folder, set gh_repo to NULL.
#
data_dir <- "~/cbb_cache" # folder containing pbp_YYYY files
gh_repo <- NULL # set to "cobrastats/cbb_pbp" to download remotely
gh_tag <- "pbp-data" # release tag (only used when gh_repo != NULL)
cache_dir <- "~/cbb_cache" # download destination (only used remotely)

# --- To pull files from GitHub instead, uncomment and run this once: ---------
# gh_repo <- "cobrastats/cbb_pbp"
# data_dir <- NULL
# purrr::walk(seasons, \(s)
#   piggyback::pb_download(
#     file = stringr::str_glue("pbp_{s}"),
#     repo = gh_repo, tag = gh_tag, dest = cache_dir
#   )
# )
# -----------------------------------------------------------------------------

# ---- Helpers ----------------------------------------------------------------

#' Convert "MM:SS" clock string to seconds remaining in the period.
parse_clock <- function(x) {
  parts <- stringr::str_split_fixed(x, ":", n = 2)
  mm <- suppressWarnings(as.integer(parts[, 1]))
  ss <- suppressWarnings(as.integer(parts[, 2]))
  mm * 60L + ss
}

#' Format a season year as "YYYY-YY" label (e.g. 2026 → "2025-26").
season_lbl <- function(s) {
  stringr::str_glue("{s - 1L}-{stringr::str_pad(s %% 100L, 2, pad = '0')}")
}

#' Classify a foul description into a coarse type.
#' Order matters: "shooting" is checked before "personal" to catch
#' "foul personal shooting" entries correctly.
classify_foul <- function(event_type, event_description) {
  desc <- stringr::str_to_lower(event_description)
  is_foul <- event_type == "Commits Foul"

  dplyr::case_when(
    !is_foul ~ NA_character_,
    stringr::str_detect(desc, "admintechnical") ~ "administrative",
    stringr::str_detect(desc, "flagrant") ~ "flagrant",
    stringr::str_detect(desc, "technical") ~ "technical",
    stringr::str_detect(desc, "offensive") ~ "offensive",
    stringr::str_detect(desc, "shooting") ~ "shooting",
    stringr::str_detect(desc, "personal") ~ "personal",
    TRUE ~ "other"
    # "other" = older seasons (pre-2022) where the description omits the type
    # keyword. These are genuine team fouls and are counted as such below.
  )
}

#' Resolve a season file — checks local dir first, then downloads via piggyback.
.resolve_pbp_path <- function(
  season,
  dir = data_dir,
  repo = gh_repo,
  tag = gh_tag,
  cache = cache_dir
) {
  filename <- stringr::str_glue("pbp_{season}")

  if (!is.null(dir)) {
    local_path <- fs::path(dir, filename)
    if (fs::file_exists(local_path)) return(local_path)
  }

  if (!is.null(repo)) {
    if (!requireNamespace("piggyback", quietly = TRUE)) {
      stop("Install piggyback:  install.packages('piggyback')", call. = FALSE)
    }

    cached_path <- fs::path(cache, filename)
    if (!fs::file_exists(cached_path)) {
      message(
        "  Downloading ",
        filename,
        " from ",
        repo,
        " (tag: ",
        tag,
        ")..."
      )
      fs::dir_create(cache)
      piggyback::pb_download(
        file = filename,
        repo = repo,
        tag = tag,
        dest = cache
      )
    }
    if (fs::file_exists(cached_path)) return(cached_path)
  }

  NA_character_
}

#' Load one season's parquet, selecting only needed columns.
load_pbp_season <- function(
  season,
  dir = data_dir,
  cols = pbp_cols,
  repo = gh_repo,
  tag = gh_tag,
  cache = cache_dir
) {
  path <- .resolve_pbp_path(
    season,
    dir = dir,
    repo = repo,
    tag = tag,
    cache = cache
  )
  if (is.na(path)) {
    warning(
      "Could not locate pbp_",
      season,
      ". Set data_dir (local) or gh_repo (remote).",
      call. = FALSE
    )
    return(tibble::tibble())
  }
  arrow::read_parquet(path, col_select = dplyr::all_of(cols)) |>
    dplyr::mutate(season = season, .before = 1)
}

# ---- Foul extraction --------------------------------------------------------

#' Filter PBP to fouls, classify them, and compute running team-foul counts.
#' OT fouls carry into H2 (no reset until halftime).
extract_fouls <- function(pbp) {
  pbp |>
    dplyr::mutate(
      foul_category = classify_foul(Event_Type, Event_Description),
      bonus_half = dplyr::if_else(Half_Status == 1L, 1L, 2L),
      clock_secs = parse_clock(Time),
      seconds_into_half = dplyr::case_when(
        Half_Status == 1L ~ 1200L - clock_secs,
        Half_Status == 2L ~ 1200L - clock_secs,
        Half_Status >= 3L ~
          1200L + (Half_Status - 3L) * 300L + (300L - clock_secs)
      )
    ) |>
    dplyr::filter(!is.na(foul_category)) |>
    dplyr::mutate(
      # Administrative technicals (bench/coach procedural fouls) do not count
      # toward the team bonus total. Everything else does — including "other",
      # which is how pre-2022 seasons appear in the source data.
      counts_toward_team_foul = foul_category != "administrative"
    ) |>
    dplyr::arrange(season, ID, Half_Status, dplyr::desc(clock_secs)) |>
    dplyr::mutate(
      team_fouls_after = cumsum(counts_toward_team_foul),
      .by = c(season, ID, Event_Team, bonus_half)
    ) |>
    dplyr::mutate(
      bonus_state = dplyr::case_when(
        team_fouls_after < 7L ~ "none",
        team_fouls_after < 10L ~ "single",
        TRUE ~ "double"
      )
    )
}

#' For each game-team-half, find the minute the team entered single / double bonus.
compute_bonus_entry <- function(fouls) {
  fouls |>
    dplyr::filter(counts_toward_team_foul) |>
    dplyr::summarise(
      single_bonus_secs = dplyr::first(seconds_into_half[
        team_fouls_after == 7L
      ]),
      double_bonus_secs = dplyr::first(seconds_into_half[
        team_fouls_after == 10L
      ]),
      .by = c(season, ID, Home, Away, Event_Team, bonus_half)
    ) |>
    dplyr::rename(fouling_team = Event_Team) |>
    dplyr::mutate(
      bonus_team = dplyr::if_else(fouling_team == Home, Away, Home)
    ) |>
    dplyr::select(
      season,
      ID,
      bonus_half,
      bonus_team,
      fouling_team,
      single_bonus_secs,
      double_bonus_secs
    )
}

# ---- Run the pipeline -------------------------------------------------------

message("Loading seasons: ", stringr::str_c(seasons, collapse = ", "))

all_fouls <- seasons |>
  purrr::map(\(s) {
    message("  -> ", s)
    load_pbp_season(s) |> extract_fouls()
  }) |>
  purrr::list_rbind()

bonus_entries <- compute_bonus_entry(all_fouls)

message(
  "Done. Foul events: ",
  nrow(all_fouls),
  " | Bonus entries: ",
  nrow(bonus_entries)
)

# ---- Chart 1: GT table — bonus frequency by season/half --------------------
#
# Shows percentage of *games* (not team-halves) where at least one team reached each
# bonus threshold during each half, for seasons 2022-23 through 2025-26.

# gtUtils has a missing import — patch it before calling gt_column_subheaders
`%||%` <- rlang::`%||%`
library(gtUtils) # also loads gt

bonus_pct_data <- bonus_entries |>
  dplyr::summarise(
    teams_single = sum(!is.na(single_bonus_secs)),
    teams_double = sum(!is.na(double_bonus_secs)),
    .by = c(season, ID, bonus_half)
  ) |>
  dplyr::summarise(
    pct_any_single = mean(teams_single >= 1),
    pct_any_double = mean(teams_double >= 1),
    .by = c(season, bonus_half)
  ) |>
  tidyr::pivot_wider(
    names_from = bonus_half,
    values_from = c(pct_any_single, pct_any_double),
    names_glue = "{.value}_h{bonus_half}"
  ) |>
  dplyr::rename(
    h1_single = pct_any_single_h1,
    h2_single = pct_any_single_h2,
    h1_double = pct_any_double_h1,
    h2_double = pct_any_double_h2
  ) |>
  dplyr::mutate(season_label = purrr::map_chr(season, season_lbl)) |>
  dplyr::select(season_label, h1_single, h1_double, h2_single, h2_double) |>
  dplyr::arrange(dplyr::desc(season_label)) # most recent season first

bonus_header <- glue::glue(
  "<div style='display:flex; justify-content:space-between; align-items:center;'>
    <div>
      <img src='https://a.espncdn.com/combiner/i?img=/redesign/assets/img/icons/ESPN-icon-basketball.png'
           style='height:40px; width:auto; vertical-align:middle;'>
    </div>
    <div style='flex-grow:1; margin-left:30px; margin-right:30px;'>
      <span style='display:block; font-weight:bold; text-align:center; font-size:24px;'>
        How Often Are Teams in the Bonus?
      </span>
      <span style='font-size:14px; font-weight:normal; display:block; text-align:center;'>
        Percentage of games where at least one team reaches the bonus.<br>
      </span>
    </div>
    <div>
      <img src='https://a.espncdn.com/combiner/i?img=/redesign/assets/img/icons/ESPN-icon-basketball.png'
           style='height:40px; width:auto; vertical-align:middle;'>
    </div>
  </div><br>"
)

tbl_bonus_pct <- bonus_pct_data |>
  gt::gt() |>
  gt_theme_gtutils() |>
  gt_column_subheaders(
    season_label = list(heading = "Season", subtitle = ""),
    h1_single = list(heading = "Single Bonus", subtitle = "7\u20139 fouls"),
    h1_double = list(heading = "Double Bonus", subtitle = "10+ fouls"),
    h2_single = list(heading = "Single Bonus", subtitle = "7\u20139 fouls"),
    h2_double = list(heading = "Double Bonus", subtitle = "10+ fouls"),
    heading_color = "black",
    subtitle_color = "gray"
  ) |>
  gt::fmt_percent(
    columns = c(h1_single, h1_double, h2_single, h2_double),
    decimals = 1
  ) |>
  gt::tab_spanner(
    label = "First Half",
    columns = c(h1_single, h1_double)
  ) |>
  gt::tab_spanner(
    label = "Second Half",
    columns = c(h2_single, h2_double)
  ) |>
  gt::tab_style(
    style = list(gt::cell_text(
      font = gt::google_font("Signika Negative"),
      size = gt::px(15),
      weight = "bold"
    )),
    locations = gt::cells_column_spanners()
  ) |>
  gt::data_color(
    columns = c(h1_single, h1_double, h2_single, h2_double),
    direction = "column",
    method = "numeric",
    palette = c("#ffffbf", "#fdae61", "#d7191c"),
    alpha = 0.7
  ) |>
  gt::tab_header(title = gt::html(bonus_header)) |>
  gt::tab_source_note(
    source_note = gt::html(
      "<hr>Play-by-play data via stats.ncaa.org + @cobrastats | theme via {gtUtils}<br>
     Single bonus is 7\u20139 team fouls per half and double bonus is 10+. Fouls reset at halftime.<br>
     <hr><b>Table by Chris at Bless your Chart</b>"
    )
  ) |>
  gt::tab_options(table.width = gt::px(640)) |>
  gt::tab_style(
    locations = gt::cells_source_notes(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      size = gt::px(11.5),
      weight = 250
    )
  ) |>
  gt::tab_style(
    style = list(gt::cell_text(
      font = gt::google_font("Signika Negative"),
      size = gt::px(14)
    )),
    locations = gt::cells_body(
      rows = gt::everything(),
      columns = gt::everything()
    )
  ) |>
  gt::tab_style(
    style = list(gt::cell_borders(
      sides = c("left", "right", "top", "bottom"),
      color = "black",
      weight = gt::px(2)
    )),
    locations = list(
      gt::cells_body(),
      gt::cells_column_labels(),
      gt::cells_row_groups(),
      gt::cells_column_spanners()
    )
  ) |>
  gt_border_bars_bottom(c("#0d1e2d", "#c1d6e2", "#f07e1d"))

# ---- Chart 2: Vanderbilt vs Alabama bonus state tile (January 7, 2026) -----
#
# Shows the bonus state each team was *in* across all 40 minutes of the game —
# the most free-throw-heavy SEC game of the 2025-26 season (88 combined FTs).
# Fill: gray = no bonus | light orange = one-and-one | dark orange = 2-shot.
# The fouling team determines the opponent's bonus state, so the "Alabama" row
# shows what Vanderbilt faced, and vice versa.

alaba_van_id <- 6516602L # Vanderbilt (H) vs Alabama (A), 2026-01-07

# Pull foul events from the already-loaded all_fouls — no extra file reads
game_fouls_av <- all_fouls |>
  dplyr::filter(ID == alaba_van_id, counts_toward_team_foul) |>
  dplyr::mutate(
    game_minute = dplyr::if_else(
      bonus_half == 1L,
      seconds_into_half / 60,
      20 + seconds_into_half / 60
    )
  )

# For each half-minute of the game, determine the bonus state each team is in
# (a team's bonus state is set by the OPPONENT's cumulative foul count)
av_teams <- c("Alabama", "Vanderbilt")
av_minutes <- seq(0, 39.5, by = 0.5)

bonus_timeline_av <- purrr::map(av_teams, \(receiving_team) {
  fouling_team <- dplyr::if_else(
    receiving_team == "Alabama",
    "Vanderbilt",
    "Alabama"
  )

  fouls_t <- game_fouls_av |>
    dplyr::filter(Event_Team == fouling_team)

  purrr::map_dfr(av_minutes, \(m) {
    half <- dplyr::if_else(m < 20, 1L, 2L)
    n_fouls <- fouls_t |>
      dplyr::filter(bonus_half == half, game_minute <= m) |>
      nrow()

    tibble::tibble(
      team = receiving_team,
      game_minute = m,
      bonus_state = dplyr::case_when(
        n_fouls < 7L ~ "No Bonus",
        n_fouls < 10L ~ "Single Bonus",
        TRUE ~ "Double Bonus"
      )
    )
  })
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    bonus_state = factor(
      bonus_state,
      levels = c("No Bonus", "Single Bonus", "Double Bonus")
    ),
    # Vanderbilt on top (they're the home team and winner)
    team = factor(team, levels = c("Vanderbilt", "Alabama"))
  )

plot_game_tile <- bonus_timeline_av |>
  ggplot2::ggplot(ggplot2::aes(x = game_minute, y = team, fill = bonus_state)) +
  ggplot2::geom_tile(
    width = 0.5,
    height = 0.7,
    color = "white",
    linewidth = 0.3
  ) +
  ggplot2::geom_vline(
    xintercept = 20,
    color = "gray30",
    linewidth = 1,
    linetype = "dashed"
  ) +
  ggplot2::annotate(
    "text",
    x = 20.4,
    y = 2.52,
    label = "Halftime",
    size = 3,
    color = "gray30",
    fontface = "italic",
    hjust = 0
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "No Bonus" = "#e8e8e8",
      "Single Bonus" = "#f5c18a",
      "Double Bonus" = "#f07e1d"
    ),
    name = NULL
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
    labels = c("0'", "5'", "10'", "15'", "H", "25'", "30'", "35'", "40'"),
    expand = ggplot2::expansion(add = 0.5)
  ) +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::labs(
    title = "Vanderbilt 96, Alabama 90 | January 7, 2026",
    subtitle = "Bonus state by team across 40 minutes of game time.\nLight orange indicates one-and-one bonus. Dark orange is double-bonus or two fouls shots. \n88 combined free throws | Vanderbilt: 35-50 FTM-FTA | Alabama: 29-38 FTM-FTA\n",
    caption = "Play-by-play data via stats.ncaa.org + @cobrastats\nViz by Chris at Bless your Chart"
  ) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      color = "black",
      face = "bold",
      size = 14
    ),
    plot.subtitle = ggplot2::element_text(size = 10, lineheight = 1.3),
    axis.text.x = ggplot2::element_text(color = "black", size = 10),
    axis.text.y = ggplot2::element_text(
      face = "bold",
      color = "black",
      size = 12
    ),
    legend.position = "bottom",
    panel.grid = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(
      size = 9,
      color = "gray40",
      hjust = 0
    )
  )

# ---- Save outputs -----------------------------------------------------------

fs::dir_create("plots")

gtUtils::gt_save_crop(
  tbl_bonus_pct,
  file = "plots/tbl_bonus_pct.png",
  whitespace = 40,
  bg = "#FFFDF5"
)

ggplot2::ggsave(
  "plots/alabama_vanderbilt_tile.png",
  plot_game_tile,
  width = 10,
  height = 4.5,
  dpi = 600
)

message(
  "Saved:\n  plots/tbl_bonus_pct.png\n  plots/alabama_vanderbilt_tile.png"
)
