# ── Setup ─────────────────────────────────────────────────────────────────────
home_team <- "Duke"
away_team <- "UConn"
home_col  <- "#00539B"
away_col  <- "#e4002b"

# ── Non-FT events ─────────────────────────────────────────────────────────────
no_fts <- st |>
  dplyr::filter(
    Event_Type %in% c(
      "Two Point Jumper", "Three Point Jumper", "Layup",
      "Dunk", "Hook", "Tip Shot", "Turnover"
    )
  ) |>
  dplyr::mutate(
    scoring = dplyr::case_match(
      Event_Result, "made" ~ TRUE, "missed" ~ FALSE, NA ~ FALSE
    ),
    pts_value = dplyr::case_when(
      Event_Type == "Turnover" ~ -1L,
      Event_Result == "missed" ~ 0L,
      Event_Result == "made" & Shot_Value == 2 ~ 2L,
      Event_Result == "made" & Shot_Value == 3 ~ 3L,
      .default = 0L
    ),
    ft_label = NA_character_
  )

# ── FTs: per-possession detail string ─────────────────────────────────────────
ft_detail <- st |>
  dplyr::filter(Event_Type == "Free Throw") |>
  dplyr::arrange(Game_Seconds) |>
  dplyr::mutate(
    ft_symbol = dplyr::if_else(Event_Result == "made", "1", "\u00d7")
  ) |>
  dplyr::group_by(Poss_Num) |>
  dplyr::summarise(
    ft_made  = sum(Event_Result == "made"),
    ft_att   = dplyr::n(),
    ft_label = paste(ft_symbol, collapse = " "),
    .groups  = "drop"
  )

# Possessions that also had a made field goal (and-1 situations)
and1_fg <- no_fts |>
  dplyr::filter(scoring == TRUE, Event_Type != "Turnover") |>
  dplyr::select(Poss_Num, fg_pts = pts_value)

ft_events <- st |>
  dplyr::filter(Event_Type == "Free Throw") |>
  dplyr::group_by(Poss_Num) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup() |>
  dplyr::left_join(ft_detail, by = "Poss_Num") |>
  dplyr::left_join(and1_fg,   by = "Poss_Num") |>
  dplyr::mutate(
    scoring   = ft_made > 0 | !is.na(fg_pts),
    pts_value = ft_made + dplyr::coalesce(fg_pts, 0L),
    ft_label  = dplyr::if_else(
      !is.na(fg_pts),
      paste0(fg_pts, "+", ft_label),
      ft_label
    )
  ) |>
  dplyr::select(-fg_pts)

# ── FT summary (for team_summary) ─────────────────────────────────────────────
ft_summary_tbl <- st |>
  dplyr::filter(Event_Type == "Free Throw") |>
  dplyr::group_by(Poss_Team) |>
  dplyr::summarise(
    ft_made = sum(Event_Result == "made"),
    ft_att  = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(ft_str = paste0(ft_made, "/", ft_att))

# ── Combine and build scoring type ────────────────────────────────────────────
game_plot <- no_fts |>
  dplyr::bind_rows(ft_events) |>
  dplyr::arrange(Game_Seconds) |>
  dplyr::filter(
    Event_Type %in% c(
      "Two Point Jumper", "Three Point Jumper", "Layup",
      "Dunk", "Hook", "Tip Shot", "Free Throw", "Turnover"
    )
  ) |>
  dplyr::mutate(
    scoring_type = dplyr::case_when(
      Event_Type == "Turnover"                        ~ "turnover",
      Event_Type == "Free Throw" & scoring == TRUE    ~ "free_throw",
      Event_Type == "Free Throw" & scoring == FALSE   ~ "miss_ft",
      Shot_Value == 2 & scoring == TRUE               ~ "two_pointer",
      Shot_Value == 3 & scoring == TRUE               ~ "three_pointer",
      scoring == FALSE & Shot_Value == 2              ~ "miss_2",
      scoring == FALSE & Shot_Value == 3              ~ "miss_3",
      TRUE                                            ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(scoring_type))

# ── Half labels ───────────────────────────────────────────────────────────────
half_labels <- c("1st Half", "2nd Half")

# ── Lollipop data ─────────────────────────────────────────────────────────────
lollipop_data <- game_plot |>
  dplyr::mutate(
    is_home_poss = (Poss_Team == home_team),
    y_val        = dplyr::if_else(is_home_poss, Poss_Length, -Poss_Length),
    isTransition = Poss_Length <= 8,
    dot_label = dplyr::case_when(
      scoring_type == "turnover"                            ~ "\u00d7",
      scoring_type %in% c("free_throw", "miss_ft")         ~ ft_label,
      scoring_type %in% c("two_pointer", "miss_2")         ~ "2",
      scoring_type %in% c("three_pointer", "miss_3")       ~ "3",
      TRUE                                                  ~ ""
    ),
    dot_fontface = dplyr::if_else(
      scoring_type %in% c("free_throw", "two_pointer", "three_pointer"),
      "bold", "plain"
    ),
    dot_color = dplyr::if_else(is_home_poss, home_col, away_col),
    label_color = dplyr::case_when(
      scoring_type == "turnover"                            ~ "#cc0000",
      scoring_type %in% c("miss_ft", "miss_2", "miss_3")   ~ "#cc0000",
      scoring_type == "free_throw"                          ~ "#2E8B57",
      scoring_type == "two_pointer"                         ~ "#006400",
      scoring_type == "three_pointer"                       ~ "#004225",
      TRUE                                                  ~ "#333333"
    ),
    half = factor(
      dplyr::if_else(Game_Seconds <= 1200, "1st Half", "2nd Half"),
      levels = half_labels
    )
  ) |>
  dplyr::group_by(Poss_Num) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup() |>
  dplyr::arrange(Game_Seconds)

# ── Possession summary (for team_summary) ─────────────────────────────────────
poss_summary <- lollipop_data |>
  dplyr::group_by(Poss_Team) |>
  dplyr::summarise(
    possessions  = dplyr::n(),
    total_sec    = sum(Poss_Length, na.rm = TRUE),
    avg_poss_len = round(mean(Poss_Length, na.rm = TRUE), 1),
    .groups      = "drop"
  ) |>
  dplyr::mutate(
    time_fmt = sprintf("%d:%02d", total_sec %/% 60L, as.integer(total_sec %% 60L))
  )

# ── Offensive rebound markers ──────────────────────────────────────────────────
poss_start <- st |>
  dplyr::group_by(Poss_Num) |>
  dplyr::summarise(poss_start_sec = min(Game_Seconds), .groups = "drop")

oreb_data <- st |>
  dplyr::filter(Event_Type == "Offensive Rebound") |>
  dplyr::select(Poss_Num, Poss_Team, Game_Seconds, Poss_Length) |>
  dplyr::left_join(poss_start, by = "Poss_Num") |>
  dplyr::mutate(
    is_home_poss = (Poss_Team == home_team),
    elapsed      = Game_Seconds - poss_start_sec,
    y_oreb       = dplyr::if_else(is_home_poss, elapsed, -elapsed)
  ) |>
  dplyr::left_join(
    lollipop_data |> dplyr::select(Poss_Num, x_game_sec = Game_Seconds, half),
    by = "Poss_Num"
  ) |>
  dplyr::filter(!is.na(x_game_sec))

# ── OREB summary (for team_summary) ───────────────────────────────────────────
oreb_summary <- st |>
  dplyr::filter(Event_Type == "Offensive Rebound") |>
  dplyr::count(Poss_Team, name = "orebs")

# ── Team summary ──────────────────────────────────────────────────────────────
team_summary <- poss_summary |>
  dplyr::left_join(oreb_summary,   by = "Poss_Team") |>
  dplyr::left_join(ft_summary_tbl, by = "Poss_Team") |>
  dplyr::mutate(
    orebs        = tidyr::replace_na(orebs, 0L),
    ft_str       = tidyr::replace_na(ft_str, "0/0"),
    time_fmt     = tidyr::replace_na(time_fmt, "0:00"),
    avg_poss_len = tidyr::replace_na(avg_poss_len, 0)
  )

home_stats <- team_summary |> dplyr::filter(Poss_Team == home_team)
away_stats <- team_summary |> dplyr::filter(Poss_Team == away_team)

# ___ Logos -----------

home_logo_data <- tibble::tibble(
  x    = c(50, 1250),
  y    = c(30, 30),
  team = "Duke",
  half = factor(c("1st Half", "2nd Half"), levels = half_labels)
)

away_logo_data <- tibble::tibble(
  x    = c(50, 1250),
  y    = c(-30, -30),
  team = "Connecticut",
  half = factor(c("1st Half", "2nd Half"), levels = half_labels)
)


# ── Score diff ribbon (full game) ─────────────────────────────────────────────
score_diff_overlay <- st |>
  dplyr::select(Game_Seconds, Home_Score, Away_Score) |>
  dplyr::distinct(Game_Seconds, .keep_all = TRUE) |>
  dplyr::arrange(Game_Seconds) |>
  dplyr::mutate(score_diff = Home_Score - Away_Score)

# ── Find where away team takes the lead ───────────────────────────────────────
lead_change_sec <- score_diff_overlay |>
  dplyr::arrange(Game_Seconds) |>
  dplyr::mutate(prev_diff = dplyr::lag(score_diff)) |>
  dplyr::filter(!is.na(prev_diff), score_diff <= 0, prev_diff > 0, Game_Seconds > 1200) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::pull(Game_Seconds)

# ── 10-minute marks ───────────────────────────────────────────────────────────
ten_min_lines <- tibble::tibble(
  Game_Seconds = c(600, 1800),
  half         = factor(c("1st Half", "2nd Half"), levels = half_labels)
)

# ── Detect scoring runs ───────────────────────────────────────────────────────
run_data <- lollipop_data |>
  dplyr::mutate(
    pts_scored = dplyr::case_when(
      scoring_type == "two_pointer"   ~ 2L,
      scoring_type == "three_pointer" ~ 3L,
      scoring_type == "free_throw"    ~ pts_value,
      TRUE                            ~ 0L
    ),
    scoring_team = dplyr::if_else(pts_scored > 0, Poss_Team, NA_character_)
  )

detect_runs <- function(data, min_pts = 6, min_edge = 6,
                        home_team = "", away_team = "") {
  
  scoring <- data |>
    dplyr::filter(pts_scored > 0) |>
    dplyr::arrange(Game_Seconds)
  
  if (nrow(scoring) < 2) {
    return(tibble::tibble(
      team = character(), start_sec = numeric(), end_sec = numeric(),
      team_pts = integer(), opp_pts = integer(), edge = integer(),
      duration_sec = numeric(), half = character(),
      is_home = logical(), run_label = character()
    ))
  }
  
  # ── Part 1: True X-0 runs (First 35 minutes) ─────────────────────────────
  x0_scoring <- scoring |> dplyr::filter(Game_Seconds <= 2100)
  
  x0_runs <- purrr::map(c("1st Half", "2nd Half"), \(h) {
    hd <- x0_scoring |> dplyr::filter(half == h)
    if (nrow(hd) < 1) return(NULL)
    
    runs      <- list()
    cur_team  <- NA_character_
    cur_pts   <- 0L
    cur_start <- NA_real_
    cur_end   <- NA_real_
    
    for (i in seq_len(nrow(hd))) {
      row  <- hd[i, ]
      team <- row$Poss_Team
      if (is.na(cur_team) || team == cur_team) {
        if (is.na(cur_team)) cur_start <- row$Game_Seconds
        cur_team <- team
        cur_pts  <- cur_pts + row$pts_scored
        cur_end  <- row$Game_Seconds
      } else {
        if (cur_pts >= min_pts) {
          runs <- c(runs, list(tibble::tibble(
            team         = cur_team,
            start_sec    = cur_start,
            end_sec      = cur_end,
            team_pts     = cur_pts,
            opp_pts      = 0L,
            edge         = cur_pts,
            duration_sec = cur_end - cur_start,
            half         = h
          )))
        }
        cur_team  <- team
        cur_pts   <- row$pts_scored
        cur_start <- row$Game_Seconds
        cur_end   <- row$Game_Seconds
      }
    }
    if (!is.na(cur_team) && cur_pts >= min_pts) {
      runs <- c(runs, list(tibble::tibble(
        team         = cur_team,
        start_sec    = cur_start,
        end_sec      = cur_end,
        team_pts     = cur_pts,
        opp_pts      = 0L,
        edge         = cur_pts,
        duration_sec = cur_end - cur_start,
        half         = h
      )))
    }
    dplyr::bind_rows(runs)
  }) |> dplyr::bind_rows()
  
  # ── Part 2: Differential runs (last 5 min) ───────────────────────────────
  # FIXED: Look-ahead scan to avoid splitting 15-5 into smaller chunks.
  late_scoring <- scoring |> dplyr::filter(Game_Seconds > 2100)
  
  diff_runs <- if (nrow(late_scoring) >= 2) {
    hd <- late_scoring
    n  <- nrow(hd)
    is_home_vec <- hd$Poss_Team == home_team
    cum_home    <- cumsum(dplyr::if_else(is_home_vec,  hd$pts_scored, 0L))
    cum_away    <- cumsum(dplyr::if_else(!is_home_vec, hd$pts_scored, 0L))
    cum_diff    <- cum_home - cum_away
    
    runs <- list()
    i    <- 1L
    while (i < n) {
      ref_home <- if (i > 1L) cum_home[i - 1L] else 0L
      ref_away <- if (i > 1L) cum_away[i - 1L] else 0L
      ref_diff <- if (i > 1L) cum_diff[i - 1L] else 0L
      
      best_j <- NULL
      for (j in (i + 1L):n) {
        local_diff <- cum_diff[j] - ref_diff
        # Instead of breaking, keep updating best_j to extend the run 
        # as long as the net edge is maintained.
        if (abs(local_diff) >= min_edge) {
          best_j <- j
        }
      }
      
      if (!is.null(best_j)) {
        is_home_run <- (cum_diff[best_j] - ref_diff) > 0
        h_pts    <- cum_home[best_j] - ref_home
        a_pts    <- cum_away[best_j] - ref_away
        duration <- hd$Game_Seconds[best_j] - hd$Game_Seconds[i]
        
        runs <- c(runs, list(tibble::tibble(
          team         = if (is_home_run) home_team else away_team,
          start_sec    = hd$Game_Seconds[i],
          end_sec      = hd$Game_Seconds[best_j],
          team_pts     = if (is_home_run) h_pts else a_pts,
          opp_pts      = if (is_home_run) a_pts else h_pts,
          edge         = abs(cum_diff[best_j] - ref_diff),
          duration_sec = duration,
          half         = "2nd Half"
        )))
        i <- best_j + 1L
      } else {
        i <- i + 1L
      }
    }
    dplyr::bind_rows(runs)
  } else { NULL }
  
  result <- dplyr::bind_rows(x0_runs, diff_runs)
  
  if (nrow(result) == 0) {
    return(result |> dplyr::mutate(
      is_home      = logical(),
      half         = factor(character(), levels = half_labels),
      run_label    = character()
    ))
  }
  
  result |>
    dplyr::mutate(
      is_home   = (team == home_team),
      half      = factor(half, levels = half_labels),
      run_label = paste0(
        team_pts, "-", opp_pts, " (",
        as.integer(duration_sec %/% 60), ":",
        sprintf("%02d", as.integer(duration_sec %% 60)), ")"
      )
    )
}

runs <- detect_runs(
  run_data,
  min_pts   = 6,
  min_edge  = 6,
  home_team = home_team,
  away_team = away_team
)

# ── X-axis labels ─────────────────────────────────────────────────────────────
game_clock_breaks <- c(seq(0, 1200, by = 120), seq(1200, 2400, by = 120)) |> unique()
game_clock_labels <- game_clock_breaks |>
  purrr::map_chr(function(sec) {
    half_sec  <- dplyr::if_else(sec <= 1200, sec, sec - 1200)
    mins_left <- (1200 - half_sec) / 60
    sprintf("%d:00", as.integer(mins_left))
  })

# ── Subtitle ──────────────────────────────────────────────────────────────────
full_subtitle <- glue::glue(
  "<span style='color:{away_col}'>**{away_team}**</span>: ",
  "{away_stats$possessions[1]} poss \u00b7 {away_stats$time_fmt[1]} \u00b7 ",
  "{away_stats$avg_poss_len[1]}s avg \u00b7 {away_stats$orebs[1]} OREB \u00b7 ",
  "FT {away_stats$ft_str[1]} <br><br>",
  "<span style='color:{home_col}'>**{home_team}**</span>: ",
  "{home_stats$possessions[1]} poss \u00b7 {home_stats$time_fmt[1]} \u00b7 ",
  "{home_stats$avg_poss_len[1]}s avg \u00b7 {home_stats$orebs[1]} OREB \u00b7 ",
  "FT {home_stats$ft_str[1]} &nbsp;&nbsp;&nbsp;",
)

# ── Plots ─────────────────────────────────────────────────────────────────────
max_diff <- max(score_diff_overlay$score_diff, na.rm = TRUE)
min_diff <- min(score_diff_overlay$score_diff, na.rm = TRUE)

p_ribbon <- score_diff_overlay |>
  ggplot2::ggplot(ggplot2::aes(x = Game_Seconds)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = pmax(score_diff, 0)), fill = home_col, alpha = 0.5) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = pmin(score_diff, 0), ymax = 0), fill = away_col, alpha = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = score_diff), color = "#333333", linewidth = 0.5) +
  { if (length(lead_change_sec) > 0)
    list(
      ggplot2::geom_vline(xintercept = lead_change_sec, linetype = "dashed", color = away_col, linewidth = 0.5),
      ggplot2::annotate("text", x = lead_change_sec, y = max(max_diff * 0.85, 1), label = paste0(away_team, " takes lead"), hjust = 1.08, size = 2.3, color = away_col, fontface = "bold", family = "Roboto Condensed")
    )
  } +
  ggplot2::annotate("text", x = 2420, y = max(max_diff * 0.6, 1), label = home_team, hjust = 0, size = 2.5, color = home_col, fontface = "bold", family = "Roboto Condensed") +
  ggplot2::annotate("text", x = 2420, y = min(min_diff * 0.6, -1), label = away_team, hjust = 0, size = 2.5, color = away_col, fontface = "bold", family = "Roboto Condensed") +
  ggplot2::scale_x_continuous(limits = c(0, 2400), expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(labels = function(x) abs(x)) +
  hrbrthemes::theme_ipsum(base_family = "Roboto Condensed", grid = FALSE, plot_margin = ggplot2::margin(5, 40, 0, 5)) +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(size = 7, color = "#999999"), axis.title = ggplot2::element_blank(), plot.title = ggtext::element_markdown(size = 16, face = "bold"), plot.subtitle = ggtext::element_markdown(size = 9, color = "#555555")) +
  ggplot2::labs(title = "UConn 73, Duke 72", x = "", y = "", subtitle = full_subtitle)

runs_home <- runs |> dplyr::filter(is_home)
runs_away <- runs |> dplyr::filter(!is_home)

p_lollipop <- lollipop_data |>
  ggplot2::ggplot(ggplot2::aes(x = Game_Seconds, y = y_val)) +
  {
    layers <- list()
    if (nrow(runs_home) > 0) {
      layers <- c(layers, list(
        ggplot2::geom_rect(data = runs_home, ggplot2::aes(xmin = start_sec - 5, xmax = end_sec + 5, ymin = 0, ymax = Inf, fill = is_home), alpha = 0.08, inherit.aes = FALSE),
        ggplot2::geom_label(data = runs_home, ggplot2::aes(x = (start_sec + end_sec) / 2, y = 32, label = run_label, color = home_col), size = 3, fontface = "bold", vjust = 0, label.size = 0, fill = "white", alpha = 0.8, family = "Roboto Condensed", inherit.aes = FALSE)
      ))
    }
    if (nrow(runs_away) > 0) {
      layers <- c(layers, list(
        ggplot2::geom_rect(data = runs_away, ggplot2::aes(xmin = start_sec - 5, xmax = end_sec + 5, ymin = -Inf, ymax = 0, fill = is_home), alpha = 0.08, inherit.aes = FALSE),
        ggplot2::geom_label(data = runs_away, ggplot2::aes(x = (start_sec + end_sec) / 2, y = -32, label = run_label, color = away_col), size = 3, fontface = "bold", vjust = 1, label.size = 0, fill = "white", alpha = 0.8, family = "Roboto Condensed", inherit.aes = FALSE)
      ))
    }
    layers
  } +
  ggplot2::scale_fill_manual(values = stats::setNames(c(home_col, away_col), c(TRUE, FALSE)), guide = "none") +
  ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  cfbplotR::geom_cfb_logos(
    data = home_logo_data,
    ggplot2::aes(x = x, y = y, team = team),
    height = 0.085, alpha = 0.5, inherit.aes = FALSE
  ) +
  cfbplotR::geom_cfb_logos(
    data = away_logo_data,
    ggplot2::aes(x = x, y = y, team = team),
    height = 0.085, alpha = 0.5, inherit.aes = FALSE
  ) +
  ggplot2::geom_vline(data = ten_min_lines, ggplot2::aes(xintercept = Game_Seconds), linetype = "dashed", color = "#cccccc", linewidth = 0.3) +
  ggplot2::geom_segment(ggplot2::aes(x = Game_Seconds, xend = Game_Seconds, y = 0, yend = y_val, color = dot_color, linetype = isTransition), linewidth = 0.6, alpha = 0.35) +
  ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "dotted"), c(FALSE, TRUE)), guide = "none") +
  ggplot2::geom_point(data = oreb_data, ggplot2::aes(x = x_game_sec, y = y_oreb), shape = 21, size = 2, fill = NA, color = "gray50", stroke = 0.7, inherit.aes = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = dot_label, color = label_color, fontface = dot_fontface, vjust = dplyr::if_else(y_val >= 0, 0, 1)), size = 2.5, family = "Roboto Condensed") +
  ggplot2::scale_color_identity() +
  ggplot2::facet_wrap(~ half, ncol = 1, scales = "free_x", strip.position = "left") +
  ggplot2::scale_x_continuous(breaks = game_clock_breaks, labels = game_clock_labels, expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
  ggplot2::scale_y_continuous(breaks = seq(-30, 30, by = 10), labels = abs(seq(-30, 30, by = 10))) +
  ggplot2::coord_cartesian(clip = "off") +
  hrbrthemes::theme_ipsum(base_family = "Roboto Condensed", grid = FALSE, plot_margin = ggplot2::margin(2, 40, 5, 5)) +
  ggplot2::theme(legend.position = "none", panel.spacing = ggplot2::unit(6, "pt"), strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, size = 9, face = "bold", color = "#555555", family = "Roboto Condensed"), strip.background = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(size = 7, color = "#999999"), axis.text.y = ggplot2::element_text(size = 7, color = "#999999"), axis.title.y = ggplot2::element_text(size = 9, color = "#555555"), plot.subtitle = ggtext::element_markdown(size = 7.5, lineheight = 1.3), plot.caption = ggplot2::element_text(size = 7, color = "#999999", face = "plain", family = "Roboto Condensed")) +
  ggplot2::labs(x = NULL, y = "Possession Length (s)", subtitle = glue::glue("Scoring: <span style='color:#2E8B57'>free throw trips</span>, <span style='color:#006400'>two-pointers</span>, <span style='color:#004225'>three-pointers</span> &nbsp;<br><span> &nbsp;Offensive rebounds (o)</span> &nbsp; <br>Empty possessions: <span style='color:#CC0000'>missed shots or turnovers (\u00d7)</span><br>Dotted line: transition possessions"),  caption = "data via bigballR | March 29, 2026\nViz by Chris at Bless your Chart")

patchwork::wrap_plots(p_ribbon, p_lollipop, ncol = 1, heights = c(1, 5))


combined_plot <- patchwork::wrap_plots(p_ribbon, p_lollipop, ncol = 1, heights = c(1, 5))

ggplot2::ggsave(
  filename = "uconn_duke.png",
  plot     = combined_plot,
  width    = 10,
  height   = 8.5,
  dpi      = 600,
  bg       = "white"
)

