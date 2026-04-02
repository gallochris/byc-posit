# set teams
away_team <- "VCU"
home_team <- "North Carolina"
home_col <- "#56a0d3"
away_col <- "black"


schedule <- bigballR::get_team_schedule(season = "2025-26", team.name = "North Carolina")


# Get play by play for all games played so far in season
vcu <- bigballR::get_play_by_play(schedule$Game_ID[33])


no_fts <- vcu |>
  dplyr::filter(
    Event_Type %in% c(
      "Two Point Jumper",
      "Three Point Jumper",
      "Layup",
      "Dunk",
      "Hook",
      "Tip Shot",
      "Turnover"
    )
  ) |>
  dplyr::mutate(
    scoring = dplyr::case_match(Event_Result, "made" ~ TRUE, "missed" ~ FALSE, NA ~ FALSE),
    pts_value = dplyr::case_when(
      Event_Type == "Turnover" ~ -1,
      Event_Result == "missed" & Shot_Value %in% c(2, 3) ~ 0,
      Event_Result == "made" & Shot_Value == 2 ~ 2,
      Event_Result == "made" & Shot_Value == 3 ~ 3,
      .default = 0
    )
  )

# handle fts separately
fts <- vcu |>
  dplyr::filter(Event_Type == "Free Throw") |>
  dplyr::mutate(
    scoring = dplyr::case_match(Event_Result, "made" ~ TRUE, "missed" ~ FALSE, NA ~ FALSE),
    pts_value = dplyr::case_when(
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "1of1") ~ 1,
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "1of2") ~ 1,
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "2of2")
      & dplyr::lag(
        stringr::str_detect(Event_Description, "1of2") &
          Event_Result == "made"
      ) ~ 2,
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "2of2")
      & dplyr::lag(
        stringr::str_detect(Event_Description, "1of2") &
          Event_Result == "missed"
      ) ~ 1,
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "1of3") ~ 1,
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "2of3")
      & dplyr::lag(
        stringr::str_detect(Event_Description, "1of3") &
          Event_Result == "made"
      ) ~ 2,
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "2of3")
      & dplyr::lag(
        stringr::str_detect(Event_Description, "1of3") &
          Event_Result == "missed"
      ) ~ 1,
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "3of3")
      & dplyr::lag(
        stringr::str_detect(Event_Description, "2of3") &
          Event_Result == "made"
      ) ~ 3,
      Event_Result == "made" &
        stringr::str_detect(Event_Description, "3of3")
      & dplyr::lag(
        stringr::str_detect(Event_Description, "2of3") &
          Event_Result == "missed"
      ) ~ 2,
      Event_Result == "missed" & Shot_Value == 1 ~ 0
    )
  )

game_plot <- no_fts |>
  dplyr::bind_rows(fts) |>
  dplyr::arrange(Game_Seconds) |>
  dplyr::filter(
    Event_Type %in% c(
      "Two Point Jumper",
      "Three Point Jumper",
      "Layup",
      "Dunk",
      "Hook",
      "Tip Shot",
      "Free Throw",
      "Turnover"
    )
  ) |>
  dplyr::mutate(
    scoring_type = dplyr::case_when(
      Event_Type == "Turnover" ~ "turnover",
      Event_Type == "Free Throw" & scoring == TRUE ~ "free_throw",
      Event_Type == "Free Throw" & scoring == FALSE ~ "miss_ft",
      Shot_Value == 2 & scoring == TRUE ~ "two_pointer",
      Shot_Value == 3 & scoring == TRUE ~ "three_pointer",
      scoring == FALSE & Shot_Value == 2 ~ "miss_2",
      scoring == FALSE & Shot_Value == 3 ~ "miss_3",
      TRUE ~ NA_character_
    ),
    pts_shape = dplyr::case_when(
      scoring_type == "turnover"      ~ 4L,
      scoring_type == "miss_ft"       ~ 49L,
      scoring_type == "miss_2"        ~ 50L,
      scoring_type == "miss_3"        ~ 51L,
      scoring_type == "free_throw"    ~ 49L,
      scoring_type == "two_pointer"   ~ 50L,
      scoring_type == "three_pointer" ~ 51L,
      TRUE                            ~ 45L
    ),
    display_size = dplyr::case_when(
      scoring_type == "turnover"      ~ 2.5,
      scoring_type == "miss_ft"       ~ 2,
      scoring_type == "miss_2"        ~ 2.5,
      scoring_type == "miss_3"        ~ 2.5,
      scoring_type == "free_throw"    ~ 3,
      scoring_type == "two_pointer"   ~ 3.5,
      scoring_type == "three_pointer" ~ 4,
      TRUE                            ~ 2
    )
  ) |>
  dplyr::filter(!is.na(scoring_type)) |>
  # Stagger free throws within each possession
  dplyr::group_by(Poss_Team, Poss_Num) |>
  dplyr::mutate(
    poss_event_seq = dplyr::row_number(),
    y_nudge = dplyr::case_when(
      scoring_type == "three_pointer" ~ 0.16,
      scoring_type == "two_pointer"   ~ 0.13,
      scoring_type == "free_throw"    ~ 0.06 + (poss_event_seq - 1) * 0.04,
      scoring_type %in% c("miss_2", "miss_3") ~ -0.10,
      scoring_type == "miss_ft"       ~ -0.06 - (poss_event_seq - 1) * 0.04,
      scoring_type == "turnover"      ~ -0.16,
      TRUE ~ 0
    )
  ) |>
  dplyr::ungroup()

score_diff_overlay <- game_plot |>
  dplyr::select(Game_Seconds, Away_Score, Home_Score) |>
  dplyr::distinct() |>
  dplyr::mutate(
    score_diff = Away_Score - Home_Score,
    max_diff = max(abs(score_diff)),
    # Flip the sign so ribbon goes DOWN when away (Duke) leads
    diff_scaled = 1.5 + (score_diff / max_diff * 0.25)
  )

max_unc_lead <- score_diff_overlay |>
  dplyr::filter(score_diff == max(score_diff)) |>
  dplyr::slice(1)

max_ul_lead <- score_diff_overlay |>
  dplyr::filter(score_diff == min(score_diff)) |>
  dplyr::slice(1)

game_plot |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = Game_Seconds,
      y = as.numeric(factor(Poss_Team)) + y_nudge,
      color = scoring_type,
      size = display_size
    )
  ) +
  ggplot2::geom_point(ggplot2::aes(shape = pts_shape)) +
  ggplot2::scale_shape_identity() +
  ggplot2::scale_size_identity() +
  ggplot2::scale_y_continuous(breaks = seq_along(sort(unique(
    game_plot$Poss_Team
  ))), labels = sort(unique(game_plot$Poss_Team))) +
  ggplot2::scale_color_manual(
    values = c(
      "free_throw"    = "#2E8B57",
      "two_pointer"   = "#006400",
      "three_pointer" = "#004225",
      "miss_ft"       = "#cc0000",
      "miss_2"        = "#cc0000",
      "miss_3"        = "#cc0000",
      "turnover"      = "#cc0000"
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 600, 1200, 1800, 2400, 2700),
    labels = c("1 minute", "10 minutes", "Halftime", "30 Minutes", "40 minutes", "45 minutes")
  ) +
  ggplot2::geom_vline(xintercept = 1200,
                      linetype = "dashed",
                      color = "#333333") +
  ggthemes::theme_fivethirtyeight() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.y = cbbplotR::element_cbb_teams(size = 0.9),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown()
  ) +
  ggplot2::labs(
    x = "Possessions",
    y = "",
    title = "VCU 82, North Carolina 78 (OT)",
    subtitle = "Scoring possessions shown in different shades of green: <span style='color:#2E8B57'>free throw trips</span>, <span style='color:#006400'>two-pointers</span>, <span style='color:#004225'>three-pointers</span><br>Empty possessions shown in red: <span style='color:#CC0000'>missed shots</span> or <span style='color:#CC0000'>turnovers (x)</span>",
    caption = "data via bigballR + logos via cbbplotR | March 19, 2026 \nTable by Chris at Bless your Chart"
  ) +
  ggplot2::annotate(
    "label",
    x = 200,
    y = 2.45,
    label = "VCU: 1.07 PPP  \nUNC: 1.02 PPP  \n68 possessions",
    size = 2.5,
    color = "#333333",
    fill = "floralwhite",
    fontface = "bold",
    alpha = .3
  ) +
  ggplot2::annotate(
    "label",
    x = 1200,
    y = 2.5,
    label = "UNC 39, VCU 28",
    size = 2.5,
    color = "#333333",
    fill = "floralwhite",
    fontface = "bold"
  ) +
  ggplot2::geom_ribbon(
    data = score_diff_overlay,
    ggplot2::aes(
      x = Game_Seconds,
      ymin = 1.5,
      ymax = diff_scaled,
      fill = score_diff > 0
    ),
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_line(
    data = score_diff_overlay,
    ggplot2::aes(x = Game_Seconds, y = diff_scaled),
    color = "#333333",
    size = 0.8,
    inherit.aes = FALSE
  ) +
  ggplot2::annotate(
    "label",
    x = 1525,
    y = 1.40,
    label = "UNC largest lead \n56-37",
    size = 2.5,
    color = "#333333",
    fill = "floralwhite",
    fontface = "bold"
  ) +
  ggplot2::geom_hline(
    yintercept = 1.5,
    linetype = "dotted",
    color = "#666666",
    alpha = 0.5
  ) +
  ggplot2::geom_hline(
    yintercept = 1.5,
    linetype = "dotted",
    color = "#666666",
    alpha = 0.5
  ) +
  ggplot2::scale_fill_manual(values = c("TRUE" = away_col, "FALSE" = home_col),
                             guide = "none") +
  ggplot2::geom_vline(
    xintercept = 2399,
    linetype = "dashed",
    color = "black",
    linewidth = 0.6,
    alpha = 0.2
  ) -> pp



# save the plot
ggplot2::ggsave(
  "unc_vcu_plot.png",
  pp,
  w = 9.5,
  h = 7,
  dpi = 600,
  type = 'cairo'
)
