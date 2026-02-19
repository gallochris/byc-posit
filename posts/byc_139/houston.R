schedule <- bigballR::get_team_schedule(team.name="Houston",season="2024-25")

pbp_hou <- bigballR::get_play_by_play(schedule$Game_ID[40])

no_fts <- pbp_duke |>
  dplyr::mutate(
    scoring = dplyr::case_match(Event_Result,
                                "made" ~ TRUE,
                                "missed" ~ FALSE,
                                NA ~ FALSE),
    pts_value = dplyr::case_when(
      Event_Type == "Turnover" ~ -1,
      Event_Result == "missed" & Shot_Value %in% c(2, 3) ~ 0,
      Event_Result == "made" & Shot_Value == 2 ~ 2,
      Event_Result == "made" & Shot_Value == 3 ~ 3,
      .default = 0
    )) |> 
  dplyr::filter(Event_Type != "Free Throw")

# handle fts separately 
fts <- pbp_duke |> 
  dplyr::filter(Event_Type == "Free Throw") |> 
  dplyr::mutate(
    scoring = dplyr::case_match(Event_Result,
                                "made" ~ TRUE,
                                "missed" ~ FALSE,
                                NA ~ FALSE),
    pts_value = dplyr::case_when(
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "1of1") ~ 1,
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "1of2") ~ 1,
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "2of2") 
      & dplyr::lag( stringr::str_detect(Event_Description, "1of2") &
                      Event_Result == "made") ~ 2,
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "2of2") 
      & dplyr::lag( stringr::str_detect(Event_Description, "1of2") &
                      Event_Result == "missed") ~ 1,
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "1of3") ~ 1,
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "2of3") 
      & dplyr::lag( stringr::str_detect(Event_Description, "1of3") &
                      Event_Result == "made") ~ 2,
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "2of3") 
      & dplyr::lag( stringr::str_detect(Event_Description, "1of3") &
                      Event_Result == "missed") ~ 1,
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "3of3") 
      & dplyr::lag( stringr::str_detect(Event_Description, "2of3") &
                      Event_Result == "made") ~ 3,
      Event_Result == "made" & 
        stringr::str_detect(Event_Description, "3of3") 
      & dplyr::lag( stringr::str_detect(Event_Description, "2of3") &
                      Event_Result == "missed") ~ 2,
      Event_Result == "missed" & Shot_Value == 1 ~ 0))

game_plot <- no_fts |> 
  dplyr::bind_rows(fts) |> 
  dplyr::arrange(Game_Seconds) |> 
  dplyr::mutate(
    pts_shape = dplyr::case_match(pts_value,
                                  -1 ~ 4,
                                  0 ~ 48,
                                  1 ~ 49,
                                  2 ~ 50,
                                  3 ~ 51),
    scoring_type = dplyr::case_when(
      Event_Type == "Free Throw" & scoring == TRUE ~ "free_throw",
      Shot_Value == 2 & scoring == TRUE ~ "two_pointer",
      Shot_Value == 3 & scoring == TRUE ~ "three_pointer",
      scoring == FALSE ~ "miss",
      TRUE ~ "turnover"
    )
  )

game_plot <- no_fts |> 
  dplyr::bind_rows(fts) |> 
  dplyr::arrange(Game_Seconds) |> 
  dplyr::mutate(
    pts_shape = dplyr::case_match(pts_value,
                                  -1 ~ 4,
                                  0 ~ 48,
                                  1 ~ 49,
                                  2 ~ 50,
                                  3 ~ 51),
    scoring_type = dplyr::case_when(
      Event_Type == "Free Throw" & scoring == TRUE ~ "free_throw",
      Shot_Value == 2 & scoring == TRUE ~ "two_pointer",
      Shot_Value == 3 & scoring == TRUE ~ "three_pointer",
      scoring == FALSE ~ "miss",
      TRUE ~ "turnover"
    )
  )

game_plot |> 
  ggplot2::ggplot(ggplot2::aes(
    x = Game_Seconds,
    y = Poss_Team,
    color = scoring_type,
    size = pts_value / 2
  )) +
  ggplot2::geom_point(ggplot2::aes(shape = factor(pts_shape)),
                      position = ggplot2::position_dodge(width = 0.5)) +
  ggplot2::scale_color_manual(values = c(
    "free_throw" = "#78c679",    # Sea green for free throws
    "two_pointer" = "#31a354",   # Dark green for 2-pointers
    "three_pointer" = "#006d2c",  # Deep forest green for 3-pointers
    "miss" = "#cc0000",          # Red for misses
    "turnover" = "#a50f15"       # Red for turnovers
  )) +
  ggplot2::scale_shape_manual(values = c(4, 45, 49, 50, 51)) +
  ggplot2::scale_x_continuous(breaks = c(0,600, 1200, 1800, 2400), 
                              labels = c("1 minute", "10 minutes", "Halftime", 
                                         "30 Minutes", "40 minutes")) +
  ggplot2::geom_vline(xintercept = c(600, 1200, 1800),
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
    title = "Houston 70, Duke 67",
    subtitle = "Scoring possessions shown in different shades of green: <span style='color:#78c679'>free throw trips</span>, <span style='color:#31a354'>two-pointers</span>, <span style='color:#006d2c'>three-pointers</span><br>Empty possessions shown in red: <span style='color:#CC0000'>missed shots (-)</span> or <span style='color:#a50f15'>turnovers (x)</span>",
    caption = "data via bigballR + logos via cbbplotR | April 5, 2025 | Table by Chris at Bless your Chart"
  ) +
  ggplot2::annotate(
    "label",
    x = 2200,
    y = 1.5,
    label = "Final 10 minutes \nScore  \nHouston 25, Duke 9  \nRebounds  \nHouston 15, Duke 8",
    size = 3,
    color = "#333333",
    fill = "floralwhite",
    fontface = "bold",
    alpha = .3
  ) +
  ggplot2::annotate(
    "label",
    x = 250,
    y = 1.5,
    label = "61 possessions \nHouston 1.15 PPP  \nDuke 1.10 PPP",
    size = 3,
    color = "#333333",
    fill = "floralwhite",
    fontface = "bold",
    alpha = .3
  ) +
  ggplot2::annotate(
    "label",
    x = 1650,
    y = 1.5,
    label = "Largest lead \nDuke 54  \nHouston 42  \n12:38 remaining",
    size = 3,
    color = "#333333",
    fill = "floralwhite",
    fontface = "bold",
    alpha = .3
  ) -> dplot 
  
  # quarts

# 
qrts <- tibble::tribble(
  ~qrt, ~Houston, ~Duke, ~diff,
  "0-10", 9, 18, 9,
  "10-20", 19, 16, 3,
  "20-30", 17, 24, 7,
  "30-40", 25, 9, 16,
  "Final", 70, 67, 3
)

team_headers <- c(
  "Houston","Duke"
)

# cfbplotR urls from https://github.com/Kazink36/cfbplotR/blob/main/data-raw/logo_ref_2.csv
team_img_urls <- c(
  "http://a.espncdn.com/i/teamlogos/ncaa/500/248.png",
  "http://a.espncdn.com/i/teamlogos/ncaa/500/150.png"
)

md = function(x) {
  gt::html(x
           
  )
}

add_team_header <- function(team_name, img_url) {
  gtExtras::img_header(
    label = "",
    img_url = img_url,
    height = 20,
    palette = c("white")
  )
}
team_headers_with_images <- purrr::map2(team_headers, team_img_urls, add_team_header) |>
  purrr::set_names(team_headers)

qrts |> 
gt::gt() |>
gtUtils:::gt_theme_athletic() |>   
  gt::cols_label(qrt = "Minutes", diff = "+/-", !!!team_headers_with_images) |> 
  gt::fmt(
    columns = c(diff),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) |> 
  gt::tab_style(
    style = list(gt::cell_fill(color = "#99bad7")),
    locations = gt::cells_body(columns = c(diff), 
                               rows = c(1, 3))
  ) |> 
  gt::tab_style(
    style = list(gt::cell_fill(color = "#e48897")),
    locations = gt::cells_body(columns = c(diff), 
                               rows = c(2, 4, 5))
  ) |> 
  gt::cols_align(qrt, align = "left") |> 
  gt::cols_align(columns = c("Houston", "Duke", "diff"), align = "right") |> 
  gt::tab_header(title = "2025 Final Four: Houston 70, Duke 67",
subtitle = "Score by 10 minute segment of game time.") |>
  gt::tab_source_note(
    source_note =
      gt::html(
        "<hr>Data via kenpom.com | theme via {gtUtils} + logos from cbbplotR <br>
        <hr><b>Table by Chris at Bless your chart | April 5, 2025</b>"
      )
  ) |>
  gt::tab_style(
    locations = gt::cells_body(columns = diff),
    style = gt::cell_text(
      weight = "bold"
    )
  ) |> 
  gtUtils::gt_border_bars_bottom(c("#00b2a9", "#ef426f", "#ff8200"))
