knapp <- tibble::tribble(
  ~result, ~opp,              ~innings, ~pitch_count, ~bf, ~hits, ~er, ~bb, ~k, ~metadata,
  "-",     "Texas Tech",         3,         46,        11,  2, 1, 0,  3,   "W, 5-1 | 2:40",
  "-",     "East Carolina",      3,         73,        14,  4, 2, 1,  2,   "W, 11-6 | 3:50",
  "W",     "Stony Brook",        5,         93,        23,  6, 1, 1,  8,   "W, 6-1 | 2:40",
  "W",     "Stanford",           7,        111,        26,  4, 1, 1,  7,   "W, 11-1 (ACC) | 2:33",
  "W",     "Louisville",         7,        107,        28,  4, 1, 2,  8,   "W, 6-4 (ACC) | 3:03",
  "W",     "Boston College",   6.2,         85,        24,  2, 1, 1,  5,   "W, 5-1 (ACC) | 2:56",
  "W",     "Miami",              7,        115,        26,  5, 0, 0,  10,  "W, 2-0 (ACC) | 2:20",
  "W",     "Duke",               7,        112,        28,  4, 1, 1,  2,   "W, 4-3 (ACC) | 2:55",
  "W",     "Wake Forest",      5.2,        116,        24,  4, 1, 3,  3,   "W, 11-1 (ACC) |	2:35",
  "W",     "Virginia Tech",    5.1,        101,        23,  5, 3, 2,  7,   "W, 9-6 (ACC) | 4:01",
  "W",     "Pittsburgh",       6.1,        103,        25,  5, 3, 1,  8,   "W, 15-5 (ACC) | 3:25",
  "W",     "NC State",           8,        107,        28,  5, 1, 0,  4,  "W, 8-1 ACC | 2:24",
  "W",     "Florida State",      9,        115,        31,  4, 3, 0,  7,  "W, 8-3 (ACC) | 2:40",
  "W",     "Boston College",     7,         83,        25,  5, 2, 0,  4,  "W, 7-2 (ACCT) | 2:48",
  "W",     "Holy Cross",       8.1,        119,        30, 4, 0, 3,   7,  "W, 4-0 (NCAA Regional) | 2:45",
  "W",     "Arizona",            7,        101,        31, 9, 2, 0,   3,  "W, 18-2 (NCAA Super Regional) | 2:51"
)


kp_wide <- knapp |>
  dplyr::rowwise() |>
  dplyr::mutate(inn_split = list({
    full <- floor(innings)
    partial <- innings - full
    split <- c(rep(1, full), partial)
    length(split) <- 9
    split[is.na(split)] <- 0
    split
  })) |>
  tidyr::unnest_wider(inn_split, names_sep = "_") |>
  dplyr::rename_with(
    .fn = ~ paste0("inn_", seq_along(.)),
    .cols = dplyr::starts_with("inn_split")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    inn_6 = dplyr::if_else(!inn_6 %in% c(0 | 1), 0.5, inn_6),
    inn_7 = dplyr::if_else(!inn_7 %in% c(0 | 1), 0.5, inn_7),
    inn_8 = dplyr::if_else(!inn_8 %in% c(0 | 1), 0.5, inn_8)
  )

# add custom header
knapp_header <- glue::glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
      <div>
       <img src='https://a3.espncdn.com/combiner/i?img=%2Fredesign%2Fassets%2Fimg%2Ficons%2FESPN%2Dicon%2Dbaseball.png'
       style='height: 40px; width: auto; vertical-align: middle;'>
     </div>
     <div style='flex-grow:1; margin-left: 30px; margin-right: 30px'>
       <span style='display: block; font-weight: bold; text-align: center; font-size: 24px;'>Jake Knapp Pitching Log</span>
       <span style='font-size: 14px; font-weight: normal; display: block; text-align: center;'>Knapp is one of the most efficient pitchers in the country with a 0.86 WHIP in 102.1 innings.</span>
     </div>
     <div>
       <img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/153.png'
       style='height: 60px; width: auto; vertical-align: middle;'>
     </div>
   </div>
  <br>"
)

kp_tbl <- kp_wide |>
  cbbplotR::gt_cbb_teams(opp, opp, logo_height = 15) |>
  dplyr::select(
    opp,
    metadata,
    result,
    inn_1,
    inn_2,
    inn_3,
    inn_4,
    inn_5,
    inn_6,
    inn_7,
    inn_8,
    inn_9,
    innings,
    pitch_count,
    bf,
    hits, 
    bb,
    k,
    er,
  ) |>
  gt::gt() |>
  gtUtils::gt_theme_savant() |>
  gt::cols_align(opp, align = "left") |>
  gtUtils::gt_indicator_boxes(
    key_columns = c("result", "metadata", "opp", "innings", "pitch_count", "bf", "hits", "bb", "k", "er"),
    color_yes = "#56a0d3",
    color_na = "#e1e1e1",
    border_width = 0.25,
    box_width = 20, box_height = 20
  ) |>
  gt::fmt_markdown(opp) |>
  gtExtras::gt_merge_stack(
    col1 = "opp",
    col2 = "metadata",
    palette = c("black", "#333333"),
    small_cap = FALSE
  ) |> 
  gt::rows_add(
    result = "14-0",
    opp = "TOTALS",
    innings = 102.1,
    pitch_count = 1587,
    bf = 397,
    hits = 72,
    bb = 16,
    k =  88,
    er = 23
  ) |> 
  gtExtras::gt_merge_stack(
    col1 = "bb",
    col2 = "k",
    palette = c("black", "#333333")
  ) |> 
  gt::sub_missing(rows = gt::everything(), missing_text = "---") |> 
  gtUtils::gt_column_subheaders(
    opp = list(heading = "Opponent", subtitle = "Score | Game Duration"),
    result = list(heading = "Result", subtitle = "W/L/-"),
    innings = list(heading = "Innings", subtitle = "Pitched"),
    pitch_count = list(heading = "Pitch", subtitle = "Count"),
    bf = list(heading = "Batters", subtitle = "Faced"),
    hits = list(heading = "Hits", subtitle = "Allowed"),
    bb = list(heading = "Walks", subtitle = "Strikeouts"),
    er = list(heading = "Runs", subtitle = "Earned"),
    heading_color = "black",
    subtitle_color = "gray"
  ) |> 
  gt::tab_spanner(columns = gt::starts_with("inn_"), label = "Innings") |>
  gt::cols_label(!!!rlang::set_names(as.character(1:9), paste0("inn_", 1:9))) |>
  gt::tab_header(title = gt::html(knapp_header)) |>
  gt::tab_source_note(
    source_note = gt::html(
      "<hr>The 2025 ACC Pitcher of the Year, Knapp's 14 wins match the single-season UNC record set by Greg Norris in 1978.<br>
      Knapp has thrown at least 7 innings in his last 5 starts and earned the win in each of those appearances.<br>
      Knapp has hit seven batters this season (season-high: 2 Arizona, at Boston College).<br>
      On average, Knapp throws ~16 pitches per inning and faces ~4 batters per inning.<br>
  12 of Knapp's 15 starts have finished with a game duration under three hours.<br>
  <hr><b>Table by Chris at Bless your chart | data via ncaa.org through
                                             June 6 games</b>"
    )
  ) |>
  gtUtils::gt_border_bars_bottom(c("#56a0d3", "#89BDE0", "#BBD9ED")) |>
  gtExtras::gt_highlight_rows(
    columns = c(result),
    rows = c(3:16),
    fill = "#BBD9ED"
  ) |>
  gt::cols_align(result, align = "center") |>
  gtExtras::gt_highlight_rows(columns =  -gt::starts_with("inn_"),
    rows = c(17), fill = "#fffec8") |>
  gtExtras::gt_highlight_rows(columns = c(pitch_count),
    rows = c(15), fill = "lightpink") |>
  gtExtras::gt_highlight_rows(columns = c(innings),
                              rows = c(13), fill = "lightpink") |>
  gtExtras::gt_highlight_rows(columns = c(bf),
                              rows = c(13, 16), fill = "lightpink") |>
  gtExtras::gt_highlight_rows(columns = c(hits),
                              rows = c(16), fill = "lightpink") |>
  gtExtras::gt_highlight_rows(columns = c(bb),
                              rows = c(7), fill = "lightpink") |>
  gtExtras::gt_highlight_rows(columns = c(er),
                              rows = c(10, 11, 13), fill = "lightpink") |>
  gt::tab_style(
    locations = gt::cells_source_notes(),
    style = gt::cell_text(
      font = gt::google_font("Roboto Condensed"),
      size = gt::px(11.5),
      weight = 450
    )
  ) |>
  gt::tab_style(
    style = list(gt::cell_text(
      font = gt::google_font("Roboto Condensed"),
      size = gt::px(14)
    )),
    locations = gt::cells_body(
      rows = gt::everything(),
      columns = gt::everything()
    )
  ) |>
  gtUtils::gt_border_grid(color = "black",
                          weight = 1,
                          include_labels = FALSE) |>
  gt::tab_options(table.width = gt::px(775))


gtUtils::gt_save_crop(
  kp_tbl,
  file = "kp_tbl.png",
  whitespace = 40,
  bg = "white"
)

kp_tbl