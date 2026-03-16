pbp <- bigballR::get_play_by_play(schedule$Game_ID[24:32])

scores_by_5min_span <- pbp |>
  dplyr::mutate(
    Minute_Mark = dplyr::case_when(
      Game_Seconds <= 300  ~ 5,
      Game_Seconds <= 600  ~ 10,
      Game_Seconds <= 900  ~ 15,
      Game_Seconds <= 1200 ~ 20,
      Game_Seconds <= 1500 ~ 25,
      Game_Seconds <= 1800 ~ 30,
      Game_Seconds <= 2100 ~ 35,
      Game_Seconds <= 2400 ~ 40,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::filter(!is.na(Minute_Mark)) |>
  dplyr::group_by(ID, Minute_Mark) |>
  dplyr::arrange(Game_Seconds, .by_group = TRUE) |>
  dplyr::slice(dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::group_by(ID) |>
  dplyr::mutate(
    Prev_Home_Score         = dplyr::lag(Home_Score, default = 0),
    Prev_Away_Score         = dplyr::lag(Away_Score, default = 0),
    Home_Points_In_Span     = Home_Score - Prev_Home_Score,
    Away_Points_In_Span     = Away_Score - Prev_Away_Score,
    UNC_Opponent            = dplyr::if_else(Home == "North Carolina", Away, Home),
    UNC_Points_In_Span      = dplyr::if_else(
      Home == "North Carolina",
      Home_Points_In_Span,
      Away_Points_In_Span
    ),
    Opponent_Points_In_Span = dplyr::if_else(
      Home == "North Carolina",
      Away_Points_In_Span,
      Home_Points_In_Span
    ),
    UNC_Span_Diff = UNC_Points_In_Span - Opponent_Points_In_Span
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(UNC_Opponent) |>
  dplyr::mutate(
    Game_Num = dplyr::dense_rank(Date),
    UNC_Opponent_Label = dplyr::case_when(
      dplyr::n_distinct(Date) > 1 ~ paste0(UNC_Opponent, " (", Game_Num, ")"),
      TRUE ~ UNC_Opponent
    )
  ) |>
  dplyr::ungroup()

# Build the full wide table (keep raw cols for totals row)
unc_spans_wide_full <- scores_by_5min_span |>
  dplyr::select(
    ID, Date, UNC_Opponent_Label, Minute_Mark,
    UNC_Span_Diff, UNC_Points_In_Span, Opponent_Points_In_Span
  ) |>
  tidyr::pivot_wider(
    names_from  = Minute_Mark,
    values_from = c(UNC_Span_Diff, UNC_Points_In_Span, Opponent_Points_In_Span),
    names_sep   = "_"
  ) |>
  dplyr::mutate(
    score_5    = paste0(UNC_Points_In_Span_5,  "-", Opponent_Points_In_Span_5),
    score_10   = paste0(UNC_Points_In_Span_10, "-", Opponent_Points_In_Span_10),
    score_15   = paste0(UNC_Points_In_Span_15, "-", Opponent_Points_In_Span_15),
    score_20   = paste0(UNC_Points_In_Span_20, "-", Opponent_Points_In_Span_20),
    score_25   = paste0(UNC_Points_In_Span_25, "-", Opponent_Points_In_Span_25),
    score_30   = paste0(UNC_Points_In_Span_30, "-", Opponent_Points_In_Span_30),
    score_35   = paste0(UNC_Points_In_Span_35, "-", Opponent_Points_In_Span_35),
    score_40   = paste0(UNC_Points_In_Span_40, "-", Opponent_Points_In_Span_40),
    diff_t         = rowSums(dplyr::across(dplyr::starts_with("UNC_Span_Diff_")),           na.rm = TRUE),
    UNC_total      = rowSums(dplyr::across(dplyr::starts_with("UNC_Points_In_Span_")),      na.rm = TRUE),
    Opponent_total = rowSums(dplyr::across(dplyr::starts_with("Opponent_Points_In_Span_")), na.rm = TRUE),
    score_total    = paste0(UNC_total, "-", Opponent_total)
  )

# Build totals row while raw cols still exist
totals_row <- unc_spans_wide_full |>
  dplyr::summarise(dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) |>
  dplyr::mutate(
    UNC_Opponent_Label = "TOTALS",
    score_5     = paste0(UNC_Points_In_Span_5,  "-", Opponent_Points_In_Span_5),
    score_10    = paste0(UNC_Points_In_Span_10, "-", Opponent_Points_In_Span_10),
    score_15    = paste0(UNC_Points_In_Span_15, "-", Opponent_Points_In_Span_15),
    score_20    = paste0(UNC_Points_In_Span_20, "-", Opponent_Points_In_Span_20),
    score_25    = paste0(UNC_Points_In_Span_25, "-", Opponent_Points_In_Span_25),
    score_30    = paste0(UNC_Points_In_Span_30, "-", Opponent_Points_In_Span_30),
    score_35    = paste0(UNC_Points_In_Span_35, "-", Opponent_Points_In_Span_35),
    score_40    = paste0(UNC_Points_In_Span_40, "-", Opponent_Points_In_Span_40),
    score_total = paste0(UNC_total, "-", Opponent_total)
  )

drop_and_rename <- function(df) {
  df |>
    dplyr::select(
      -dplyr::any_of("ID"),
      -dplyr::starts_with("UNC_Points_In_Span_"),
      -dplyr::starts_with("Opponent_Points_In_Span_")
    ) |>
    dplyr::rename_with(
      ~ stringr::str_replace(.x, "UNC_Span_Diff_", "diff_"),
      dplyr::starts_with("UNC_Span_Diff_")
    )
}

unc_spans_wide   <- drop_and_rename(unc_spans_wide_full)
totals_row_clean <- drop_and_rename(totals_row)

unc_spans_with_totals <- dplyr::bind_rows(
  totals_row_clean |> dplyr::mutate(row_group = "Totals", sort_order = 0),
  unc_spans_wide   |> dplyr::mutate(row_group = "Games",  sort_order = 1)
) |>
  dplyr::arrange(sort_order, dplyr::desc(as.Date(Date, format = "%m/%d/%Y"))) |>
  dplyr::relocate(row_group, sort_order, Date, UNC_Opponent_Label)

# make table
diff_tbl_a <- unc_spans_with_totals |>
  dplyr::mutate(
      opp_desc = dplyr::case_when(
        UNC_Opponent_Label == "Miami FL"    ~ paste0("Away | ", Date),
        UNC_Opponent_Label == "Pittsburgh"  ~ paste0("Home | ", Date),
        UNC_Opponent_Label == "NC State"    ~ paste0("Away | ", Date),
        UNC_Opponent_Label == "Syracuse"    ~ paste0("Away | ", Date),
        UNC_Opponent_Label == "Louisville"  ~ paste0("Home | ", Date),
        UNC_Opponent_Label == "Virginia Tech" ~ paste0("Home | ", Date),
        UNC_Opponent_Label == "Clemson (1)" ~ paste0("Home | ", Date),
        UNC_Opponent_Label == "Duke"        ~ paste0("Away | ", Date),
        UNC_Opponent_Label == "Clemson (2)" ~ paste0("Neutral (ACCT) | ", Date),
        UNC_Opponent_Label == "TOTALS"      ~ "5-4 in this span",
        TRUE ~ Date
      ),
    team_lookup = dplyr::case_when(
      UNC_Opponent_Label == "TOTALS"                          ~ "North Carolina",
      stringr::str_detect(UNC_Opponent_Label, "NC State")     ~ "NC State Wolfpack",
      stringr::str_detect(UNC_Opponent_Label, "Miami")        ~ "Miami FL",
      stringr::str_detect(UNC_Opponent_Label, "ETSU")         ~ "East Tennessee State",
      stringr::str_detect(UNC_Opponent_Label, "N.C. Central") ~ "North Carolina Central",
      stringr::str_detect(UNC_Opponent_Label, "Central Ark.") ~ "Central Arkansas",
      TRUE ~ stringr::str_remove(UNC_Opponent_Label, " \\(\\d+\\)$")
    )
  ) |>
  cbbplotR::gt_cbb_teams(team_lookup, team_lookup) |>
  dplyr::mutate(
    UNC_Opponent = dplyr::case_when(
      UNC_Opponent_Label == "TOTALS" ~ "TOTALS",
      TRUE ~ UNC_Opponent_Label
    ),
    Date = dplyr::if_else(is.na(Date), "", Date)
  ) |>
  dplyr::relocate(team_lookup) |>
  gt::gt(groupname_col = "row_group") |>
  gtUtils::gt_theme_athletic() |>
  gt::fmt_markdown(team_lookup) |>
  gtExtras::gt_merge_stack(
    col1 = team_lookup,
    col2 = opp_desc,
    palette = c("black", "#333333"),
    small_cap = FALSE
  ) |>
  gt::cols_hide(columns = c(sort_order, UNC_Opponent_Label, UNC_Opponent, opp_desc, Date)) |>
  gtExtras::gt_merge_stack(col1 = diff_5,  col2 = score_5,     palette = c("black", "#333333")) |>
  gtExtras::gt_merge_stack(col1 = diff_10, col2 = score_10,    palette = c("black", "#333333")) |>
  gtExtras::gt_merge_stack(col1 = diff_15, col2 = score_15,    palette = c("black", "#333333")) |>
  gtExtras::gt_merge_stack(col1 = diff_20, col2 = score_20,    palette = c("black", "#333333")) |>
  gtExtras::gt_merge_stack(col1 = diff_25, col2 = score_25,    palette = c("black", "#333333")) |>
  gtExtras::gt_merge_stack(col1 = diff_30, col2 = score_30,    palette = c("black", "#333333")) |>
  gtExtras::gt_merge_stack(col1 = diff_35, col2 = score_35,    palette = c("black", "#333333")) |>
  gtExtras::gt_merge_stack(col1 = diff_40, col2 = score_40,    palette = c("black", "#333333")) |>
  gtExtras::gt_merge_stack(col1 = diff_t,  col2 = score_total, palette = c("black", "#333333")) |>
  gt::cols_add(diff_gap  = "", .before = diff_25) |>
  gt::cols_add(total_gap = "", .before = diff_t) |>
  gt::cols_label(
    team_lookup  = "",
    diff_5       = "0-5",
    diff_10      = "5-10",
    diff_15      = "10-15",
    diff_20      = "15-20",
    diff_gap     = "",
    diff_25      = "20-25",
    diff_30      = "25-30",
    diff_35      = "30-35",
    diff_40      = "35-40",
    total_gap    = "",
    diff_t       = "TOTAL"
  ) |>
  gt::tab_spanner(label = "1st Half", columns = c(diff_5, diff_10, diff_15, diff_20)) |>
  gt::tab_spanner(label = "2nd Half", columns = c(diff_25, diff_30, diff_35, diff_40)) |>
  gt::fmt(
    columns = c(diff_5, diff_10, diff_15, diff_20, diff_25, diff_30, diff_35, diff_40, diff_t),
    fns = function(x) ifelse(x > 0, paste0("+", x), as.character(x))
  ) |>
  gt::data_color(
    columns = c(diff_5, diff_10, diff_15, diff_20, diff_25, diff_30, diff_35, diff_40, diff_t),
    colors = scales::col_bin(
      bins    = c(-Inf, 0, Inf),
      palette = c("#ffe7e7", "#d0e4f3")
    )
  ) |>
  gt::cols_align(columns = team_lookup, align = "left") |>
  gt::tab_style(
    style = list(gt::cell_borders(
      sides  = c("left", "right", "top", "bottom"),
      color  = "black",
      weight = gt::px(2)
    )),
    locations = list(
      gt::cells_body(),
      gt::cells_column_labels(),
      gt::cells_row_groups(),
      gt::cells_column_spanners()
    )
  ) |>
  gt::tab_style(
    style     = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_spanners()
  ) |>
  gt::tab_style(
    style     = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(columns = diff_t)
  ) |>
  gt::tab_header(
    title    = gt::html("North Carolina: Point differential by 5-minute segment<br> of game time over the last 9 games"),
    subtitle = "Carolina is 5-4 over its last nine games."
  ) |>
  gtUtils::gt_538_caption(
    "Data via {bigballR} | theme via {gtUtils} and logos from ESPN.",
    "<b>Table by Chris at Bless your chart | March 13, 2026</b>"
  ) |>
  gtUtils::gt_border_bars_bottom(c("#56a0d3", "#89BDE0", "#BBD9ED")) |>
  gt::tab_options(table.width = gt::px(725)) |>
  gt::cols_hide(c(
    dplyr::starts_with("UNC_Points_In_Span_"),
    dplyr::starts_with("Opponent_Points_In_Span_"),
    "UNC_total", "Opponent_total", "sort_order"
  ))

gt_save_crop(diff_tbl_a,
             file = "diff_tbl_a.png",
             whitespace = 60,
             bg = "white")
