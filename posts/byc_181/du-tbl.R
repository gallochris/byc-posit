du_header <- glue::glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
      <div>
        <img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/41.png'
       style='height: 45px; width: auto; vertical-align: middle;'>
     </div>
     <div style='flex-grow:1; margin-left: 30px; margin-right: 30px'>
       <span style='display: block; font-weight: bold; text-align: center; font-size: 24px;'>UConn 73, Duke 72<br>Possessions over last 5 minutes</span>
       <span style='display: block; font-weight: bold; text-align: center; font-size: 12px; text-align: center;'>UConn outscored Duke 15-5 over the last five minutes.</span>
       <span style='font-size: 12px; font-weight: normal; display: block; text-align: center;'><br>March 29, 2026 |  Capital One Arena Washington D.C.</span>
     </div>
     <div>
        <img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/150.png'
       style='height: 40px; width: auto; vertical-align: middle;'>
     </div>
   </div>"
)

du <- tibble::tibble(
  team = c("Connecticut", "Duke"),
  p1   = c(2L,  0L),
  p2   = c(2L,  0L),
  p3   = c(3L,  2L),
  p4   = c(0L,  0L),
  p5   = c(0L,  1L),
  p6   = c(1L,  0L),
  p7   = c(3L,  2L),
  p8   = c(1L,  0L),
  p9   = c(3L,  NA_integer_),
  f  = c(15L, 5L)
)
du |> 
  cbbplotR::gt_cbb_teams(team, team, include_name = FALSE)  |>
  gt::gt()  |>
  gt::fmt_markdown(columns = gt::everything()) |> 
  gtExtras::gt_theme_dot_matrix() |>
  gt::tab_options (
    source_notes.font.size = gt::px(10),
    row.striping.background_color = '#ffffff',
    column_labels.text_transform = 'none',
    column_labels.font.weight = 'bold',
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "black"
  ) |>
  gtExtras::gt_highlight_rows(
    rows = c(1),
    columns = c(2, 3, 4, 7, 8, 9, 10, 11),
    fill =  "#e4002b",
    alpha = 0.6
  ) |>
  gtExtras::gt_highlight_rows(
    rows = c(2),
    columns = c(4, 6, 8),
    fill = "#00539B",
    alpha = 0.6
  ) |> 
  gt::sub_missing(missing_text = "---") |> 
  gt::cols_align(columns = -c(team), align = "center") |> 
  gtUtils::gt_border_grid(color = "black",
                          weight = 0.5,
                          include_labels = FALSE) |>
  gt::tab_style(
    style = gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(0.5)
    ),
    locations = gt::cells_body(rows = 1)
  ) |> 
  gt::tab_header(title = gt::html(du_header)) |>
  gt::tab_source_note(
    source_note =
      gt::html(
        "<b>UConn advances to 2026 Final Four</b> | <b>Season records</b>: UConn 33-5 | Duke 35-3<br><br>
        <hr>
        <b>UConn Last five minutes</b>:<br>
        <b>3PTM-3PTA</b>: 2-3 | <b>2PTM-2PTA</b>: 3-5 | <b>FTM-FTA</b>: 3-5<br>
        <b>Rebounds</b>: 5 DR, 1 OR | <b>Turnovers</b>: 0<br>
        <b>Blocks</b>: 1 | <b>Steals:</b> 3<br>
        <hr>
        <b>Duke Last five minutes</b>:<br>
        <b>3PTM-3PTA</b>: 0-0 | <b>2PTM-2PTA</b>: 2-4 | <b>FTM-FTA</b>: 1-2<br>
        <b>Rebounds</b>: 2 DR, 0 OR | <b>Turnovers</b>: 3<br>
        <b>Blocks</b>: 0 | <b>Steals:</b> 0<br>
        <hr style='border-color: black;'><b>Data via statbroadcast.com | Table by Chris at Bless your chart</b>"
      )
  ) |> 
  gt::cols_label_with(fn = ~ gsub("^p", "", .x)) |> 
  gt::cols_label(
    team = "",
    f = ""
  ) |> 
  gt::cols_width(
    team ~ gt::px(50),
    tidyselect::matches("^p[0-9]+$") ~ gt::px(55),
    f ~ gt::px(65)
  ) |> 
  gtExtras::gt_add_divider(columns = c(10), sides = "right",
                           color = "black", weight = 4.5) |> 
  gtUtils::gt_border_bars_bottom(c("#000e2f", 
                                   "white", "#7C878E", "#e4002b"))  -> du_tbl 

gt_save_crop(
  du_tbl,
  file = "du_tbl.png",
  whitespace = 40,
  bg = "white"
)



