du_header <- glue::glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
      <div>
       <img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/153.png'
       style='height: 45px; width: auto; vertical-align: middle;'>
     </div>
     <div style='flex-grow:1; margin-left: 30px; margin-right: 30px'>
       <span style='display: block; font-weight: bold; text-align: center; font-size: 24px;'>Last 20 Carolina-Duke games at Cameron Indoor Stadium</span>
       <span style='font-size: 14px; font-weight: normal; display: block; text-align: center;'>Carolina is 10-10 at Duke since 2006<br>Duke has been favored in 16 of those 20 games</span>
     </div>
     <div>
       <img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/150.png'
       style='height: 40px; width: auto; vertical-align: middle;'>
     </div>
   </div>
  <br>"
)


ps |>
  dplyr::filter(Year > 2005) |>  
  cbbplotR::gt_cbb_teams(winner, winner, include_name = FALSE) |> 
  dplyr::arrange(dplyr::desc(Year)) |>
  dplyr::mutate(
    unc_favored = dplyr::if_else(Spread < 0, Spread, NA_real_),
    unc_dog     = dplyr::if_else(Spread > 0, Spread, NA_real_)
  ) |>
  dplyr::mutate(
    # Reorder score so higher number is always first
    Score = purrr::map_chr(Score, function(s) {
      parts <- as.integer(stringr::str_split_1(s, "-"))
      paste0(max(parts), "-", min(parts))
    }),
  ) |>
  dplyr::mutate(outcome = paste0(winner, " ", Score)) |> 
  dplyr::select(Year, Date, unc_favored, outcome, unc_dog) |>
  gt::gt() |>
  gtUtils::gt_theme_savant() |> 
  gt::fmt_markdown(outcome) |> 
  gtExtras::gt_merge_stack(
    col1 = "Year",
    col2 = "Date",
    palette = c("black", "#333333")
  ) |> 
  gt::cols_label(
    Year        = "",
    unc_favored = "UNC -",
    outcome      = "Result",
    unc_dog     = "UNC +"
  ) |>
  gt::tab_header(title = gt::html(du_header)) |>
  gt::tab_source_note(
    source_note =
      gt::html(
        "<hr>Data from TeamRankings BetIQ and theme for {gtUtils}<br>
                <hr>
                <b>Overall series since 2005-06:</b><br>
                Carolina is 22-25 overall against Duke since the 2005-06 season<br>
                UNC is 2-4 on a neutral floor (1-4 ACC Tournament & 1-0 in NCAA Tournament)<br>
                Carolina is 10-11 overall at home or in Chapel Hill<br>
                <hr><b>Table by Chris at Bless your chart | March 6, 2026</b>"
      )
  ) |>
  gt::text_transform(
    locations = gt::cells_body(columns = unc_favored),
    fn = function(x) {
      spread_vals <- as.numeric(x)
      max_abs <- max(abs(ps$Spread), na.rm = TRUE)
      
      purrr::map_chr(spread_vals, function(val) {
        if (is.na(val)) return("")
        bar_pct <- round((abs(val) / max_abs) * 100)
        glue::glue(
          '<div style="display:flex;align-items:center;justify-content:flex-end;width:100%;">',
          '<span style="margin-right:5px;font-size:11px;color:#333;">{val}</span>',
          '<div style="width:{bar_pct}%;height:14px;background:#56a0d3;border-radius:3px 0 0 3px;"></div>',
          '</div>'
        )
      })
    }
  ) |>
  gt::text_transform(
    locations = gt::cells_body(columns = unc_dog),
    fn = function(x) {
      spread_vals <- as.numeric(x)
      max_abs <- max(abs(ps$Spread), na.rm = TRUE)
      
      purrr::map_chr(spread_vals, function(val) {
        if (is.na(val)) return("")
        bar_pct <- round((abs(val) / max_abs) * 100)
        glue::glue(
          '<div style="display:flex;align-items:center;width:100%;">',
          '<div style="width:{bar_pct}%;height:14px;background:#003087;border-radius:0 3px 3px 0;"></div>',
          '<span style="margin-left:5px;font-size:11px;color:#333;">+{val}</span>',
          '</div>'
        )
      })
    }
  ) |> 
  gt::tab_options(
    table.font.size = gt::px(14),
    heading.align = "left",
    column_labels.font.weight = "bold",
    row_group.font.size = gt::px(13),
    row_group.background.color = "#f5f5f5"
  ) |> 
  gt::tab_style(
    style = list(gt::cell_borders(
      sides = c("left", "right", "top", "bottom"),
      color = "black",
      weight = gt::px(2)  # Make it thicker than internal borders
    )),
    locations = list(
      gt::cells_body(),
      gt::cells_column_labels(),
      gt::cells_row_groups()
    )
  ) |> 
  gt::cols_align(columns = c(unc_favored, unc_dog, outcome), align = "center") |> 
  gt::tab_options(table.width = gt::px(450)) -> ud 

gt_save_crop(ud,
             file = "duke_unc_06.png",
             whitespace = 60,
             bg = "white")
