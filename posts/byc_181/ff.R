
f4_2026 <- c("Connecticut", "UConn", "Illinois", "Arizona", "Michigan")

bball_header <- glue::glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
      <div>
       <img src='https://a.espncdn.com/combiner/i?img=/redesign/assets/img/icons/ESPN-icon-basketball.png'
       style='height: 40px; width: auto; vertical-align: middle;'>
     </div>
     <div style='flex-grow:1; margin-left: 30px; margin-right: 30px'>
       <span style='display: block; font-weight: bold; text-align: center; font-size: 24px;'>2026 Final Four</span>
       <span style='font-size: 14px; font-weight: normal; display: block; text-align: center;'><br>
       Highlights every Final Four appearance for the teams in this season's Final Four since the 1985 NCAA Tournament.</span>
     </div>
     <div>
       <img src='https://cdn.ibj.com/wp-content/uploads/2024/12/Final_Four_Logo_2026_D3.jpg'
       style='height: 60px; width: auto; vertical-align: middle;'>
     </div>
   </div>
  <br>"
)


# 3. Create the GT Table
ncaa_ff_split |> 
  cbbplotR::gt_cbb_teams(champ_a, champ_a, include_name = FALSE) |> 
  cbbplotR::gt_cbb_teams(champ_b, champ_b, include_name = FALSE) |> 
  cbbplotR::gt_cbb_teams(team1_a, team1_a, include_name = FALSE) |> 
  cbbplotR::gt_cbb_teams(team2_a, team2_a, include_name = FALSE) |> 
  cbbplotR::gt_cbb_teams(team3_a, team3_a, include_name = FALSE) |> 
  cbbplotR::gt_cbb_teams(team1_b, team1_b, include_name = FALSE) |> 
  cbbplotR::gt_cbb_teams(team2_b, team2_b, include_name = FALSE) |> 
  cbbplotR::gt_cbb_teams(team3_b, team3_b, include_name = FALSE) |> 
  gt::gt() |> 
  # Handle 2020 Canceled row
  gt::text_transform(
    locations = gt::cells_body(columns = c(champ_a, team1_a, team2_a, team3_a), rows = year_a == 2020),
    fn = function(x) "---"
  ) |> 
  # HIGHLIGHT LOGIC: Use the original data to find row indices
  gt::tab_style(
    style = list(
      gt::cell_fill(color = "#fff3cd"), 
      gt::cell_borders(sides = "all", color = "#ffc107", weight = px(1.5), style = "solid")
    ),
    locations = list(
      gt::cells_body(columns = champ_a, rows = which(ncaa_ff_split$champ_a %in% f4_2026)),
      gt::cells_body(columns = team1_a, rows = which(ncaa_ff_split$team1_a %in% f4_2026)),
      gt::cells_body(columns = team2_a, rows = which(ncaa_ff_split$team2_a %in% f4_2026)),
      gt::cells_body(columns = team3_a, rows = which(ncaa_ff_split$team3_a %in% f4_2026)),
      gt::cells_body(columns = champ_b, rows = which(ncaa_ff_split$champ_b %in% f4_2026)),
      gt::cells_body(columns = team1_b, rows = which(ncaa_ff_split$team1_b %in% f4_2026)),
      gt::cells_body(columns = team2_b, rows = which(ncaa_ff_split$team2_b %in% f4_2026)),
      gt::cells_body(columns = team3_b, rows = which(ncaa_ff_split$team3_b %in% f4_2026))
    )
  ) |> 
  gtUtils::gt_theme_savant() |> 
  gt::tab_header(title = gt::html(bball_header)) |> 
  gt::cols_align(align = "center") |> 
  gt::fmt_markdown(columns = everything()) |> 
  gt::cols_add(diff_gap = "", .before = year_b) |>
  gt::cols_label(
    year_a = "Year", champ_a = "", team1_a = "", team2_a = "", team3_a = "",
    diff_gap = "",
    year_b = "Year", champ_b = "", team1_b = "", team2_b = "", team3_b = ""
  ) |> 
  gt::tab_source_note(gt::html(
    "<hr>
     data via wikipedia.com and ncaa.com<br>
     First team listed is the NCAA Champion, not including 2026<br>
     <hr>
     <span style='display:inline-block; background:#fff3cd; border:1px solid #ffc107;
       border-radius:4px; padding:1px 3px; font-size:12px;'>Yellow indicates team in the 2026 Final Four</span><br><br>
     <hr>
     April 1, 2026 | Table by Chris at Bless your Chart"
  )) |> 
  gt::tab_style(
    style = list(gt::cell_borders(
      sides = c("left", "right", "top", "bottom"),
      color = "black",
      weight = gt::px(2)
    )),
    locations = list(
      gt::cells_body(),
      gt::cells_column_labels(),
      gt::cells_row_groups()
    )
  ) |> 
  gtUtils::gt_border_bars_bottom(c("#0d1e2d", "#c1d6e2",
                                   "#f07e1d"))
