tibble::tribble(
  ~opponent,~inn_one,~inn_two,~inn_three,~inn_four,~inn_five,~inn_six,~inn_seven,~inn_eight,~inn_nine,~inn_ten,~total,
  "Long Island",1,0,-3,1,0,3,0,-1,2,0,3,
  "LSU",0,0,0,0,3,0,-1,2,0,0,4,
  "LSU",-2,0,-1,-3,-1,1,2,0,0,0,-4,
  "LSU",1,-1,-1,0,0,0,0,0,1,1,1,
  "West Virginia",0,0,3,-1,0,-4,1,0,3,0,2,
  "West Virginia",1,0,1,0,0,0,-1,0,0,0,1
) -> ncaat_diff


title <- gt_cbb_logo_title(title = 'North Carolina Baseball: Back Home in Omaha',
                           subtitle = 'Shows the run differential by inning for all games during 2024 NCAA Tournament. Carolina its earned 12th trip to Omaha this past weekend.',
                           type = 'team',
                           value = 'North Carolina',
                           logo_height =75)
  
  
  ncaat_diff |> 
  cbbplotR::gt_cbb_teams(opponent, opponent) |> 
  gt::gt() |> 
  gt::fmt_markdown(opponent) |>  
  gt::grand_summary_rows(
    columns = -c(opponent),
    fns = list(Total = ~sum(.x, na.rm = TRUE)),
    fmt = list(~ gt::fmt(., fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }))) |> 
  gt::cols_label(
    opponent = "Opponent",
    inn_one = "1",
    inn_two = "2",
    inn_three = "3",
    inn_four = "4", 
    inn_five = "5",
    inn_six = "6", 
    inn_seven = "7", 
    inn_eight = "8",
    inn_nine = "9", 
    inn_ten = "10",
    total = "Final"
  ) |>
    gt::fmt(
      columns = c(-opponent),
      fns = function(x) {
        ifelse(x > 0, paste0("+", x), x)
      }
    ) |>
  gt::tab_spanner(label = "Innings", 
                  columns = -c(opponent)) |> 
  gtExtras::gt_add_divider(columns = c(opponent, inn_ten), 
                           sides = "right", color = "black") |> 
    gt::data_color(
      columns = -c(opponent),
      colors = scales::col_bin(
        bins = c(-Inf, 0, Inf),
        palette = c("#ffe7e7", "#d0e4f3")
      )
    ) |> 
  cbbplotR::gt_theme_athletic() |>  
    gt::cols_align(align = "left", columns = c(opponent)) |>
  gt::tab_style(
    style = gt::cell_borders(sides = "top", color = 'black', weight = gt::px(1.5), style = 'solid'),
    locations = gt::cells_body(
      rows = gt::everything()
    )
  ) |> 
  gt::tab_options(
    heading.align = "left",
  ) |> 
  gt::tab_style(
    locations = gt::cells_row_groups(groups = gt::everything()),
    style = list(
      gt::cell_text(
        align = "center"
      ),
      gt::cell_borders(sides = c("top", "right"), color = "black", weight = gt::px(2))
    )) |> 
    gt::tab_header(title = gt::html(title)) |>
    gt::tab_source_note(source_note = gt::html("<hr><br>
                                       Data via goheels.com | Logos and table theme from cbbplotR<br>
                                       <hr>
                                       <b>Table by Chris (@dadgumboxscores) + Bless your chart | June 9, 2024</b>"))
  