# ------ 

# function to count by decade 
convert_to_decade <- function(years) {
  paste0(floor(years / 10) * 10, "s")
}

# --------

library(readr)
state <- read_csv("Downloads/state.csv")
View(state)


# manually add in other numbers from sports-refernece and wikipedia 

st_acct <- c(1983, 1987, 2024)

st_rs <- c(1985, 1989)

st_f4 <- c(1983, 2024)

st_titles <- c(1983)



st_decade_counts <- data.frame(
  Decade = unique(c(convert_to_decade(acct), convert_to_decade(rs), convert_to_decade(f4), convert_to_decade(titles)))
) |>
  dplyr::mutate(
    ACCT = sapply(Decade, function(d) sum(convert_to_decade(st_acct) == d)),
    RS = sapply(Decade, function(d) sum(convert_to_decade(rs) == d)),
    F4 = sapply(Decade, function(d) sum(convert_to_decade(f4) == d)),
    Titles = sapply(Decade, function(d) sum(convert_to_decade(titles) == d)),
  ) |>
  dplyr::arrange(Decade)

# summarize the data
state_summary <- state |>
  dplyr::mutate(
    Wins = as.numeric(wins),
    Losses = as.numeric(losses)
  ) |> 
  dplyr::mutate(
    Decade = paste0(floor(Year / 10) * 10, "s")  # Convert Year to decade format
  ) |>
  dplyr::group_by(Decade) |>
  dplyr::summarize(
    Total_Wins = sum(Wins),
    Total_Losses = sum(Losses),
    Win_Percentage = Total_Wins / (Total_Wins + Total_Losses)
  ) |>
  dplyr::arrange(Decade)

# summarize the win percentages in a list 
state_with_decade <- state |>
  dplyr::mutate(
    Decade = paste0(floor(Year / 10) * 10, "s")  # Convert Year to decade format
  ) |>
  dplyr::group_by(Decade) |>
  dplyr::summarize(
    Yearly_Win_Pcts = list(rev(winpct))  # Store win percentages as a list per decade
  ) |>
  dplyr::arrange(Decade)

# all the tables 
state_hist <- state_summary |> 
  dplyr::left_join(decade_counts, by = "Decade") |> 
  dplyr::left_join(state_with_decade, by = "Decade") |> 
  dplyr::mutate(team = "North Carolina St.")

#--------------------------------------------
duke <- readr::read_csv("Downloads/duke.csv")

# manually add in other numbers from sports-refernece and wikipedia 

acct <- c(1980, 1986, 1988, 1992, 1999, 2000, 2001, 2002, 2003, 2005, 2006, 
          2009, 2010, 2011, 2017, 2019, 2023, 2025)

rs <- c(1986, 1991, 1992, 1994, 1997, 1998, 1999, 2000, 2001, 2004, 2006, 
        2010, 2022, 2025)

f4 <- c(1986, 1988, 1989, 1990, 1991, 1992, 1994, 1999, 2001, 2004,
        2010, 2015, 2022, 2025)

titles <- c(1991, 1992, 2001, 2010, 2015)

convert_to_decade <- function(years) {
  paste0(floor(years / 10) * 10, "s")
}

decade_counts <- data.frame(
  Decade = unique(c(convert_to_decade(acct), convert_to_decade(rs), convert_to_decade(f4), convert_to_decade(titles)))
) |>
  dplyr::mutate(
    ACCT = sapply(Decade, function(d) sum(convert_to_decade(acct) == d)),
    RS = sapply(Decade, function(d) sum(convert_to_decade(rs) == d)),
    F4 = sapply(Decade, function(d) sum(convert_to_decade(f4) == d)),
    Titles = sapply(Decade, function(d) sum(convert_to_decade(titles) == d)),
  ) |>
  dplyr::arrange(Decade)

# summarize the data
duke_summary <- duke |>
  dplyr::mutate(
    Wins = as.numeric(wins),
    Losses = as.numeric(losses)
  ) |> 
  dplyr::mutate(
    Decade = paste0(floor(Year / 10) * 10, "s")  # Convert Year to decade format
  ) |>
  dplyr::group_by(Decade) |>
  dplyr::summarize(
    Total_Wins = sum(Wins),
    Total_Losses = sum(Losses),
    Win_Percentage = Total_Wins / (Total_Wins + Total_Losses)
  ) |>
  dplyr::arrange(Decade)

# summarize the win percentages in a list 
duke_with_decade <- duke |>
  dplyr::mutate(
    Decade = paste0(floor(Year / 10) * 10, "s")  # Convert Year to decade format
  ) |>
  dplyr::group_by(Decade) |>
  dplyr::summarize(
    Yearly_Win_Pcts = list(rev(winpct))  # Store win percentages as a list per decade
  ) |>
  dplyr::arrange(Decade)

# all the tables 
duke_hist <- duke_summary |> 
  dplyr::left_join(decade_counts, by = "Decade") |> 
  dplyr::left_join(duke_with_decade, by = "Decade") |> 
  dplyr::mutate(team = "Duke")

# --------------------------------------------- 


all_hist <- unc_hist |> 
  dplyr::bind_rows(state_hist, duke_hist) |> 
  dplyr::relocate(team)


all_hist |> 
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ tidyr::replace_na(.x, 0)
    )
  ) |> 
  dplyr::arrange(Decade, -Titles, -Win_Percentage) |> 
  dplyr::mutate(record = paste0(Total_Wins, "-", Total_Losses)) |> 
  dplyr::relocate(Win_Percentage, .after = Titles) |>
  dplyr::mutate(
    Titles = dplyr::case_when(
      Titles %% 1 == 0 ~ strrep("trophy,", Titles),
      Titles %% 1 != 0 ~ paste0(strrep("trophy,", floor(Titles)), "trophy"))
  ) |> 
  cbbplotR::gt_cbb_teams(team, team, include_name = FALSE) |> 
  dplyr::group_by(Decade) |> 
  gt::gt() |> 
  gt::fmt_markdown(team) |> 
  gtUtils::gt_theme_gtutils() |> 
  gt::fmt_icon(
    columns = c(Titles),
    rows = c(1, 2, 4, 5, 7, 8, 10, 11),
    fill_color = "orange"
  ) |> 
  gt::cols_hide(columns = c(Total_Wins, Total_Losses)) |> 
  gt::fmt_number(Win_Percentage, decimals = 3) |> 
  gtExtras::gt_merge_stack(
    col1 = Win_Percentage,
    col2 = record,
    palette = c("black", "#333333")
  ) |> 
  gtUtils::gt_column_subheaders(
    Win_Percentage = list(heading = "Win %", subtitle = "W-L"),
    ACCT = list(heading = "Tournament", subtitle = "Titles"),
    RS = list(heading = "Regular", subtitle = "Season"),
    F4 = list(heading = "Final Fours", subtitle = "# of times"),
    Titles = list(heading = "Titles", subtitle = "# of titles"),
    heading_color = "black",
    subtitle_color = "gray"
  ) |> 
  gt::cols_label(team = "") |> 
  gt::tab_spanner(
    columns = c(ACCT, RS),
    label = "ACC Championships"
  ) |> 
  gt::tab_spanner(
    columns = c(F4, Titles),
    label = "NCAA Tournament"
  ) |> 
  gt::cols_label(
    Decade = ""
  ) |> 
  gt::cols_nanoplot(
    columns = Yearly_Win_Pcts,
    reference_line = .70,
    autohide = TRUE,
    plot_type = "line",
    new_col_label = "Over time",
    options = gt::nanoplot_options(
      data_area_fill_color = "#ea9999",
      reference_line_color = "#333333",
      show_data_area = TRUE,
      show_data_points = FALSE
    )
  ) |> 
  gt::tab_header(title = "Title") |>
  gt::tab_source_note(source_note = gt::html("<hr>Over time column shows win percentage by season throughout the decade (dashed line indicates a .700 win percentage)<br>Decades include the season's final year (i.e. 2019-20 is part of the 2020s)<hr><br><b>Table by Chris at Bless your Chart | data via sports-reference.com | April 6, 2025</b>")) |> 
  gt::data_color(
    columns = c(ACCT, RS, F4, Win_Percentage),
    direction = c("column"),
    method = c("numeric"),
    palette = c("#d7191c", "#fdae61", 
                "#ffffbf", "#a6d96a", 
                "#1a9641"),
    alpha = 0.6
  ) |>
  gt::cols_align(team, align = "right") |> 
  gt::tab_style(
    locations = gt::cells_column_labels(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      weight = "650",
      size = gt::px(14)
    )
  ) |> 
  gt::tab_style(
    locations = gt::cells_body(columns = Decade),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      weight = "bold"
    )
  ) |> 
  gt::tab_style(
    locations = gt::cells_column_spanners(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      weight = 850,
      size = gt::px(14)
    )
  ) |>
  gt::tab_style(
    locations = gt::cells_source_notes(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      size = gt::px(11.5),
      weight = 250
    )
  ) |>
  gt::tab_style(
    locations = gt::cells_row_groups(),
    style = list(
      gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 850,
        size = gt::px(16),
        color = "black",
        align = "left"
      ),
      gt::cell_fill(
        color = "#C5C5FF"
      )
    )
  ) |> 
  gt::tab_style(
    style = list(
      gt::cell_text(font = gt::google_font("Signika Negative")
      )
    ),
    locations = gt::cells_body(
      columns = c(team)
    )
  ) |> 
  gtUtils::gt_border_grid(color = "black",
                          weight = 2,
                          include_labels = FALSE) |> 
  gtUtils::gt_border_bars_bottom(c("#012169", "#56a0d3", "#CC0000")) |> 
  gt::tab_options(table.width = gt::px(700))


#--------------------------------------------
state_header <- glue::glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
      <div>
       <img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/152.png'
       style='height: 55px; width: auto; vertical-align: middle;'>
     </div>
     <div style='flex-grow:1; margin-left: 30px; margin-right: 30px'>
       <span style='display: block; font-weight: bold; text-align: center; font-size: 24px;'>State hoops over the past five decades</span>
       <span style='font-size: 12.5px; font-weight: normal; display: block; text-align: center;'>Performance across ACC titles, the NCAA Tournament, and overall win percentage.</span>
     </div>
     <div>
       <img src='https://a.espncdn.com/combiner/i?img=/redesign/assets/img/icons/ESPN-icon-basketball.png' 
       style='height: 45px; width: auto; vertical-align: middle;'>
     </div>
   </div>
  <br>"
)


state_hist |> 
  dplyr::mutate(
    dplyr::across(
      where(is.numeric),
      ~ tidyr::replace_na(.x, 0)
    )
  ) |> 
  dplyr::mutate(record = paste0(Total_Wins, "-", Total_Losses)) |> 
  dplyr::relocate(Win_Percentage, .after = Titles) |>
  dplyr::mutate(
    Titles = dplyr::case_when(
      Titles %% 1 == 0 ~ strrep("trophy,", Titles),
      Titles %% 1 != 0 ~ paste0(strrep("trophy,", floor(Titles)), "trophy"))
  ) |> 
  gt::gt() |> 
  gtUtils::gt_theme_gtutils() |> 
  gt::fmt_icon(
    columns = c(Titles),
    rows = c(1),
    fill_color = "orange"
  ) |> 
  gt::cols_hide(columns = c(Total_Wins, Total_Losses)) |> 
  gt::fmt_number(Win_Percentage, decimals = 3) |> 
  gtExtras::gt_merge_stack(
    col1 = Win_Percentage,
    col2 = record,
    palette = c("black", "#333333")
  ) |> 
  gtUtils::gt_column_subheaders(
    Win_Percentage = list(heading = "Win %", subtitle = "W-L"),
    ACCT = list(heading = "Tournament", subtitle = "Titles"),
    RS = list(heading = "Regular", subtitle = "Season"),
    F4 = list(heading = "Final Fours", subtitle = "# of times"),
    Titles = list(heading = "Titles", subtitle = "# of titles"),
    heading_color = "black",
    subtitle_color = "gray"
  ) |> 
  gt::tab_spanner(
    columns = c(ACCT, RS),
    label = "ACC Championships"
  ) |> 
  gt::tab_spanner(
    columns = c(F4, Titles),
    label = "NCAA Tournament"
  ) |> 
  gt::cols_label(
    Decade = ""
  ) |> 
  gt::cols_nanoplot(
    columns = Yearly_Win_Pcts,
    reference_line = .70,
    autohide = TRUE,
    plot_type = "line",
    new_col_label = "Over time",
    options = gt::nanoplot_options(
      data_area_fill_color = "#ea9999",
      reference_line_color = "#333333",
      show_data_area = TRUE,
      show_data_points = FALSE
    )
  ) |> 
  gt::tab_header(title = gt::html(state_header)) |>
  gt::tab_source_note(source_note = gt::html("<hr>Over time column shows win percentage by season throughout the decade (dashed line indicates a .700 win percentage)<br>Decades include the season's final year (i.e. 2019-20 is part of the 2020s)<hr><br><b>Table by Chris at Bless your Chart | data via sports-reference.com | April 6, 2025</b>")) |> 
  gt::data_color(
    columns = c(ACCT, RS, F4, Win_Percentage),
    direction = c("column"),
    method = c("numeric"),
    palette = c("#d7191c", "#fdae61", 
                "#ffffbf", "#a6d96a", 
                "#1a9641"),
    alpha = 0.6
  ) |>
  gt::tab_style(
    locations = gt::cells_column_labels(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      weight = "650",
      size = gt::px(14)
    )
  ) |> 
  gt::tab_style(
    locations = gt::cells_body(columns = Decade),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      weight = "bold"
    )
  ) |> 
  gt::tab_style(
    locations = gt::cells_column_spanners(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      weight = 850,
      size = gt::px(14)
    )
  ) |>
  gt::tab_style(
    locations = gt::cells_source_notes(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      size = gt::px(11.5),
      weight = 250
    )
  ) |>
  gtUtils::gt_border_grid(color = "black",
                          weight = 2,
                          include_labels = FALSE) |> 
  gtUtils::gt_border_bars_bottom(c("#CC0000", "#000000", "#FFFFFF")) |> 
  gt::tab_options(table.width = gt::px(620)) -> stbl 


gtUtils::gt_save_crop(
  stbl,
  file = "st_decade_tbl.png",
  whitespace = 60,
  bg = "#FFFDF5"
)
