# NOTRE DAME DATA
get_notre_dame_stats <- function(start_year, end_year) {
  years <- start_year:end_year
  all_stats <- list()
  
  for (year in years) {
    stats <- cfbfastR::cfbd_game_team_stats(year, team = "Notre Dame")
    stats$year <- year  # Add a new column for the year
    all_stats[[as.character(year)]] <- stats
  }
  
  combined_stats <- dplyr::bind_rows(all_stats)
  
  return(combined_stats)
}

# Specify the range of years
start_year <- 2014
end_year <- 2022

# Get and combine game statistics for Notre Dame for the specified years
nd_stats <- get_notre_dame_stats(start_year, end_year)

# Sort the combined game statistics by year
nd_results <- nd_stats |> 
  dplyr::add_row(year = 2014, school = "Notre Dame", opponent = "LSU",
                 opponent_conference = "SEC", points = 31, points_allowed = 28) |> 
  dplyr::add_row(year = 2015, school = "Notre Dame", opponent = "Ohio State",
                 opponent_conference = "Big Ten", points = 28, points_allowed = 44) |> 
  dplyr::add_row(year = 2017, school = "Notre Dame", opponent = "LSU",
                 opponent_conference = "SEC", points = 21, points_allowed = 17) |> 
  dplyr::add_row(year = 2018, school = "Notre Dame", opponent = "Clemson", home_away = "neutral",
                 opponent_conference = "ACC", points = 3, points_allowed = 30) |> 
  dplyr::add_row(year = 2019, school = "Notre Dame", opponent = "Iowa State",
                 opponent_conference = "Big 12", points = 33, points_allowed = 9) |> 
  dplyr::add_row(year = 2020, school = "Notre Dame", opponent = "Alabama",
                 opponent_conference = "SEC", points = 14, points_allowed = 31) |> 
  dplyr::add_row(year = 2021, school = "Notre Dame", opponent = "Oklahoma State",
                 opponent_conference = "Big 12", points = 35, points_allowed = 37) |> 
  dplyr::add_row(year = 2022, school = "Notre Dame", opponent = "South Carolina",
                 opponent_conference = "SEC", points = 45, points_allowed = 38) |> 
  dplyr::arrange(year) |> 
  dplyr::select(year, school, opponent, opponent_conference, home_away, points, points_allowed) |> 
  dplyr::mutate(diff = points -points_allowed) |> 
  dplyr::mutate(result = dplyr::if_else(diff > 0, "W", "L"))


only_acc <- nd_results |> 
  dplyr::filter(opponent_conference == "ACC") |> 
  dplyr::mutate(score = paste(points, points_allowed, sep = "-")) |> 
  dplyr::mutate(home_away = dplyr::case_match(home_away, 
                      "home" ~ "H",
                      "away" ~ "A", 
                      "neutral" ~ "N")) |> 
  dplyr::select(year, opponent, home_away, result, score, diff)

# make the tenure table! 
only_acc |> 
  dplyr::slice(1:25) |> 
  gt::gt() |> 
  gt::cols_label(
    opponent = "Opponent",
    home_away = "Location",
    result = "Result", 
    score = "Score",
    diff = "+/-"
  ) |> 
  gt::data_color(# Update cell colors...
    columns = c(result),
    # ...for supp column!
    colors = scales::col_factor(
      # <- bc it's a factor
      palette = c("#ffffff", "#ffe8a2"),
      # Two factor levels, two colors
      domain = c("L", "W")# Levels
    )) |> 
  gt::fmt(
    columns = c(diff),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) |> 
  cfbplotR::gt_fmt_cfb_logo(columns = c("opponent")) |> 
  gt_theme_athletic()-> first_tab

only_acc |> 
  dplyr::slice(26:50) |> 
  gt::gt() |> 
  gt::cols_label(
    opponent = "Opponent",
    home_away = "Location",
    result = "Result", 
    score = "Score",
    diff = "+/-"
  ) |> 
  cfbplotR::gt_fmt_cfb_logo(columns = c("opponent")) |> 
  gtExtras::gt_highlight_rows(
    rows = c(1, 16),
    fill = "#ffdddd",
    bold_target_only = TRUE,
    target_col = c(result),
  ) |>
  gt::data_color(# Update cell colors...
    columns = c(result),
    # ...for supp column!
    colors = scales::col_factor(
      # <- bc it's a factor
      palette = c("#ffffff", "#ffe8a2"),
      # Two factor levels, two colors
      domain = c("L", "W")# Levels
    )) |> 
  gt::fmt(
    columns = c(diff),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) |> 
  gt_theme_athletic()-> second_tab


listed_tables <- list(first_tab, second_tab)

obj <- htmltools::div(
  style = "padding: 20px;",
  gt::html("<span style='font-size:15pt; font-weight:bold; font-family: \"Spline Sans Mono\", monospace;'><center>Notre Dame is 41-9 against the ACC since 2014</center></span>"),
  gt::html("<span style='font-size:12pt; font-weight:normal; font-family: \"Spline Sans Mono\", monospace;'><center>2014 is start of scheduling agreement, post-season games highlighted in red.</center></span>"),
  style = "<br>",
  gt::html("<span style='font-size:9.5pt; font-weight:light; font-family: \"Spline Sans Mono\", monospace;'><center>Bless your chart | data via cfbfastR </center></span>"),
  gtExtras::gt_two_column_layout(listed_tables)
)

gtExtras::gtsave_extra(obj, filename = "domers.png", vheight = 900, vwidth = 825)



all_nd <- nd_results |> 
  dplyr::group_by(opponent) |> 
  dplyr::summarize(
    W = sum(result == "W"),
    L = sum(result == "L"),
    `+/-` = sum(diff),
    total_gs = W + L
  ) |> 
  dplyr::arrange(total_gs) |> 
  dplyr::group_by(total_gs) |> 
  dplyr::mutate(row_num = dplyr::row_number() - 1) |> 
  dplyr::ungroup()
