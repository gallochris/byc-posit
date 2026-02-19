url <- "https://www.bcftoys.com/2024-wfei"


webpage <- rvest::read_html(url)

# organize table
fei <- webpage |>
  rvest::html_node("table") |>
  rvest::html_table() |>
  dplyr::slice(-1)

clean_fei <- function(data) {
  # Find rows with team names (they typically contain the year)
  team_indices <- grep("[0-9]{4}", data$X1)
  
  # Process the data
  cleaned_data <- lapply(team_indices, function(i) {
    team_name <- gsub(" [0-9]{4}.*", "", data$X1[i])  # Extract team name
    
    # Get the next set of rows until the metrics end
    next_rows <- data[(i + 2):(i + 15),]  # Adjust range if needed
    
    # Filter out NA rows and empty rows
    next_rows <- next_rows[!is.na(next_rows$X2) & next_rows$X2 != "", ]
    
    # Create a data frame for this team
    next_rows |>
      dplyr::select(
        week = X1,
        record = X2,
        conf_record = X3,
        fei = X5,
        fei_rank = X6,
        ofei = X7,
        ofei_rank = X8,
        dfei = X9,
        dfei_rank = X10,
        sfei = X11,
        sfei_rank = X12
      ) |>
      dplyr::mutate(
        team = team_name,
        week = as.numeric(week),
        fei = as.numeric(fei),
        fei_rank = as.numeric(fei_rank),
        ofei = as.numeric(ofei),
        ofei_rank = as.numeric(ofei_rank),
        dfei = as.numeric(dfei),
        dfei_rank = as.numeric(dfei_rank),
        sfei = as.numeric(sfei),
        sfei_rank = as.numeric(sfei_rank)
      ) |>
      dplyr::select(team, week, fei, fei_rank) |>  # Move team to first column
      dplyr::filter(!is.na(week))  # Remove any remaining NA rows
  })
  
  # Combine all teams' data
  dplyr::bind_rows(cleaned_data)
}

clean_fei(fei) -> progression

ptiles <- progression |> 
  dplyr::group_by(week) |> 
  dplyr::mutate(fei_ptile = dplyr::percent_rank(fei)) |> 
  dplyr::ungroup()

ptiles |>
  dplyr::group_by(team) |>
  dplyr::summarise(
    sd_fei_rank = sd(fei_ptile),
    pre_fei_ptile = fei_ptile[week == 0],
    current_fei_ptile = fei_ptile[week == 9],
    delta_fei_ptile = (pre_fei_ptile - current_fei_ptile)
  ) |>
  dplyr::arrange(-delta_fei_ptile) 


ptiles |>
  dplyr::group_by(team) |>
  dplyr::summarise(
    pre_fei_ptile = fei_ptile[week == 0],
    current_fei_ptile = fei_ptile[week == 9],
    delta_fei_ptile = (pre_fei_ptile - current_fei_ptile)
  ) |>
  dplyr::filter(delta_fei_ptile > .20) |> 
  dplyr::arrange(-delta_fei_ptile) |> 
  dplyr::slice(1:10) -> worst

ptiles |>
  dplyr::group_by(team) |>
  dplyr::summarise(
    pre_fei_ptile = fei_ptile[week == 0],
    current_fei_ptile = fei_ptile[week == 9],
    delta_fei_ptile = (pre_fei_ptile - current_fei_ptile)
  ) |>
  dplyr::filter(delta_fei_ptile < -.25) |> 
  dplyr::arrange(delta_fei_ptile) |> 
  dplyr::slice(1:10) -> best

best_teams <- best |> 
  dplyr::pull(team)

best_range <- ptiles |> 
  dplyr::filter(team %in% best_teams) |> 
  dplyr::group_by(team) |> 
  dplyr::summarise(range_list = list(fei_ptile))

worst_teams <-  worst |> 
  dplyr::pull(team)

worst_range <- ptiles |> 
  dplyr::filter(team %in% worst_teams) |> 
  dplyr::group_by(team) |> 
  dplyr::summarise(range_list = list(fei_ptile))

best_final <- best |> 
  dplyr::left_join(best_range, by = "team") |> 
  dplyr::mutate(category = "Better")

worst_final <- worst |> 
  dplyr::left_join(worst_range, by = "team") |> 
  dplyr::mutate(category = "Worse")

progress_final <- best_final |>
  dplyr::bind_rows(worst_final) |> 
  cbbplotR::gt_cbb_teams(team, team, include_name = FALSE)


progress_final |> 
  dplyr::select(category, 
                pre_fei_ptile, range_list, current_fei_ptile, team) |> 
  dplyr::group_by(category) |>
  gt::gt() |>
  gt::fmt_markdown(team) |>
  gtUtils::gt_theme_gtutils() |> 
  gt::cols_nanoplot(
    columns = range_list,
    autohide = TRUE,
    plot_type = "line",
    before = "current_fei_ptile",
    new_col_label = "Progression",
    reference_line = .50,
    options = gt::nanoplot_options(
      data_area_fill_color = "lightgreen",
      data_line_stroke_color = "#9E9ECC",
      data_point_stroke_color = "floralwhite",
      data_point_fill_color = "#333333",
      data_line_type = "curved",
      reference_line_color = "#333333",
      show_data_area = TRUE,
      show_data_points = FALSE
    )
  ) |> 
  gt::cols_align(columns = c(-team), "center") |>
  gt::cols_align(columns = c(team), "left") |> 
  gtUtils::gt_column_subheaders(
    pre_fei_ptile = list(heading = "Preseason", subtitle = "Percentile"),
    current_fei_ptile = list(heading = "Current", subtitle = "Percentile"),
    heading_color = "black",
    subtitle_color = "gray"
  ) |>
  gt::cols_label(team = "",
                 nanoplots = "Progression") |> 
  gtUtils::gt_color_pills(
    pre_fei_ptile,
    domain = c(0, 1),
    format_type = "percent",
    digits = 1,
    palette = c("#8B0000", "#FF0000", "#FF4500", "#ADFF2F", "#008000")
  ) |> 
  gtUtils::gt_color_pills(
    current_fei_ptile,
    domain = c(0, 1),
    format_type = "percent",
    digits = 1,
    palette = c("#8B0000", "#FF0000", "#FF4500", "#ADFF2F", "#008000")
  ) |> 
  gtUtils::gt_border_grid(color = "black",
                          weight = 0.5,
                          include_labels = FALSE) |> 
  gt::tab_style(
    locations = gt::cells_column_labels(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      weight = 850,
      size = gt::px(15)
    )
  ) |>
  gt::tab_style(
    locations = gt::cells_source_notes(),
    style = gt::cell_text(
      font = gt::google_font("Signika Negative"),
      size = gt::px(10),
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
        align = "right"
      ),
      gt::cell_fill(
        color = "#C5C5FF"
      )
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
        align = "right"
      ),
      gt::cell_fill(
        color = "#C5C5FF"
      )
    )
  ) |> 
  gt::tab_style(
    locations = gt::cells_column_labels(columns = gt::everything()),
    style = list(
      gt::cell_text(
        font = gt::google_font("Signika Negative"),
        weight = 650,
        size = gt::px(12),
        color = "#333333"
      )
    )
  ) |> 
  gt::tab_header(
    title = "FEI ratings progression from preseason to week 9",
    subtitle = gt::md("Uses the percentile from FEI ratings to teams progression since
                      the start of the season. The dotted line is the 50th percentile.")
  ) |> 
  gtUtils::gt_border_bars_bottom("#C5C5FF",
                        bar_height = 45,
                        img = "https://a.espncdn.com/combiner/i?img=/redesign/assets/img/icons/ESPN-icon-football-college.png",
                        text = gt::html("Data from bcftoys.com + theme from {gtUtils} <br> 
                                        Table by Chris at Bless your chart"),
                        img_width = 30, img_height = 30, text_size = 10, text_weight = "normal",
                        text_color = "black"
  ) |> 
  gt::tab_options(table.width = gt::px(450))
  




diffs <- best |>
  dplyr::bind_rows(worst) |> 
  dplyr::mutate(color = dplyr::if_else(delta_fei_ptile < 0, 
                                       "lightgreen", "lightpink")) |> 
  dplyr::rename(pone = pre_fei_ptile, ptwo = current_fei_ptile) |> 
  tidyr::pivot_longer(cols = c(pone, ptwo), names_to = "diff", 
                      values_to = "value") 

ggplot2::ggplot(diffs, ggplot2::aes(x = diff, y = value, group = team, color = color)) +
  ggplot2::geom_line(alpha = 0.6, linewidth = 1.2) +
  cfbplotR::geom_cfb_logos(ggplot2::aes(team = team, y = value), 
                           vjust = .5, width = 0.038, alpha = 0.6) +
  ggplot2::scale_color_manual(values = c("lightpink", "lightgreen")) +
  ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.10)) +
  theme_me()
