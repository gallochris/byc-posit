drive_plot <- function(game_id, offense, defense) {
  # theme 
  theme_me <- function() {
    # Create a base theme with minimal style
    base_theme <- ggplot2::theme_minimal(base_size = 10, base_family = "RobotoCondensed-Regular")
    
    # Customize the base theme with additional modifications
    custom_theme <- base_theme +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = 24,
          face = "bold"
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5,
          size = 10,
          lineheight = 0.25,
          vjust = -0.5
        ),
        plot.caption = ggplot2::element_text(
          hjust = 1,
          size = 6,
          lineheight = 0.35,
          margin = ggplot2::margin(t = 20)
        ),
        panel.grid.minor = ggplot2::element_line(color = "gray", linetype = "dotted"),  # Customize minor grid lines
        plot.background = ggplot2::element_rect(fill = "floralwhite", color = "floralwhite")
      )
    
    return(custom_theme)
  } 
  
  # Load your data (assuming your data frame is named 'your_data')
  # Modify this part to match your actual data structure and loading process
  drive_data <- drives
  
  # Filter data for the specified game_code, team_name, and opponent
  drive_data <- drive_data[drive_data$game_id == game_id & 
                             (drive_data$offense == offense | drive_data$offense == defense), ]
  
  scoring_title <- drive_data$score_sentence[1] 
  
  pivot_drives <- drive_data |> 
    dplyr::filter(!drive_result == "END OF HALF") |> 
    tidyr::pivot_longer(cols = c(offense, defense),
                        names_to = "matchup", values_to ="team") |> 
     dplyr::mutate(start_spot = dplyr::if_else(
      team == offense, start_yards_to_goal,
      start_yardline
    ),
    end_spot = dplyr::if_else(
      team == offense, end_yards_to_goal,
      end_yardline
    )) |> 
   dplyr::distinct(drive_number, .keep_all = TRUE) 
  
  only_team_name <- pivot_drives |> 
    dplyr::filter(team == offense)
  only_opponent <- pivot_drives |> 
    dplyr::filter(team == defense)
  # colors 
  teams_csv <- readr::read_csv("https://raw.githubusercontent.com/CFBNumbers/logos/main/cfblogos.csv",skip = 1,col_names = c("row","team_id","school","mascot","abbreviation","alt_name1","alt_name2","alt_name3","conference","division","color","alt_color","logo","logo_dark"))
  
  team_colors <- teams_csv |> 
    dplyr::select(school, abbreviation, color, alt_color) |> 
    dplyr::mutate(school = dplyr::if_else(stringr::str_detect(school,"^San Jos"),"San José State",school)) |> 
    dplyr::filter(school %in% cfbplotR::valid_team_names()) |> 
    tidyr::replace_na(replace = list(color = "#ffffff",alt_color = "#ffffff"))
  
  team_color <- team_colors$color[team_colors$school == only_team_name$team[1]]
  
  opponent_color <- team_colors$color[team_colors$school == only_opponent$team[1]]
  
  #max limits 
  max_drive_num <- max(pivot_drives$drive_number)
  x_limit <- max_drive_num + 4
  
  # caption 
  
  caption <- "<span style='font-family: mono; fontface: bold'> Excludes garbage time drives<br><br>
Bless your chart | data via cfbfastR</span>" 
  
  # Create the ggplot based on the filtered data
  ggplot2::ggplot(pivot_drives) +
    ggplot2::geom_segment(ggplot2::aes(x = drive_number, xend = drive_number, y = start_spot,
                                       yend = end_spot, color = team), linewidth = 1.5,
                          arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(0.07, "inches"))) +
    ggplot2::scale_y_reverse(limits = c(120, -20), breaks = seq(-20, 120, 10),
                             labels = rev(c("", "", "0", "10", "20", "30", "40", "50",
                                            "40", "30", "20", "10", "0", "", ""))) +
    ggplot2::scale_x_reverse(limits = c(x_limit, -5), breaks = seq(x_limit, -5, -1), 
                             expand = c(0, -0.5)) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c(team_color, opponent_color)) +
    theme_me() + 
    ggplot2::theme(legend.position = "none", 
                   plot.title = ggtext::element_markdown(),
                   plot.caption = ggtext::element_markdown(),
                   panel.grid.major.x = ggplot2::element_line(color = "#E6E6E6", size = 0.5), 
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(), 
                   panel.grid.minor.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = 14, face = "bold")) +
    ggplot2::geom_hline(yintercept = c(-10, -20, 110, 120), color = "floral white") +
    ggplot2::labs(x = "",
                  y = "Yard line",
                  title = "",
                  caption = caption) +
    ggplot2::annotate(
      geom = "label",
      x = only_team_name$drive_number,
      y = 114,
      label =  paste(only_team_name$drive_result, "\n",
                     only_team_name$plays, "plays", 
                     only_team_name$yards, "yards"),
      color = "#333333",
      fill = team_color,
      size = 3,
      fontface = 'bold',
      family = 'mono',
      alpha = .3,
      hjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "label",
      x = only_opponent$drive_number,
      y = -14,
      label =  paste(only_opponent$drive_result, "\n",
                     only_opponent$plays, "plays", 
                     only_opponent$yards, "yards"),
      color = "#333333",
      fill = opponent_color,
      size = 3,
      fontface = 'bold',
      family = 'mono',
      alpha = .3,
      hjust = 0.5
    ) +
    ggplot2::annotate(
      cfbplotR::GeomCFBlogo,
      x = -2.5,
      y = 114,
      team = only_team_name$team,
      height = .075,
    ) +
    ggplot2::annotate(
      cfbplotR::GeomCFBlogo,
      x = -2.5,
      y = -14,
      team = only_opponent$team,
      height = .075,
    ) +
    ggtext::geom_richtext(
      x = 2.8,
      y = -50,
      label = scoring_title,
      fill = "floral white", 
      label.color = NA,
      family = 'mono',
      size = 6,
      label.padding = ggplot2::unit(c(2.7,2.5,2.7,2.5), "lines")
    ) + 
    ggplot2::annotate(
      geom = "text",
      x = 0,
      y = c(100, 90, 80, 70, 60, 50, 40, 30, 20, 10, 0),
      label =  c("0", "10", "20", "30", "40", "50",
                 "40", "30", "20", "10", "0"),
      color = "#333333",
      size = 4,
      fontface = 'bold',
      family = 'mono',
      hjust = 0.5
    ) +
    ggplot2::annotate(
      geom = "text",
      x =  max_drive_num + 2,
      y = c(100, 90, 80, 70, 60, 50, 40, 30, 20, 10, 0),
      label =  c("0", "10", "20", "30", "40", "50",
                 "40", "30", "20", "10", "0"),
      color = "#333333",
      size = 4,
      fontface = 'bold',
      family = 'mono',
      hjust = 0.5
    )
}
