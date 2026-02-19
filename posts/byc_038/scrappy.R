#


tibble::tribble( 
~team, ~change, ~type, 

  )








opp_totals <- cdata |> 
  dplyr::group_by(team_name) |> 
  dplyr::summarise(opp_win_total = sprintf("%.1f", mean(win_total, na.rm = TRUE)))

team_totals <- cdata |> 
  dplyr::distinct(opponent, .keep_all = TRUE) |> 
  dplyr::select(opponent, win_total) |> 
  dplyr::rename(team_name = opponent, team_win_total = win_total) |> 
  dplyr::filter(!is.na(team_win_total))

# merge this data 
wt_all <- merge(opp_totals, team_totals, by = "team_name") 


wt_all |> 
  ggplot2::ggplot(ggplot2::aes(x = team_win_total, y = opp_win_total)) + 
  cfbplotR::geom_cfb_logos(ggplot2::aes(team = team_name), alpha = 0.5, width = 0.045) + 
  cfbplotR::geom_median_lines(ggplot2::aes(v_var = team_win_total, h_var = opp_win_total, color = "lightgreen")) +
  ggplot2::scale_x_continuous(breaks = seq(0, 12, 0.5), limits = c(1,12)) + 
  ggplot2::scale_y_continuous(breaks = seq(0, 8, 0.5), limits = c(5,7.5)) + 
  ggthemes::theme_solarized() + 
  ggplot2::theme(legend.position = "none") + 
  ggplot2::labs(
    x = "Team win total",
    y = "Average opponent win total",
    title = "2023 Team Win Totals and Average Opponent Win Total",
    subtitle = "Win totals are composite numbers from vegasinsiders.com",
    caption = "Bless your chart | July 14, 2023 | data via cfbplotR and vegasinsider.com"
  ) -> my_plot
