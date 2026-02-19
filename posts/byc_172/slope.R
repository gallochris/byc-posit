wabnc <- wab_2025 |> 
  dplyr::filter(game_type == "played") |> 
  dplyr::group_by(team) |> 
  dplyr::summarise(
    wab_non_conf = sum(wab[type == "nc"], na.rm = TRUE),
    wab_conf = sum(wab[type == "conf"], na.rm = TRUE)
  ) |> 
  dplyr::mutate(diff = abs(wab_non_conf - wab_conf))

wabnc_long <- wabnc |>
  tidyr::pivot_longer(
    cols = c(wab_non_conf, wab_conf),
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = factor(metric, levels = c("wab_non_conf", "wab_conf"))
  )

# Define the two groups
group1_teams <- c("St. John's", "Texas A&M", "N.C. State", "Wisconsin")

group2_teams <- c("BYU", "Alabama", "North Carolina", "Oklahoma St.")

wabnc_long <- wabnc |>
  tidyr::pivot_longer(
    cols = c(wab_non_conf, wab_conf),
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = factor(metric, levels = c("wab_non_conf", "wab_conf")),
    highlight = dplyr::case_when(
      team %in% group1_teams ~ "group1",
      team %in% group2_teams ~ "group2",
      TRUE ~ "other"
    )
  )

wabnc_long |>
  ggplot2::ggplot(ggplot2::aes(x = metric, y = value, group = team)) +
  ggplot2::geom_line(ggplot2::aes(color = highlight, size = highlight, alpha = highlight)) +
  ggplot2::geom_point(ggplot2::aes(color = highlight, size = highlight, alpha = highlight)) +
  ggplot2::scale_color_manual(
    values = c("group1" = "lightgreen", "group2" = "lightpink", "other" = "gray70")
  ) +
  ggplot2::scale_size_manual(
    values = c("group1" = 1.2, "group2" = 1.2, "other" = 0.4)
  ) +
  ggplot2::scale_alpha_manual(
    values = c("group1" = 1, "group2" = 1, "other" = 0.3)
  ) +
  ggplot2::scale_x_discrete(
    labels = c("wab_non_conf" = "Non-Conference", 
               "wab_conf" = "Conference")
  ) +
  hrbrthemes::theme_ipsum() +
  ggplot2::labs(
    title = "There is more than one way to build a tournament resume",
    subtitle = "Green = More Conference WAB | Orange = More Non-Conference WAB",
    x = "",
    y = "WAB",
    caption = "data via barttorvik.com | Through February 4 games\nViz by Chris at Bless your chart"
  ) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.x = ggplot2::element_blank()
  ) +
  # st. johns 3.19 
  # texas a&M 
  # n.c state 2.36
  # wisco 2.12 
  
  # byu 3.03
  # bama 2.53 
  # unc 2.34
  # okie st 1.41


  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 2.1,
    y = 2.36,
    team = "NC State", # 25 rank 
    height = 0.055,
    alpha = 0.8
  ) 
