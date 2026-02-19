
# ranks <- kp_get_ratings(year = 2026)

ranks |>
  dplyr::mutate(
    tier = dplyr::case_when(
      RankAdjEM <= 10 ~ "1-10",
      RankAdjEM <= 50 ~ "11-50",
      RankAdjEM <= 100 ~ "51-100",
      RankAdjEM <= 200 ~ "101-200",
      RankAdjEM <= 300 ~ "201-300",
      TRUE ~ "301+"
    ),
    tier = factor(tier, levels = c(
      "1-10", "11-50", "51-100", "101-200", "201-300", "301+"
    ))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = 1, y = AdjEM)) +
  ggplot2::geom_segment(
    ggplot2::aes(
      x = 0.5,
      xend = 1.5,
      yend = AdjEM,
      color = tier
    ),
    linewidth = .01,
    alpha = 0.1
  ) +
  ggplot2::geom_point(
    ggplot2::aes(color = tier),
    position = ggplot2::position_jitter(
      width = 0.09,
      height = 0,
      seed = 123
    )
  ) +
  ggplot2::geom_text(ggplot2::aes(
    label = ifelse(
      RankAdjEM %in% c(1, 10, 25, 50, 100, 150, 200, 250, 300, 
                       350, 365),
      RankAdjEM,
      ""
    ),
    x = 0.65
  ),
  size = 4,
  hjust = 1,
  family = "Roboto Condensed") +
  ggplot2::scale_color_viridis_d(
    option = "turbo",
    direction = -1,
    begin = 0.2,
    end = 0.8
  ) +
  ggplot2::scale_x_continuous(limits = c(0, 2)) +
  ggplot2::scale_y_continuous(
    limits = c(-35, 40),
    breaks = seq(-35, 40, by = 15)
  ) +
  ggplot2::labs(
    title = "College Basketball's Rating Reality",
    subtitle = "Shows a team's actual kenpom rating compared to it's ordinal rating.",
    x = "",
    y = "Rating",
    caption = "Viz by Chris at Bless your chart | data through January 14 games  \ndata via kenpom.com | Rating = adjusted efficiency margin",
    color = ""
  ) +
  hrbrthemes::theme_ipsum() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      size = 20,
      family = "Roboto Condensed"
    ),
    plot.subtitle = ggtext::element_markdown(
      hjust = 0.5,
      size = 9.5,
      lineheight = 1.5,
      family = "Roboto Condensed"
    ),
    legend.position = "none",
    axis.text.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(size = 14, family = "Roboto Condensed"),
    plot.caption = ggplot2::element_text(face = "plain", size = 8, "Roboto Condensed")
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = .90,
    y = 40,
    team = "Michigan", # 1 rank
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 1.10,
    y = 39,
    team = "Arizona", # 2 rank
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = 30,
    team = "Connecticut", #10 rank
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    geom = "curve", # distance to 10 and 1/2
    color = "#474747",
    x = 1.15,
    y = 30,
    xend = 1.15,
    yend = 37,
    curvature = .6,
    arrow = ggplot2::arrow(length = grid::unit(2, "mm"),
                           ends = "both")
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = 22,
    team = "Villanova", # 25 rank 
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = 16,
    team = "Creighton", # 50 rank
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    geom = "curve", # distance from 25 to 50 
    color = "#474747",
    x = 1.15,
    y = 16,
    xend = 1.15,
    yend = 22,
    curvature = .6,
    arrow = ggplot2::arrow(length = grid::unit(2, "mm"), ends = "both")
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = 7.8,
    team = "Hofstra", # 100 rank 
    height = 0.055,
    alpha = 0.8
  ) +
ggplot2::annotate(
  cfbplotR::GeomCFBlogo,
  x = 0.8,
  y = 1.3,
  team = "Cal Baptist", #150 rank 
  height = 0.055,
  alpha = 0.8
) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = -3,
    team = "Navy", # 200 rank 
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    geom = "curve", # distance from 100 to 10
    color = "#474747",
    x = 1.25,
    y = 8.5,
    xend = 1.25,
    yend = 30,
    curvature = .6,
    arrow = ggplot2::arrow(length = grid::unit(2, "mm"),
                           ends = "both")
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = -7.4,
    team = "Drexel", # 250 rank 
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = -11.9,
    team = "Alabama St.", # 300 rank 
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    geom = "curve", # distance from 300 to 100 
    color = "#474747",
    x = 1.25,
    y = 7.5,
    xend = 1.25,
    yend = -12,
    curvature = -.6,
    arrow = ggplot2::arrow(length = grid::unit(2, "mm"),
                           ends = "both")
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = -19.63,
    team = "NJIT", # 350 rank 
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    cfbplotR::GeomCFBlogo,
    x = 0.8,
    y = -33,
    team = "Mississippi Valley St.", # 365 rank 
    height = 0.055,
    alpha = 0.8
  ) +
  ggplot2::annotate(
    "label",
    x = 1.75,
    y = 30,
    label = "The ratings gap \nbetween #10 and the \ntop 2 is greater than \nthe gap from \n#25 to #50",
    size = 3,
    color = "#333333",
    family = "Roboto Condensed",
    fill = "floral white"
  ) +
  ggplot2::annotate(
    "label",
    x = 1.75,
    y = -10,
    label = "The gap from \n#100 to #300 is less \nthan the gap from \n#10 to #100",
    size = 3,
    color = "#333333",
    family = "Roboto Condensed",
    fill = "floral white",
  ) 
  
