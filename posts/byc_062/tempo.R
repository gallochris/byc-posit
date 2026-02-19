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
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        size = 6,
        lineheight = 0.35,
        margin = ggplot2::margin(t = 0.5)
      ),
      plot.background = ggplot2::element_rect(fill = "floralwhite", color = "floralwhite")
    )
  
  return(custom_theme)
} 

first_med <- cbbdata::cbd_torvik_game_factors(team = "North Carolina") |>
  dplyr::filter(date > "2021-05-01") |>
  dplyr::group_by(year) |>
  dplyr::summarise(med = median(tempo), 
                   games = dplyr::n())

cbbdata::cbd_torvik_game_factors(team = "North Carolina") |>
  dplyr::filter(date > "2021-05-01") |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = tempo, group = year)) + 
  ggplot2::geom_boxplot(fill = "floralwhite", colour = "#56a0d3", 
                        outlier.colour = "#acacac", outlier.shape = 1, 
                        outlier.size = 3) +
  ggplot2::geom_jitter(position= ggplot2::position_jitter(0.2), 
                       size = 3, shape = 1, color = "#acacac") + 
  ggplot2::scale_x_continuous(breaks = seq(2022, 2024, 1)) + 
  theme_me() + 
  ggplot2::geom_label(
    data = first_med,
    ggplot2::aes(
      y = 80,
      x = year,
      label= paste0(med, "\n", games, " games")),
      color = "#333333",
      fill = "floralwhite",
    fontface = "bold",
    family = "mono"
  ) + 
  theme_me() +
  ggplot2::theme(legend.position = "none", 
                 plot.title = ggtext::element_markdown(face ='bold', family = 'mono'),
                 strip.text.x = ggtext::element_markdown(size = 12, 
                                                         face ='bold', family = 'mono'),
                 plot.subtitle = ggtext::element_markdown(size = 10, family = "mono"),
                 plot.caption = ggtext::element_markdown(size = 7, family = 'mono'),
                 axis.text.x = ggtext::element_markdown(size = 12, family = 'mono'),  
                 axis.text.y = ggtext::element_markdown(size = 12, family = 'mono')
  ) +
  ggplot2::labs(x = "",
                y = "",
                title = "<span style='color:#56a0d3;'>North Carolina Tempo </span>  \nsince 2021-22 season",
                subtitle = "Median number of possessions by season  \nfor all 81 games in Hubert Davis era.",
                caption = "Bless your chart | data via cbbdata"
  ) -> tempo_plot 


ggplot2::ggsave(
  "tempo_plot.png",
  tempo_plot,
  w = 7,
  h = 7,
  dpi = 600,
  type = 'cairo'
)
