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

# first nine games only 
cbbdata::cbd_torvik_game_stats(team = "North Carolina") |> 
  dplyr::group_by(year) |> 
  dplyr::arrange(date) |> 
  dplyr::slice(1:9) |> 
  dplyr::summarise(ftr = sum(fta) / sum(fga) * 100) |> 
  dplyr::arrange(-year) |> 
  gt::gt() |> 
  gt::cols_label(
    year = "Season",
    ftr = "FT Rate"
  ) |> 
  gt::fmt_number(columns = c(ftr), decimals = 1) |> 
  gtExtras::gt_theme_dot_matrix() |> 
  gt::tab_header(title = "North Carolina Offense: Free Throw Rate", 
             subtitle = "Through only the first NINE games of each season" ) |> 
  gt::tab_source_note(source_note = "Bless your chart | data via cbbdata | December 13, 2023") |> 
  gt::tab_options (
    source_notes.font.size = gt::px(10),
    row.striping.background_color = '#d0e4f3',
    table.font.size = gt::px(12),
    column_labels.text_transform = 'capitalize'
  ) -> table_ftr 

gt::gtsave(table_ftr,
             "table_ftr.png",
             vwidth = 350,
             vheight = 650)

## since 2008 
cbbdata::cbd_torvik_game_factors(team = "North Carolina") |>
  dplyr::mutate(season = dplyr::if_else(is.na(season), 2024, season)) |> 
  dplyr::mutate(month = lubridate::month(date), 
                year = lubridate::year(date)) |> 
  dplyr::arrange(date) |> 
  dplyr::group_by(month, year) |> 
  dplyr::mutate(month_label = 
                  paste0(dplyr::case_match(month, 
                                                11 ~ "November",
                                                12 ~ "December", 
                                                1 ~ "January", 
                                                2 ~ "February", 
                                                3 ~ "March", 
                                                4 ~ "April", 
                                                month ~ as.character(month)),
                         " ", year)) |> 
  dplyr::arrange(year) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(order_month = dplyr::row_number()) -> ftr_all


ftr_all |> 
  dplyr::group_by(season) |> 
  dplyr::mutate(game_no = dplyr::row_number()) |> 
  dplyr::ungroup() |> 
  ggplot2::ggplot(ggplot2::aes(x = game_no, y = off_ftr)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(fill = "#acacac", color = "#56a0d3") +
  ggplot2::facet_wrap(~factor(season, levels=c(2024:2008)), nrow = 8) +
  ggplot2::scale_y_continuous(breaks = seq(0, 80, 20)) +
  ggplot2::scale_x_continuous(breaks = seq(0, 40, 10)) +
  theme_me() +
  ggplot2::theme(legend.position = "none", 
                 plot.title = ggtext::element_markdown(face ='bold', family = 'mono'),
                 strip.text.x = ggtext::element_markdown(size = 12, 
                                                         face ='bold', family = 'mono'),
                 plot.subtitle = ggtext::element_markdown(size = 10, family = "mono"),
                 plot.caption = ggtext::element_markdown(size = 7, family = 'mono'),
                 axis.text.x = ggtext::element_markdown(size = 12, family = 'mono', face ='bold'),  
                 axis.text.y = ggtext::element_markdown(size = 12, family = 'mono', face ='bold')
  ) +
  ggplot2::labs(x = "",
                y = "",
                title = "<span style='color:#56a0d3;'>North Carolina  \nFree Throw Rate</span>  \nsince 2008-09 season",
                subtitle = "Shows offensive free throw rate by game  \nfor each season since the 2008-09 season.",
                caption = "Bless your chart | data via cbbdata | December 13, 2023"
  ) -> ftr_plot


ggplot2::ggsave(
  "ftr_plot.png",
  ftr_plot,
  w = 6,
  h = 9,
  dpi = 600,
  type = 'cairo'
)
