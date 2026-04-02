# Scrape SRS for sports-reference.com 

scrape_cbb_team <- function(team_name) {
  url <- glue::glue("https://www.sports-reference.com/cbb/schools/{team_name}/")
  
  message(glue::glue("Scraping: {team_name}..."))
  
  out <- tryCatch({
    page <- rvest::read_html(url)
    
    table <- page |>
      rvest::html_table() |>
      purrr::pluck(1)
    
    col_names <- as.character(table[1, ])
    colnames(table) <- make.unique(col_names)
    
    table |>
      dplyr::slice(-1) |>
      dplyr::filter(Season != "Season") |> 
      dplyr::select(dplyr::any_of(c("Season", "SRS"))) |>
      dplyr::mutate(
        team_slug = team_name,
        team_name = stringr::str_replace_all(team_name, "-", " ") |> stringr::str_to_title(),
        SRS = as.numeric(SRS),
        year_suffix = as.integer(stringr::str_extract(Season, "\\d+$")),
        year = dplyr::if_else(year_suffix >= 50, 1900L + year_suffix, 2000L + year_suffix)
      ) |>
      dplyr::filter(!is.na(SRS)) |>
      dplyr::select(year, Season, team_name, team_slug, SRS)
    
  }, error = function(e) {
    message(glue::glue("   Error scraping {team_name}: {e$message}"))
    return(NULL)
  })
  
  return(out)
}


teams <- c(
  # ACC
  "boston-college", "california", "clemson", "duke", "florida-state", 
  "georgia-tech", "louisville", "miami-fl", "north-carolina", "north-carolina-state", 
  "notre-dame", "pittsburgh", "stanford", "syracuse", "virginia", 
  "virginia-tech", "wake-forest", "southern-methodist",
  
  # Big Ten
  "illinois", "indiana", "iowa", "maryland", "michigan", 
  "michigan-state", "minnesota", "nebraska", "northwestern", "ohio-state", 
  "oregon", "penn-state", "purdue", "rutgers", "ucla", 
  "southern-california", "washington", "wisconsin",
  
  # Big 12
  "arizona", "arizona-state", "baylor", "brigham-young", "colorado", 
  "houston", "iowa-state", "kansas", "kansas-state", "oklahoma-state", 
  "texas-christian", "texas-tech", "central-florida", "utah", "west-virginia", "cincinnati",
  
  # SEC
  "alabama", "arkansas", "auburn", "florida", "georgia", 
  "kentucky", "louisiana-state", "mississippi", "mississippi-state", "missouri", 
  "oklahoma", "south-carolina", "tennessee", "texas", "texas-am", "vanderbilt",
  
  # Big East
  "butler", "connecticut", "creighton", "depaul", "georgetown", 
  "marquette", "providence", "st-johns-ny", "seton-hall", "villanova", "xavier"
)

all_teams_srs <- purrr::map(teams, \(team) {
  Sys.sleep(5) # Crucial to avoid being blocked
  scrape_cbb_team(team)
}) |>
  dplyr::bind_rows()

write.csv(all_teams_srs, "all_teams_srs.csv")

#--------------------------------------------------------------------

df <- all_teams_srs  |>
  dplyr::mutate(
    year   = as.integer(stringr::str_sub(Season, 1, 4)),
    is_unc = team_name == "North Carolina"
  ) |>
  dplyr::filter(year >= 1960) |>
  dplyr::arrange(team_name, year) |>
  dplyr::group_by(team_name) |>
  dplyr::mutate(
    srs_roll = zoo::rollmean(SRS, k = 5, fill = NA, align = "center")
  ) |>
  dplyr::ungroup()

gray_teams <- dplyr::filter(df, !is_unc)
unc        <- dplyr::filter(df,  is_unc)

unc_label <- unc |>
  dplyr::filter(!is.na(srs_roll)) |>
  dplyr::slice_tail(n = 1)


ggplot2::ggplot() +
  ggplot2::geom_line(
    data      = gray_teams,
    mapping   = ggplot2::aes(x = year, y = srs_roll, group = team_name),
    color     = "#D0D0D0",
    linewidth = 0.4,
    alpha     = 0.5,
    na.rm     = TRUE
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype   = "dashed",
    color      = "#BBBBBB",
    linewidth  = 0.35
  ) +
  ggplot2::geom_ribbon(
    data    = unc,
    mapping = ggplot2::aes(x = year, ymin = 0, ymax = srs_roll),
    fill    = "#4B9CD3",
    alpha   = 0.12,
    na.rm   = TRUE
  ) +
  ggplot2::geom_line(
    data      = unc,
    mapping   = ggplot2::aes(x = year, y = srs_roll),
    color     = "#4B9CD3",
    linewidth = 1.5,
    na.rm     = TRUE
  ) +
  ggplot2::annotate(
    "text",
    x        = unc_label$year + 1,
    y        = unc_label$srs_roll,
    label    = "North Carolina",
    color    = "#4B9CD3",
    family = "Roboto Condensed",
    hjust    = 0,
    size     = 3.3,
    fontface = "bold"
  ) +
  ggplot2::scale_x_continuous(
    limits = c(1960, 2030),
    breaks = seq(1960, 2030, by = 5),
    expand = ggplot2::expansion(mult = c(0.01, 0.12))
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(-10, 35, by = 5),
    labels = function(x) dplyr::if_else(x == 35, "35 (higher = better)", as.character(x))
  ) +
  ggplot2::labs(
    title    = "Carolina basketball's continues to strive for sustained consistency",
    subtitle = "5-year rolling SRS (simple-rating-system) across 75+ major-conference programs since 1960. North Carolina in blue, all others programs in gray.",
    x        = "",
    y        = "Simple Rating System (SRS)",
    caption  = "data via sports-reference.com <br><br>Viz by Chris at Bless your chart"
  ) +
 hrbrthemes::theme_ipsum(
   base_family = "Roboto Condensed",
 ) +
  ggplot2::theme(
    plot.title    = ggtext::element_markdown(face = "bold", size = 14, family = "Roboto Condensed"),
    plot.subtitle = ggtext::element_markdown(size = 10, colour = "grey40", family = "Roboto Condensed"),
    plot.caption  = ggtext::element_markdown(size = 8, colour = "grey50", family = "Roboto Condensed", face = "plain"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "#EEEEEE"),
    axis.title.y     = ggplot2::element_text(size = 10, color = "#444444", family = "Roboto Condensed"),
    axis.text.x      = ggplot2::element_text(size = 10),
    legend.position  = "none",
    plot.margin      = ggplot2::margin(16, 24, 12, 16)
  )

ggplot2::ggsave(
  "srs_rolling.png",
  width  = 11,
  height = 6.5,
  dpi    = 180,
  bg     = "white"
)

