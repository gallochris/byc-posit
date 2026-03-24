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


#--------------------------------------------------------------------

df <- readr::read_csv("srs_combined.csv") |>
  dplyr::mutate(
    year   = as.integer(stringr::str_sub(Season, 1, 4)),
    is_unc = Team == "North Carolina"
  ) |>
  dplyr::filter(year >= 1960) |>
  dplyr::arrange(Team, year) |>
  dplyr::group_by(Team) |>
  dplyr::mutate(
    srs_roll = zoo::rollmean(SRS, k = 5, fill = NA, align = "center")
  ) |>
  dplyr::ungroup()

gray_teams <- dplyr::filter(df, !is_unc)
unc        <- dplyr::filter(df,  is_unc)

unc_label <- unc |>
  dplyr::filter(!is.na(srs_roll)) |>
  dplyr::slice_tail(n = 1)

# Era markers: coach name + start year
eras <- tibble::tibble(
  year  = c(1962, 1997, 2003, 2021),
  label = c("Dean Smith", "Bill Guthridge /\nMatt Doherty", "Roy Williams", "Hubert Davis")
)

ggplot2::ggplot() +
  ggplot2::geom_vline(
    data     = eras,
    mapping  = ggplot2::aes(xintercept = year),
    color    = "#CCCCCC",
    linetype = "dotted",
    linewidth = 0.5
  ) +
  ggplot2::geom_text(
    data    = eras,
    mapping = ggplot2::aes(x = year, y = 32, label = label),
    color   = "#AAAAAA",
    size    = 2.6,
    hjust   = 0,
    nudge_x = 0.4,
    lineheight = 0.85
  ) +
  ggplot2::geom_line(
    data      = gray_teams,
    mapping   = ggplot2::aes(x = year, y = srs_roll, group = Team),
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
    hjust    = 0,
    size     = 3.3,
    fontface = "bold"
  ) +
  ggplot2::scale_x_continuous(
    limits = c(1960, 2030),
    breaks = seq(1960, 2030, by = 10),
    expand = ggplot2::expansion(mult = c(0.01, 0.12))
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(-10, 35, by = 5),
    labels = function(x) dplyr::if_else(x == 35, "35 (higher = better)", as.character(x))
  ) +
  ggplot2::labs(
    title    = "North Carolina Basketball SRS Over Time",
    subtitle = "5-year rolling average ",
    x        = NULL,
    y        = "Simple Rating System (SRS)",
    caption  = "Source: Sports-Reference · 5-year centered rolling mean (±2 seasons)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", size = 16,
                                             margin = ggplot2::margin(b = 4)),
    plot.subtitle    = ggplot2::element_text(color = "#555555", size = 11,
                                             margin = ggplot2::margin(b = 12)),
    plot.caption     = ggplot2::element_text(color = "#888888", size = 8, hjust = 1),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(color = "#EEEEEE"),
    axis.title.y     = ggplot2::element_text(size = 10, color = "#444444"),
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

