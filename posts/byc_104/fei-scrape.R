# Fetch the win totals from Vegas Insider
url <- "https://www.bcftoys.com/2024-fplus/"

webpage <- rvest::read_html(url)

# pull out the table and the first six columns 
f_plus <- webpage |>
  rvest::html_node("table") |>
  rvest::html_table() |>
  dplyr::slice(-1) |> 
  dplyr::select(f_plus_rk = X1, team_name = X2, f_plus = X3, record = X11) |> 
  dplyr::slice(-1) |> 
  dplyr::filter(f_plus_rk != "Rk") |> 
  na.omit() |> 
  dplyr::mutate_at(dplyr::vars(-team_name, -record), as.numeric) |> 
  dplyr::mutate(team_name = dplyr::case_match(team_name, 
                                              "Connecticut" ~ "UConn",
                                              "UL Monroe" ~ "Louisiana Monroe",
                                              "Sam Houston" ~ "Sam Houston State",
                                              "Massachusetts" ~ "UMass",
                                              team_name ~ team_name),
                f_plus_ptile = dplyr::percent_rank(f_plus)
  )

fpi <- cfbfastR::espn_ratings_fpi(year = 2024) |> 
  dplyr::select(team_name, fpi) |> 
  dplyr::mutate(fpi = as.numeric(fpi)) |>     
  dplyr::mutate(
    fpi_rk = dplyr::dense_rank(dplyr::desc(fpi)),
    fpi_ptile = dplyr::percent_rank(fpi)
  ) 

# combine the two and add conferences
cfbrks <- f_plus |> 
  dplyr::left_join(fpi, by = c("team_name")) |> 
  dplyr::left_join(
    cfbfastR::cfbd_team_info(year = 2024) |>
  dplyr::select(team_name = school, conf = conference), 
          by = "team_name") |> 
  dplyr::mutate(
    team_name = paste0(team_name, " (", record, ")")
  ) |> 
  dplyr::select(team_name, 
                f_plus_rk,
                f_plus,
                f_plus_ptile, 
                fpi_rk, 
                fpi,
                fpi_ptile)





