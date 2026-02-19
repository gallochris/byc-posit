years <- 2004:2023

# Initialize a data frame to store the results
regular_season <- data.frame()

# Loop through the years
for (year in years) {
  
  # Get the rushing yards allowed for the given team and year
  team_stats <- cfbfastR::cfbd_game_team_stats(
    year = year,
    season_type = "regular",
    team = "North Carolina",
    rows_per_team = 1
  ) |> 
    dplyr::select(opponent, rushing_attempts_allowed, 
                  rushing_yards_allowed, 
                  yards_per_rush_attempt_allowed, 
                  rush_tds_allowed)
  
  # Add a column for the year
  team_stats <- team_stats |> 
    dplyr::mutate(year = year) |> 
    dplyr::relocate(year, .before = opponent)
  
  # Append the results to the main data frame
  regular_season <- rbind(regular_season, team_stats) |> 
    dplyr::mutate_at(dplyr::vars(-opponent), as.numeric) |> 
    dplyr::arrange(-rushing_yards_allowed)
  
}

# Return the results
return(regular_season)

}


years <- 2015

post_results <- data.frame()
# Loop through the years
for (year in years) {
  
  # Get the rushing yards allowed for the given team and year
  team_stats <- cfbfastR::cfbd_game_team_stats(
    year = year,
    season_type = "postseason",
    team = "North Carolina",
    rows_per_team = 1
  ) |> 
    dplyr::select(opponent, rushing_attempts_allowed, 
                  rushing_yards_allowed, 
                  yards_per_rush_attempt_allowed, 
                  rush_tds_allowed)
  
  # Add a column for the year
  team_stats <- team_stats |> 
    dplyr::mutate(year = year) |> 
    dplyr::relocate(year, .before = opponent)
  
  # Append the results to the main data frame
  post_results <- rbind(post_results, team_stats) |> 
    dplyr::mutate_at(dplyr::vars(-opponent), as.numeric) |> 
    dplyr::arrange(-rushing_yards_allowed)
  
}

# Return the results
return(post_results)

}

