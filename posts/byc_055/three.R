fbs_only <- cfbfastR::cfbd_conferences() |> 
  dplyr::filter(classification == "fbs") 

games <- cfbfastR::cfbd_game_info(
  year = "2023",
  season_type = "regular"
) |> 
  dplyr::filter(home_conference %in% fbs_only$name & 
                  away_conference %in% fbs_only$name)

# clean games with score 
clean_games <- games |> 
  dplyr::filter(!is.na(home_points)) |> 
dplyr::mutate(score_sentence = dplyr::if_else(
  as.numeric(home_points) > as.numeric(away_points), 
  paste0(home_team, " ", home_points, ", ", 
  away_team, " ", away_points),
  paste0(away_team, " ", away_points, ", ", 
         home_team, " ", home_points)))  |> 
  dplyr::select(game_id, week, score_sentence)

drives <- cfbfastR::cfbd_drives(
  year = "2023",
  season_type = "regular"
) |> 
  dplyr::filter(offense_conference %in% fbs_only$name & 
                  defense_conference %in% fbs_only$name) |> 
  dplyr::left_join(clean_games, by ="game_id") 
    
