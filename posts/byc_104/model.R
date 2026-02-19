View the results
print(result %>% select(team_name, kford_rk, fei_rk, sp_rk, rank_range, rank_sd, avg_rank))

#normalize the ratings

z_score <- function(x) {
  (x - mean(x)) / sd(x)
}

# Normalize ratings and create composite
composite <- trio |> 
  dplyr::mutate(
    sp_norm = z_score(sp_rating),
    kford_norm = z_score(kford_rating),
    fei_norm = z_score(fei_rating),
    comp_rating = round((sp_norm + kford_norm + fei_norm) / 3, 2)
  ) |> 
  dplyr::arrange(-comp_rating) |> 
  dplyr::select(team_name, comp_rating)

# Now pull in the schedule 
sched <- readr::read_csv("2024_sched_win_totals.csv") |> 
  dplyr::select(-...1, -win_total, -color) |> 
  dplyr::mutate(team_name = dplyr::case_match(team_name, 
                                              "Connecticut" ~ "UConn",
                                              "San José State" ~ "San Jose State",
                                              "Hawai'i" ~ "Hawaii",
                                              team_name ~ team_name
  ))

sched_with_rtg <- sched |> 
  dplyr::left_join(composite, by = c("team_name" = "team_name")) |> 
  dplyr::rename(team_rating = comp_rating) |> 
  dplyr::left_join(composite, by = c("opponent" = "team_name")) |> 
  dplyr::rename(opp_rating = comp_rating)  |> 
  dplyr::mutate(opp_rating = dplyr::if_else(is.na(opp_rating), -1.35, opp_rating),
                adjusted_team_rating = dplyr::case_when(
                  home_away == "home" ~ team_rating + 1.50,
                  home_away == "away" ~ team_rating - 1.50,
                  TRUE ~ team_rating
                )) |> 
  dplyr::mutate(rating_diff = adjusted_team_rating - opp_rating,
                result = dplyr::if_else(rating_diff > 0, 1, 0))

# now try to make a model 
model <- rstanarm::stan_glm(
  result ~ rating_diff,
  family = binomial(link = "logit"),
  data = sched_with_rtg,
  chains = 4,
  iter = 2000,
  seed = 123
)

# win prob function 
predict_win_prob <- function(rating_diff) {
  new_data <- data.frame(rating_diff = rating_diff)
  
  # Predict using the Bayesian model
  pred <- posterior_predict(model, newdata = new_data)
  
  # Calculate the mean probability
  win_prob <- mean(pred)
  
  return(win_prob)
}

# add win prob to dataframe 
sched_with_rtg$win_prob <- sapply(
  sched_with_rtg$rating_diff,
  predict_win_prob
)

sched_with_rtg |>
  dplyr::group_by(team_name) |>
  dplyr::summarise(
    expected_wins = sum(win_prob, na.rm = TRUE),
    games_played = dplyr::n(),
    games_with_prediction = sum(!is.na(win_prob))
  ) |>
  dplyr::arrange(desc(expected_wins))