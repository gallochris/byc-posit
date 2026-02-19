full_sched <- readr::read_csv("full_cbb_sched.csv") |> 
  dplyr::select(-`...1`)

# add results 
current_results <- cbbdata::cbd_torvik_game_stats(year = 2024) |> 
  dplyr::select(game_id, date, team, 
                opponent = opp, result, pts_scored, opp_pts)

sched_with_results <- full_sched |> 
  dplyr::left_join(current_results, by = c("game_id", "team",
                                           "opponent", "date"))
# fetch latest net rankings
today_date <- format(Sys.Date(), "%Y-%m-%d")

net_up <- readr::read_csv("https://raw.githubusercontent.com/andreweatherman/NCAA_NET_RANKINGS/main/complete_data.csv") |> 
  dplyr::mutate(conf = dplyr::case_match(team,
                                         "Tarleton St." ~ "WAC",     
                                         "Utah Tech" ~ "WAC",         
                                         "St. Thomas" ~ "Sum",  
                                         "UC San Diego" ~ "BW", 
                                         "Bellarmine" ~  "ASun",     
                                         "Queens" ~  "ASun",           
                                         "Le Moyne" ~ "NEC",   
                                         "Lindenwood" ~ "OVC",        
                                         "Texas A&M Commerce" ~ "Slnd",
                                         "Southern Indiana" ~ "OVC",  
                                         "Stonehill" ~ "NEC",
                                         team ~ conf))

yest_opp_net <- net_up |> 
  dplyr::filter(date == "2023-12-13") |> 
  dplyr::select(opponent = team, opp_prev_net = net)

yest_team_net <- net_up |> 
  dplyr::filter(date == "2023-12-13") |> 
  dplyr::select(team, prev_net = net)

opp_net <- net_up |> 
  dplyr::filter(date == today_date) |> 
  dplyr::select(opponent = team, opp_net = net) |> 
  dplyr::left_join(yest_opp_net, by = "opponent")

team_net <- net_up |> 
  dplyr::filter(date == today_date) |> 
  dplyr::select(team, net) |> 
  dplyr::left_join(yest_team_net, by = "team")

# team confs
team_confs <- net_up |> 
  dplyr::filter(date == today_date) |> 
  dplyr::select(team, conf) 

quad_sched <- sched_with_results |> 
  dplyr::filter(type != "nond1") |> 
  dplyr::left_join(team_confs, by = "team") |> 
  dplyr::left_join(opp_net, by = "opponent") |>
  dplyr::left_join(team_net, by = "team") |> 
  dplyr::mutate(quad = dplyr::case_when(
    opp_net < 31 & location == "home" ~ "Q1",
    opp_net < 51 & location == "neutral" ~ "Q1",
    opp_net < 76 & location == "away" ~ "Q1",
    opp_net > 30 & opp_net < 76 & location == "home" ~ "Q2",
    opp_net > 50 & opp_net < 101 & location == "neutral" ~ "Q2",
    opp_net > 75 & opp_net < 136 & location == "away" ~ "Q2",
    opp_net > 75 & opp_net < 161 & location == "home" ~ "Q3",
    opp_net > 100 & opp_net < 201 & location == "neutral" ~ "Q3",
    opp_net > 135 & opp_net < 241 & location == "away" ~ "Q3",
    opp_net > 160 & opp_net < 363 & location == "home" ~ "Q4",
    opp_net > 200 & opp_net < 363 & location == "neutral" ~ "Q4",
    opp_net > 240 & opp_net < 363 & location == "away" ~ "Q4"
  )) 

yesterday <- quad_sched |> 
  dplyr::filter(date == "2023-12-13") |> 
  dplyr::mutate(delta = prev_net - net, 
                opp_delta = opp_prev_net - opp_net) 

acc_teams_only <- yesterday |> 
  dplyr::filter(conf == "ACC") |> 
  dplyr::mutate(result = paste0(result, ",", pts_scored, "-", opp_pts)) |> 
  dplyr::select(team, opponent, result, net, prev_net, delta, opp_net, opp_prev_net, 
                opp_delta) |> 
  dplyr::bind_rows(idle_acc) |> 
  dplyr::mutate(across(everything(), ~ replace(.x, is.na(.x), ""))) 

idle_acc <- team_net |> 
  dplyr::filter(team %in% c("Boston College", 
                            "North Carolina", 
                            "North Carolina St.",
                            "Miami FL",
                            "Virginia")) |> 
  dplyr::mutate(delta = prev_net - net) |> 
  dplyr::select(team, net, prev_net, delta) 
  
acc_teams_only |> 
  dplyr::arrange(as.numeric(net)) |> 
  gt::gt() |> 
  gt::cols_label(
    net = "Today",
    prev_net = "Yesterday",
    delta = "+/-",
    opp_net = "Today", 
    opp_prev_net = "Yesterday",
    opp_delta = "+/-",
  ) |> 
  gt::fmt(
    columns = c(delta, opp_delta),
    fns = function(x) {
      ifelse(x > 0, paste0("+", x), x)
    }
  ) |> 
  gt::tab_header(
    title = "NET Changes by ACC Team after December 9 Games") |> 
  gt::tab_source_note(source_note = "Bless your chart | December 10, 2023") |> 
  gtExtras::gt_theme_dot_matrix() |> 
  gt::tab_options (
    source_notes.font.size = gt::px(10),
    row.striping.background_color = '#ffffed',
    table.font.size = gt::px(12),
    column_labels.text_transform = 'capitalize'
  ) |> 
  gt::tab_spanner(label = "NET Changes",
                  columns = c(net, prev_net, delta)) |> 
  gt::tab_spanner(label = "Opponent NET Changes", 
                  columns = c(opp_net, opp_prev_net, opp_delta))

### best and worst 


net_up |>
  dplyr::filter(date == today_date) |>
  dplyr::group_by(conf) |>
  dplyr::summarise(best_rk = min(net),
                   best_team_rk = dplyr::first(team[net == min(net)]),
                   worst_rk = max(net),
                   worst_team_rk = dplyr::first(team[net == max(net)]),
                   delta = worst_rk - best_rk,
                   avg = mean(net),
                   med = median(net), 
                   avg_without_worst = mean(net[team != worst_team_rk]),
                   med_without_worst = median(net[team != worst_team_rk])
                   ) |>
  dplyr::select(conf, avg, med, avg_without_worst, med_without_worst, delta, best_rk, best_team_rk, 
                worst_rk, worst_team_rk) |> 
  dplyr::filter(conf != "ind") |> 
  dplyr::arrange(avg) |>
  print(n = 32) |> 
  dplyr::mutate(conf = dplyr::case_match(conf, 
                                         "B12" ~ "Big 12",  
                                         "BE" ~ "Big East", 
                                         "P12" ~ "Pac-12", 
                                         "B10" ~ "Big Ten",
                                         "Amer" ~ "American",
                                         "SB" ~ "Sun Belt",
                                         "Slnd" ~ "Southland",
                                         "BW" ~ "Big West", 
                                         "SC" ~ "Southern",
                                         "AE" ~ "America East", 
                                         "BSth" ~ "Big South",
                                         "ASun" ~ "Atlantic Sun",
                                         "Pat" ~ "Patriot",
                                         "Horz" ~ "Horizon",
                                         "BSky" ~ "Big Sky",
                                         "OVC" ~ "Ohio Valley",
                                         "Sum" ~ "Summit", 
                                         "A10" ~ "Atlantic 10",
                                         "MWC" ~ "Mountain West",
                                         "MVC" ~ "Missouri Valley",
                                         "NEC" ~ "Northeast",
                                         "MAC" ~ "Mid-American",
                                         "MAAC" ~ "Metro Atlantic",
                                         conf ~ conf)) -> silly 


silly |> 
  dplyr::mutate(best_team = paste0(best_rk, "-", best_team_rk),
                worst_team = paste0(worst_rk, "-", worst_team_rk)) |> 
  gt::gt(rowname_col = "conf") |> 
  gt::cols_hide(columns = c(best_rk, best_team_rk, worst_rk, worst_team_rk)) |> 
  gt::cols_label(avg = "AVG", 
                 med = "Median", 
                 avg_without_worst = "AVG", 
                 med_without_worst = "Median",
                 delta = "+/-",
                 best_team = "Highest Rank", 
                 worst_team = "Lowest Rank") |> 
  gt::fmt_number(columns = c(avg, med, avg_without_worst,
                             med_without_worst), decimals = 1) |> 
  gt::tab_spanner(label = "All Teams", columns = c(avg, med)) |> 
  gt::tab_spanner(label = "Without Worst Team", columns = c(avg_without_worst, 
                                                            med_without_worst)) |> 
  gt::tab_spanner(label = "Delta Between Highest & Lowest Rank", 
                  columns = c(best_team, worst_team, delta)) |> 
  gtExtras::gt_hulk_col_numeric(columns = c(avg_without_worst, med_without_worst, 
                                             delta), reverse = TRUE) |> 
  gtExtras::gt_theme_excel() |> 
  gt::tab_header(title = "NET Rankings by Conference Through December 13 Games", 
                 subtitle = "Shows average and median NET ranking for each conference with and without
                 the worst ranked team in each league.") |> 
  gt::tab_source_note(source_note = "Bless your chart | data via cbbdata | December 14, 2023")

