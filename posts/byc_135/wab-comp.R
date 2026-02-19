
net_page <- rvest::read_html("https://stats.ncaa.org/selection_rankings/nitty_gritties/43331")

net_rk <- net_page |>
  rvest::html_nodes("table") |>
  rvest::html_table(fill = TRUE)

net_table <- as.data.frame(net_rk[[1]])  # Assume the relevant table is the first one

# Ensure unique column names
colnames(net_table) <- make.names(colnames(net_table), unique = TRUE)

net_wab <- net_table |>   
  dplyr::mutate(
    date = gsub("[^0-9.-]", "", colnames(net_table[3])),
    # replace periods with slashes for better date formate
    date = gsub("\\.", "/", date),
    date = lubridate::mdy(date)
  ) |> 
  dplyr::mutate(
    record = stringr::str_split(WL, "-", simplify = TRUE),
    wins = record[,1],
    losses = record[,2],
    conf_record = stringr::str_split(ConfWL, "-", simplify = TRUE),
    wins_conf = conf_record[,1],
    losses_conf = conf_record[,2],
    non_conf_record = stringr::str_split(NCWL, "-", simplify = TRUE),
    wins_non_conf = non_conf_record[,1],
    losses_non_conf = non_conf_record[,2],
    road_record = stringr::str_split(RoadWL, "-", simplify = TRUE),
    road_wins = road_record[,1],
    road_losses = road_record[,2],
    first_q = stringr::str_split(Q1, "-", simplify = TRUE),
    q1_win = first_q[,1],
    q1_loss = first_q[,2],
    second_q = stringr::str_split(Q2, "-", simplify = TRUE),
    q2_win = second_q[,1],
    q2_loss = second_q[,2],
    third_q = stringr::str_split(Q3, "-", simplify = TRUE),
    q3_win = third_q[,1],
    q3_loss = third_q[,2],
    fourth_q = stringr::str_split(Q4, "-", simplify = TRUE),
    q4_win = fourth_q[,1],
    q4_loss = fourth_q[,2]
  ) |> 
  dplyr::rename(team = Team, conf = Conference, net = NET.Rank, prev_net = PrevNET, 
                sos = NETSOS, non_conf_sos = NETNCSOS,
                n_wab = WAB, wab_rank = WABRk, non_conf_wab = NCWAB) |> 
  dplyr::select(team, conf, net, prev_net, sos, non_conf_sos, wab_rank, n_wab, non_conf_wab, 
                wins, losses, wins_non_conf, losses_non_conf, road_wins, road_losses,
                q1_win, q1_loss, 
                q2_win, q2_loss, q3_win, q3_loss, q4_win, q4_loss, date) |> 
  dplyr::mutate_at(dplyr::vars(-team, -conf, -date), as.numeric)

bart_wab <-  cbbdata::cbd_torvik_ratings(year = 2025) |> 
  dplyr::mutate(wab_trk = dplyr::dense_rank(dplyr::desc(wab))) |> 
  dplyr::select(team, wab_trk, total = wab)


bart_wab |> 
  dplyr::left_join(net_wab, by = "team") |> 
  dplyr::mutate(record = paste0(wins, "-", losses)) |> 
  dplyr::select(team, conf, record,  wab_trk, net_wab_rk = wab_rank) |> 
  dplyr::filter(team %in% bub_teams) |> 
  dplyr::mutate(wab_diff = net_wab_rk - wab_trk) |> 
  dplyr::arrange(-wab_diff) -> wab_comp 
