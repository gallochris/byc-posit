rtg <- kp_get_ratings(year = 2026) |> 
  dplyr::select(TeamName, ConfShort, Seed, Coach, Wins, Losses, AdjEM, RankAdjEM)

ff <- kp_get_four_factors(year = 2026) |> 
  dplyr::left_join(rtg, by = "TeamName")
  
tram <- ff |>   
  dplyr::mutate(
    to_pct = TO_Pct / 100,
    or_pct = OR_Pct / 100,
    d_to_pct = DTO_Pct / 100,
    d_or_pct = DOR_Pct / 100
  ) |> 
  dplyr::mutate(
    off_svi = ((100 - (100 * to_pct)) + (or_pct * (0.561 * (100 - (100 * to_pct))))),
    def_svi = ((100 - (100 * d_to_pct)) + (d_or_pct * (0.561 * (100 - (100 * d_to_pct))))),
    tram = off_svi - def_svi
  ) |> 
  dplyr::filter(Seed != 0) |> 
  dplyr::select(seed = Seed, team= TeamName, conf = ConfShort, tram, adj_em = AdjEM,
                off_svi, def_svi, 
                to_pct, or_pct, d_to_pct, d_or_pct,
                adj_o = AdjOE, adj_d = AdjDE, off_e_fg_pct = eFG_Pct, off_ft_rate = FT_Rate,
                def_e_fg_pct = DeFG_Pct, def_ft_rate = DFT_Rate) |> 
  dplyr::arrange(-tram)



cbbdata::cbd_torvik_team_factors(year = 2025)