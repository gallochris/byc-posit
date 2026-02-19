# scrap table from espn+ 
# https://www.convertcsv.com/html-table-to-csv.htm
# save as csv
# load data
data <- readr::read_csv("sp.csv")

# function to clean table 
clean_sp_table <- function(sp) {
  sp |> 
    dplyr::mutate(team = gsub("[^a-zA-Z]", "", Team),
                  rating = `SP+`,
                  off_rating = stringr::str_extract(`Off. SP+`, "\\d+\\.?\\d*"),
                  offense_ranking = stringr::str_extract(`Off. SP+`,
                                                         pattern = "(?<=\\().*(?=\\))"),
                  defense_ranking = stringr::str_extract(`Def. SP+`,
                                                         pattern = "(?<=\\().*(?=\\))"),
                  def_rating = stringr::str_extract(`Def. SP+`, "\\d+\\.?\\d*"),
                  st_ranking = stringr::str_extract(`ST SP+`,
                                                    pattern = "(?<=\\().*(?=\\))"),
                  st_rating = stringr::str_extract(`ST SP+`, "\\d+\\.?\\d*"),
    ) |> 
    dplyr::mutate_at(c('rating', 'off_rating', 'offense_ranking', 'defense_ranking', 'def_rating', 
                       'st_ranking', 'st_rating'), as.numeric) |> 
    dplyr::mutate(
      # change names to be abbreviated
      team = dplyr::case_match(team,
                               'NCarolina' ~ 'North Carolina',
                               'OhioSt' ~ 'Ohio State',
                               'TexasAM' ~ 'Texas A&M',
                               'PennSt' ~ 'Penn State',
                               'IowaSt' ~ 'Iowa State',
                               'NotreDame' ~ 'Notre Dame',
                               'ArizonaSt' ~ 'Arizona State',
                               'OleMiss' ~ 'Mississippi',
                               'MichiganSt' ~ 'Michigan State',
                               'NCSt' ~ 'NC State',
                               'MissSt' ~ 'Mississippi State',
                               'OklahomaSt' ~ 'Oklahoma State',
                               'KansasSt' ~ 'Kansas State',
                               'CoastalCaro' ~ 'Coastal Carolina',
                               'WakeForest' ~ 'Wake Forest',
                               'SanDiegoSt' ~ 'San Diego State',
                               'AppSt' ~ 'Appalachian State',
                               'BoiseSt' ~ 'Boise State',
                               'FloridaSt' ~ 'Florida State',
                               'FresnoSt' ~ 'Fresno State',
                               'VaTech' ~ 'Virginia Tech',
                               "WVirginia" ~ "West Virginia",
                               "TexasTech" ~ "Texas Tech",
                               "BostonColl" ~ "Boston College",
                               "JMU" ~ "James Madison",
                               "WKU" ~ "Western Kentucky",
                               "ECU" ~ "East Carolina", 
                               "OregonSt" ~ "Oregon State",
                               "WashSt" ~ "Washington State",
                               "AirForce" ~ "Air Force",
                               "ODU" ~ "Old Dominion", 
                               'GaTech' ~ 'Georgia Tech',
                               "SCarolina" ~ "South Carolina", 
                               "SanJoseSt" ~ "San Jose State",
                               "ColoradoSt" ~ "Colorado State",
                               "FAU" ~ "Florida Atlantic", 
                               "MTSU" ~ "Middle Tennessee",
                               "EMU" ~ "Eastern Michigan",
                               "MiamiOH" ~ "Miami (OH)",
                               "GeorgiaSt" ~ "Georgia State",
                               "UtahSt" ~ "Utah State",
                               "KentSt" ~ "Kent State",
                               "BallSt" ~ "Ball State",
                               "SAlabama" ~ "South Alabama",
                               "NIU" ~ "Northern Illinois",
                               "USF" ~ "South Florida", 
                               "LaTech" ~ "Louisiana Tech",
                               "NTexas" ~ "North Texas",  
                               "GaSouthern" ~ "Georgia Southern",
                               "NewMexico" ~ "New Mexico",   
                               "SoMiss" ~ "Southern Mississippi",
                               "TexasSt" ~ "Texas State",
                               "ArkansasSt" ~ "Arkansas State",   
                               "CMU" ~ "Central Michigan",
                               "ULM" ~ "Louisiana Monroe",
                               "BGSU" ~ "Bowling Green",
                               "WMU" ~ "Western Michigan",
                               "NMSU" ~ "New Mexico State",
                               "UL-Monroe" ~ "Louisiana Monroe",
                               team ~ team
      )
    ) |> 
    dplyr::select(team_name = team, rating, offense_ranking, off_rating, defense_ranking, 
                  def_rating, st_ranking, st_rating)
}

sp <- clean_sp_table(sp = data)