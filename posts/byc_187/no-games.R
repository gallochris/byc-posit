library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

df <- readr::read_csv("cbase_games_today.csv", show_col_types = FALSE)

conferences <- list(
  SEC = c(
    "Georgia", "Auburn", "Texas", "Alabama", "Florida", "Texas A&M",
    "Mississippi St.", "Arkansas", "Kentucky", "Ole Miss", "Oklahoma", "Tennessee"
  ),
  ACC = c(
    "Boston College", "Florida St.", "Georgia Tech", "Miami (FL)",
    "NC State", "North Carolina", "Virginia", "Virginia Tech", "Wake Forest"
  ),
  `Sun Belt` = c(
    "Coastal Carolina", "Louisiana", "Southern Miss.", "Texas St.", "Troy"
  ),
  `Big Ten` = c(
    "Nebraska", "Oregon", "Southern California", "UCLA"
  ),
  `Conference USA` = c(
    "Jacksonville St.", "Liberty", "Missouri St."
  ),
  `Big West` = c(
    "Cal Poly", "UC Santa Barbara"
  )
)

# Parse winner from "Georgia 9-7" style Final Score
parse_winner <- function(score) {
  stringr::str_extract(score, "^[A-Za-z .()&'-]+(?=\\s+\\d)")  |>
    stringr::str_trim()
}

df <- df |>
  dplyr::mutate(winner = parse_winner(`Final Score`))

# Build head-to-head record for any two teams
# Returns: t1_wins, t2_wins across all games between them
h2h_record <- function(t1, t2, games) {
  matchups <- games |>
    dplyr::filter(
      (`Away Team` == t1 & `Home Team` == t2) |
        (`Away Team` == t2 & `Home Team` == t1)
    )
  
  if (nrow(matchups) == 0) return(NULL)
  
  matchups |>
    dplyr::summarise(
      t1_wins = sum(winner == t1, na.rm = TRUE),
      t2_wins = sum(winner == t2, na.rm = TRUE)
    )
}

# For a given conference, build the full results table
build_conf_results <- function(teams, conf_name, games) {
  pairs <- tidyr::expand_grid(t1 = teams, t2 = teams) |>
    dplyr::filter(t1 < t2)
  
  pairs |>
    dplyr::mutate(
      result = purrr::map2(t1, t2, \(a, b) h2h_record(a, b, games))
    ) |>
    dplyr::mutate(
      played  = purrr::map_lgl(result, \(r) !is.null(r)),
      t1_wins = purrr::map_int(result, \(r) if (is.null(r)) NA_integer_ else r$t1_wins),
      t2_wins = purrr::map_int(result, \(r) if (is.null(r)) NA_integer_ else r$t2_wins)
    ) |>
    dplyr::select(-result) |>
    # Sort: unplayed pairs first, then by t1 alphabetically
    dplyr::arrange(played, t1, t2) |>
    dplyr::mutate(
      conference = conf_name,
      t1_wins    = dplyr::if_else(played, as.character(t1_wins), "-"),
      t2_wins    = dplyr::if_else(played, as.character(t2_wins), "-")
    ) |>
    dplyr::select(conference, t1, t1_wins, t2_wins, t2)
}

results_all <- purrr::imap(conferences, \(teams, conf_name) {
  build_conf_results(teams, conf_name, df)
}) |>
  purrr::list_rbind()

# Print each conference
purrr::iwalk(conferences, function(teams, conf_name) {
  res <- results_all |>
    dplyr::filter(conference == conf_name)
  
  n_missing <- res |> dplyr::filter(t1_wins == "-") |> nrow()
  
  cat(sprintf("=== %s (%d pairs, %d not played) ===\n",
              conf_name, nrow(res), n_missing))
  cat(sprintf("  %-25s %4s %4s  %-25s\n", "Team 1", "W", "W", "Team 2"))
  cat(sprintf("  %s\n", strrep("-", 62)))
  
  res |>
    dplyr::mutate(
      line = sprintf("  %-25s %4s %4s  %-25s", t1, t1_wins, t2_wins, t2)
    ) |>
    dplyr::pull(line) |>
    cat(sep = "\n")
  
  cat("\n\n")
})

#### GT table 

dummy_team <- "Alabama"
team_cols <- c("sec_t1", "sec_t2", "acc_t1", "acc_t2", "sun_t1", "sun_t2", "bigten_t1", "bigten_t2")

ncaat_rendered <- ncaat |>
  # 1. Clean trailing spaces and ensure characters
  dplyr::mutate(
    across(all_of(team_cols), ~ stringr::str_trim(as.character(.x)))
  ) |>
  
  # 2 & 3. Flag missing/empty rows AND safely force-fill with dummy team
  dplyr::mutate(
    across(all_of(team_cols), ~ is.na(.x) | .x == "", .names = "{.col}_is_na"),
    across(all_of(team_cols), ~ dplyr::if_else(is.na(.x) | .x == "", dummy_team, .x))
  ) |>
  
  # Clean up results formatting
  dplyr::mutate(
    across(
      ends_with("result"), 
      ~ dplyr::case_match(.x,
                          "-------" ~ "—",
                          "--"      ~ "—",
                          .default  = .x
      )
    )
  ) |>
  
  # 4. Run the cbbplotR logo conversions
  cbbplotR::gt_cbb_teams(sec_t1, sec_t1, include_name = FALSE) |>
  cbbplotR::gt_cbb_teams(sec_t2, sec_t2, include_name = FALSE) |>
  cbbplotR::gt_cbb_teams(acc_t1, acc_t1, include_name = FALSE) |>
  cbbplotR::gt_cbb_teams(acc_t2, acc_t2, include_name = FALSE) |>
  cbbplotR::gt_cbb_teams(sun_t1, sun_t1, include_name = FALSE) |>
  cbbplotR::gt_cbb_teams(sun_t2, sun_t2, include_name = FALSE) |>
  cbbplotR::gt_cbb_teams(bigten_t1, bigten_t1, include_name = FALSE) |>
  cbbplotR::gt_cbb_teams(bigten_t2, bigten_t2, include_name = FALSE) |>
  
  # 5. Elegantly revert dummy logos back to empty strings using across()
  dplyr::mutate(
    across(
      all_of(team_cols), 
      ~ dplyr::if_else(pick(paste0(cur_column(), "_is_na"))[[1]], "", .x)
    )
  ) |>
  
  # 6. Drop the temporary flag columns
  dplyr::select(-ends_with("_is_na"))


# add header 
baseball_header <- glue::glue(
  "<div style='display: flex; justify-content: space-between; align-items: center; font-family: sans-serif;'>
     <div>
       <img src='https://a3.espncdn.com/combiner/i?img=%2Fredesign%2Fassets%2Fimg%2Ficons%2FESPN%2Dicon%2Dbaseball.png'
       style='height: 60px; width: auto; vertical-align: middle;'>
     </div>
     <div style='flex-grow:1; margin-left: 20px; margin-right: 20px;'>
       <span style='display: block; font-weight: 800; text-align: center; font-size: 24px; color: #111111; letter-spacing: -0.5px;'>Unplayed Conference Matchups Amongst 2026 NCAA Tournament Teams</span>
       <span style='display: block; text-align: center; font-size: 14px; color: #444444; margin-top: 4px; font-weight: normal;'>
         Matchups between teams from the <em>same</em> conference that <br>have <em>not</em> played each other yet this season. 
       </span>
     </div>
     <div>
       <img src='https://img.boostsport.ai/boost-cms/NCAACWS.jpg'
       style='height: 60px; width: auto; vertical-align: middle;'>
     </div>
   </div>
  <br>"
)

ncaa_b <- readr::read_csv("ncaa_b.csv")
  
# 7. Render the final table
ncaa_b |>
  dplyr::mutate(
    sec_1 = "",
    sec_col = paste0(sec_t1, " ", sec_t2),
    acc_col = paste0(acc_t1, " ", acc_t2),
    bigtw_col = paste0(bigtw_t1, " ", bigtw_t2),
    bigtw_blank = "",
    sun_col = paste0(sun_t1, " ", sun_t2),
    big_col = paste0(bigten_t1, " ", bigten_t2),
    big_blank = ""
  ) |> 
  dplyr::mutate(across(ends_with("_col"), ~ ifelse(.x == "NA NA", "", .x))) |>
  dplyr::select(
    sec_1, sec_col, sec_blank,
    acc_col, acc_blank,
    bigtw_col, bigtw_blank,
    sun_col, sun_blank,
    big_col, big_blank
  ) |> 
  gt::gt() |>
  gtUtils::gt_theme_savant() |> 
  gt::fmt_markdown(columns = ends_with(c("_col"))) |> 
  gt::cols_align(align = "center",
                 columns = gt::everything()) |> 
  gt::tab_header(title = gt::html(baseball_header)) |>
  gt::tab_source_note(source_note = gt::html("<hr>
    <div style='font-size: 11px; color: #444444; line-height: 1.5; font-family: sans-serif; padding-top: 10px;'>
      <b>SEC</b>: 16 league teams | 19 missed matchups among the 12 tournament teams.<br>
      <b>ACC</b>: 16 league teams | 9 missed matchups among the 9 tournament teams.<br>
      <b>Big 12</b>: 14 league teams | 2 missed matchups among the 6 tournament teams.<br>
      <b>Sun Belt</b>: 14 league teams | 2 missed matchups among the 5 tournament teams.<br>
      <b>Big Ten</b>: 17 league teams | 1 missed matchup among the 4 tournament teams.<br>
      <b>CUSA (3 bids) and Big West (2 bids)</b> had all tournament teams play each other.</span>
      <p style='margin-top: 8px; border-top: 1px solid #dddddd; padding-top: 6px; font-size: 10px; color: #333333;'>
        Table by Chris at Bless your chart | data via pearatings.com and ncaa.org | May 27, 2026
      </p>
    </div>")) |>
  gt::tab_spanner(
    columns = starts_with("sec_"),
    label = gt::html("<span style='font-weight: bold; font-size: 15px;'>SEC: 12 Bids</span><br><span style='color: #888888; font-size: 11px;'>19 missed <br>matchups</span>")
  ) |> 
  gt::tab_spanner(
    columns = starts_with("acc_"),
    label = gt::html("<span style='font-weight: bold; font-size: 15px;'>ACC: 9 Bids</span><br><span style='color: #888888; font-size: 11px;'>9 missed <br>matchups</span>")
  ) |> 
  gt::tab_spanner(
    columns = starts_with("bigtw_"),
    label = gt::html("<span style='font-weight: bold; font-size: 15px;'>Big 12: 6 Bids</span><br><span style='color: #888888; font-size: 11px;'>2 missed <br>matchups</span>")
  ) |> 
  gt::tab_spanner(
    columns = starts_with("sun_"),
    label = gt::html("<span style='font-weight: bold; font-size: 15px;'>Sun Belt: 5 Bids</span><br><span style='color: #888888; font-size: 11px;'>2 missed <br>matchups</span>")
  ) |> 
  gt::tab_spanner(
    columns = starts_with("big_"),
    label = gt::html("<span style='font-weight: bold; font-size: 15px;'>Big Ten: 4 Bids</span><br><span style='color: #888888; font-size: 11px;'>1 missed <br>matchup</span>")
  ) |> 
  gt::tab_options(
    table.additional_css = "th.gt_column_spanner { border-bottom-style: none !important; }"
  ) |>
  gt::cols_label(
    sec_1 = "",
    sec_col = "",
    sec_blank = "",
    acc_col = "",
    acc_blank = "",
    bigtw_col = "",
    bigtw_blank = "",
    sun_col = "",
    sun_blank = "",
    big_col = "",
    big_blank = ""
  ) |>
  gt::sub_missing(
    missing_text = ""
  ) |> 
  gt::tab_options(table.width = gt::px(665)) -> ncaab_tbl 

gt_save_crop(
  ncaab_tbl,
  file = "ncaab_tbl.png",
  whitespace = 40,
  bg = "white"
)  
