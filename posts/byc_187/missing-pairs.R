library(readr)
library(dplyr)
library(tidyr)
library(purrr)

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

# Build a set of all played matchups as a two-column tibble
played <- df |>
  dplyr::select(away = `Away Team`, home = `Home Team`) |>
  dplyr::bind_rows(
    df |> dplyr::select(away = `Home Team`, home = `Away Team`)
  ) |>
  dplyr::distinct()

did_play <- function(t1, t2) {
  played |>
    dplyr::filter(away == t1, home == t2) |>
    nrow() > 0
}

# For each conference, get all pairwise combos and filter to unplayed
find_missing <- function(teams, conf_name) {
  tidyr::expand_grid(t1 = teams, t2 = teams) |>
    dplyr::filter(t1 < t2) |>  # unique pairs only
    dplyr::mutate(
      played = purrr::map2_lgl(t1, t2, did_play),
      conference = conf_name
    ) |>
    dplyr::filter(!played) |>
    dplyr::select(conference, t1, t2)
}

missing_all <- purrr::imap(conferences, find_missing) |>
  purrr::list_rbind()

# Print results by conference
cat("=== Conference pairs that did NOT play each other ===\n\n")

purrr::iwalk(conferences, function(teams, conf_name) {
  n_teams <- length(teams)
  n_pairs <- choose(n_teams, 2)
  
  missing <- missing_all |>
    dplyr::filter(conference == conf_name)
  
  cat(sprintf("--- %s (%d teams, %d possible pairs) ---\n",
              conf_name, n_teams, n_pairs))
  
  if (nrow(missing) == 0) {
    cat("  All pairs played each other\n")
  } else {
    missing |>
      dplyr::mutate(line = sprintf("  %s vs %s", t1, t2)) |>
      dplyr::pull(line) |>
      cat(sep = "\n")
    cat("\n")
  }
  cat("\n")
})