# Fetch Barttorvik data for a single date
get_barttorvik_data <- function(date, verbose = TRUE) {
  date_str <- format(as.Date(date), "%Y%m%d")
  url <- sprintf(
    "https://barttorvik.com/timemachine/team_results/%s_team_results.json.gz",
    date_str
  )
  
  if (verbose) {
    message(sprintf("Downloading: %s", url))
  }
  
  response <- httr::GET(url)
  
  # Check for successful response
  if (httr::status_code(response) != 200) {
    if (verbose) {
      warning(sprintf(
        "Failed to download data for %s. Status code: %d",
        date_str,
        httr::status_code(response)
      ))
    }
    return(NULL)
  }
  
  raw_content <- httr::content(response, "text", encoding = "UTF-8")
  
  # Check if we received HTML instead of JSON
  if (grepl("<!DOCTYPE html>", raw_content, fixed = TRUE)) {
    if (verbose) {
      warning(sprintf("Received HTML instead of JSON for %s (likely 404)", date_str))
    }
    return(NULL)
  }
  
  # Parse JSON with error handling
  data <- tryCatch(
    {
      parsed <- jsonlite::fromJSON(raw_content, simplifyDataFrame = TRUE)
      if (is.list(parsed) && !is.data.frame(parsed)) {
        parsed <- as.data.frame(parsed)
      }
      parsed
    },
    error = function(e) {
      if (verbose) {
        warning(sprintf("Failed to parse JSON for %s: %s", date_str, e$message))
      }
      return(NULL)
    }
  )
  
  return(data)
}

# Fetch Barttorvik data for a date range
get_barttorvik_data_range <- function(start_date, end_date, verbose = TRUE) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  all_data <- lapply(dates, function(d) {
    data <- get_barttorvik_data(d, verbose = verbose)
    if (!is.null(data)) {
      data <- as.data.frame(data)
      data$date <- d
      return(data)
    }
    return(NULL)
  })
  
  # Remove NULLs and combine
  all_data <- all_data[!vapply(all_data, is.null, logical(1))]
  
  if (length(all_data) == 0) {
    warning("No data found for any dates in the range")
    return(data.frame())
  }
  
  combined <- do.call(rbind, all_data)
  rownames(combined) <- NULL
  
  if (verbose) {
    message(sprintf(
      "Successfully combined data for %d dates into %d rows",
      length(all_data),
      nrow(combined)
    ))
  }
  
  return(combined)
}

# Fetch KenPom archive data for a date range
loop_kp_archive <- function(start_date = "2025-11-30",
                            end_date = "2026-01-29",
                            delay = 0.5,
                            token = NULL) {
  seq.Date(
    from = as.Date(start_date),
    to = as.Date(end_date),
    by = "day"
  ) |>
    as.character() |>
    purrr::map(\(date) {
      message(sprintf("Fetching KenPom archive for date: %s", date))
      
      result <- tryCatch(
        {
          kp_get_archive(date = date, token = token)
        },
        error = function(e) {
          warning(sprintf("Failed for %s: %s", date, e$message))
          NULL
        }
      )
      
      Sys.sleep(delay)
      result
    }) |>
    purrr::compact() |>
    dplyr::bind_rows()
}

# Main analysis ----------------------------------------------------------------

# Fetch Barttorvik data
bt_data <- get_barttorvik_data_range("2025-11-30", "2026-01-29", verbose = TRUE)

tejas_bt <- bt_data |>
  dplyr::filter(V2 == "Texas") |>
  dplyr::mutate(rtg = "torvik") |>
  dplyr::select(date, team = V2, rank = V1, rtg)

# Fetch KenPom data
kp <- loop_kp_archive()

tejas_kp <- kp |>
  dplyr::filter(TeamName == "Texas") |>
  dplyr::mutate(
    rtg = "kenpom",
    date = as.Date(ArchiveDate),
    rank = as.double(RankAdjEM)
  ) |>
  dplyr::select(date, team = TeamName, rank, rtg)

# NET data (assuming 'net' dataframe exists)
tejas_net <- net |>
  dplyr::filter(team == "Texas") |>
  dplyr::mutate(rtg = "NET") |>
  dplyr::select(date, team, rank = net, rtg)

# Combine all rating systems
tejas <- dplyr::bind_rows(
  dplyr::mutate(tejas_bt, rank = as.numeric(rank)),
  dplyr::mutate(tejas_kp, rank = as.numeric(rank)),
  dplyr::mutate(tejas_net, rank = as.numeric(rank))
)

# Create labels for first and last dates per rating system
labels <- tejas |>
  dplyr::group_by(rtg) |>
  dplyr::filter(date == min(date) | date == max(date)) |>
  dplyr::ungroup()