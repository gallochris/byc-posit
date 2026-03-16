# Scrape bracketmatrix.com -----------------------------------------------

url <- "http://www.bracketmatrix.com/"

page <- rvest::read_html(url)

raw_table <- page |>
  rvest::html_element("table") |>
  rvest::html_table(fill = TRUE)

bracket <- raw_table |>
  dplyr::slice(4:dplyr::n()) |>
  dplyr::filter(!is.na(X1)) |>
  dplyr::select(
    seed = X1, team = X2, conf = X3
  ) |>
  dplyr::add_row(seed = 11, team = "San Diego State", conf = "Mountain West") |>
  dplyr::add_row(seed = 11, team = "Auburn", conf = "SEC") |>
  dplyr::add_row(seed = 11, team = "Oklahoma", conf = "SEC") |>
  dplyr::add_row(seed = 11, team = "Indiana", conf = "Big Ten") |>
  dplyr::add_row(seed = 11, team = "New Mexico", conf = "Mountain West") |>
  dplyr::add_row(seed = 11, team = "Stanford", conf = "ACC")


# Normalize team names -----------------------------------------------------

name_fixes <- c(
  "Michigan State"       = "Michigan St.",
  "Iowa State"           = "Iowa St.",
  "St. Mary's (CA)"      = "Saint Mary's",
  "Miami (FLA.)"         = "Miami FL",
  "Utah State"           = "Utah St.",
  "Ohio State"           = "Ohio St.",
  "Central Florida"      = "UCF",
  "North Carolina State" = "North Carolina St.",
  "Long Island"          = "LIU Brooklyn",
  "Miami (Ohio)"         = "Miami OH",
  "North Dakota State"   = "North Dakota St.",
  "Portland State"       = "Portland St.",
  "Wright State"         = "Wright St.",
  "Tennessee State"      = "Tennessee St.",
  "Bethune-Cookman"      = "Bethune Cookman",
  "San Diego State"      = "San Diego St.",
  "McNeese State"        = "McNeese St.",
  "Sam Houston State"    = "Sam Houston St.",
  "California Baptist"   = "Cal Baptist",
  "Morgan State"         = "Morgan St.",
  "Grambling"            = "Grambling St.",
  "Norfolk State"        = "Norfolk St.",
  "NC-Wilmington"        = "UNC Wilmington",
  "E. Tennessee State"   = "East Tennessee St.",
  "Montana State"        = "Montana St.",
  "Kennesaw State"       = "Kennesaw St."
)


# Auto-bid teams (locked) --------------------------------------------------

auto_bid_teams <- c(
  "UMBC", "Duke", "Arizona", "St. John's",
  "Idaho", "High Point", "Hawaii", "Hofstra", "Wright St.", "Siena",
  "Akron", "Howard", "Northern Iowa", "Utah St.", "LIU Brooklyn",
  "Tennessee St.", "Lehigh", "Furman", "McNeese St.", "Prairie View A&M",
  "North Dakota St.", "Troy", "Cal Baptist", "Gonzaga", "Queens",
  "Kennesaw St.", "Pennsylvania", "Arkansas", "VCU", "Purdue", "South Florida"
)

# First Four ---------------------------------------------------------------

first_four <- c(
  "North Carolina St.", "Texas", "UMBC", "Howard",
  "Miami OH", "SMU", "Prairie View A&M", "Lehigh"
)

# Teams in both auto-bid and First Four (purple) ---------------------------

both_auto_and_four <- intersect(auto_bid_teams, first_four)
# "UMBC", "Howard", "Lehigh", "Prairie View A&M"

# First teams out (faded) --------------------------------------------------

first_out <- c(
  "San Diego St.", "Auburn", "Oklahoma", "Indiana", "New Mexico", "Stanford"
)


# Region assignments -------------------------------------------------------

east <- c("Duke", "Siena", "Ohio St.",
          "TCU", "St. John's",
          "Northern Iowa", "Kansas", "Cal Baptist",
          "Louisville", "South Florida", "Michigan St.",
          "North Dakota St.", "UCLA", "UCF",
          "Connecticut", "Furman")

south <- c("Florida", "Prairie View A&M", "Lehigh",
           "Clemson", "Iowa", "Vanderbilt", "McNeese St.",
           "Nebraska", "Troy", "North Carolina", "VCU",
           "Illinois", "Pennsylvania", "Saint Mary's", "Texas A&M",
           "Houston", "Idaho")

west <- c("Arizona", "LIU Brooklyn", "Villanova", "Utah St.",
          "Wisconsin", "High Point", "Arkansas", "Hawaii",
          "BYU", "Texas", "North Carolina St.", "Gonzaga", "Kennesaw St.",
          "Miami FL", "Missouri", "Purdue", "Queens")

midwest <- c("Michigan", "UMBC", "Howard", "Georgia", "Saint Louis",
             "Texas Tech", "Akron", "Alabama", "Hofstra",
             "Tennessee", "Miami OH", "SMU", "Virginia",
             "Wright St.", "Kentucky", "Santa Clara", "Iowa St.",
             "Tennessee St.")


# Build bracket_long -------------------------------------------------------

bracket_long <- bracket |>
  dplyr::mutate(
    team      = dplyr::recode(team, !!!name_fixes),
    team_sort = team,
    seed      = as.integer(seed),
    region = dplyr::case_when(
      team %in% east    ~ "East",
      team %in% south   ~ "South",
      team %in% west    ~ "West",
      team %in% midwest ~ "Midwest",
      TRUE              ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(region) | team_sort %in% first_out) |>
  cbbplotR::gt_cbb_teams(team, team, include_name = FALSE) |>
  dplyr::mutate(
    team_html = dplyr::case_when(
      team_sort %in% both_auto_and_four ~
        glue::glue(
          "<span style='display:inline-block; background:#e8d5f5; border:1px solid #7b2d8b;
           border-radius:4px; padding:1px 3px; vertical-align:middle;'>{team}</span>"
        ),
      team_sort %in% first_out ~
        glue::glue(
          "<span style='display:inline-block; opacity:0.35; border:1px solid transparent;
           border-radius:4px; padding:1px 3px; vertical-align:middle;'>{team}</span>"
        ),
      team_sort %in% auto_bid_teams ~
        glue::glue(
          "<span style='display:inline-block; background:#d4edda; border:1px solid #28a745;
           border-radius:4px; padding:1px 3px; vertical-align:middle;'>{team}</span>"
        ),
      team_sort %in% first_four ~
        glue::glue(
          "<span style='display:inline-block; background:#fff3cd; border:1px solid #ffc107;
           border-radius:4px; padding:1px 3px; vertical-align:middle;'>{team}</span>"
        ),
      TRUE ~
        glue::glue(
          "<span style='display:inline-block; border:1px solid transparent;
           border-radius:4px; padding:1px 3px; vertical-align:middle;'>{team}</span>"
        )
    )
  )


# Summarise by seed --------------------------------------------------------

bracket_summary <- bracket_long |>
  dplyr::mutate(
    region_f = factor(region, levels = c("East", "South", "West", "Midwest"))
  ) |>
  dplyr::arrange(seed, region_f, team_sort) |>
  dplyr::group_by(seed) |>
  dplyr::summarise(
    teams = paste(team_html, collapse = " "),
    .groups = "drop"
  )


# Build gt table -----------------------------------------------------------

custom_header <- glue::glue(
  "<div style='display: flex; justify-content: space-between; align-items: center;'>
    <div>
      <img src='https://a.espncdn.com/combiner/i?img=/redesign/assets/img/icons/ESPN-icon-basketball.png'
        style='height: 45px; width: auto; vertical-align: middle;'>
    </div>
    <div style='flex-grow:1; margin-left: 30px; margin-right: 30px'>
      <span style='display: block; font-weight: bold; text-align: center; font-size: 24px;'>
        2026 NCAA Tournament Teams by Seed
      </span>
      <span style='font-size: 14px; font-weight: normal; display: block; text-align: center;'>
        Teams are grouped by <b>region and seed line</b>.<br>
      </span>
    </div>
    <div>
      <img src='https://a.espncdn.com/combiner/i?img=/redesign/assets/img/icons/ESPN-icon-basketball.png'
        style='height: 45px; width: auto; vertical-align: middle;'>
    </div>
  </div><br>"
)

bracket_tbl <- bracket_summary |>
  gt::gt() |>
  gtUtils::gt_theme_savant() |>
  gt::fmt_markdown(columns = teams) |>
  gt::cols_label(
    seed  = "Seed",
    teams = "Teams"
  ) |>
  gt::tab_header(title = gt::html(custom_header)) |>
  gt::tab_source_note(gt::html(
    "<hr>
     data via bracketmatrix.com<br>
     <hr>
     <span style='display:inline-block; background:#d4edda; border:1px solid #28a745;
       border-radius:4px; padding:1px 3px; font-size:12px;'>Teams in green earned automatic bid</span><br><br>
     <span style='display:inline-block; background:#fff3cd; border:1px solid #ffc107;
       border-radius:4px; padding:1px 3px; font-size:12px;'>Yellow indicates team in the First Four</span><br><br>
     <span style='display:inline-block; background:#e8d5f5; border:1px solid #7b2d8b;
       border-radius:4px; padding:1px 3px; font-size:12px;'>Teams in purple earned automatic bid <em>and</em> will play in the First Four</span><br><br>
     Faded<span style='opacity:0.35; font-size:12px;'> teams</span> were listed as first teams out
     <hr>
     Table by Chris at Bless your Chart"
  )) |>
  gt::tab_options(
    table.font.size            = gt::px(14),
    heading.align              = "left",
    column_labels.font.weight  = "bold"
  ) |>
  gt::tab_style(
    style = list(gt::cell_borders(
      sides  = c("left", "right", "top", "bottom"),
      color  = "black",
      weight = gt::px(2)
    )),
    locations = list(
      gt::cells_body(),
      gt::cells_column_labels()
    )
  ) |>
  gt::tab_options(table.width = gt::px(575))


# Save ---------------------------------------------------------------------

gt_save_crop(bracket_tbl, file = "bracket_me.png", whitespace = 60, bg = "white")

