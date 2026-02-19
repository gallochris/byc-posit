cfb_team_tiers <- function(data,
                           title = "CFB Team Tiers, 2021 as of Week 6",
                           subtitle = "created with the #cfbplotR Tiermaker",
                           caption = NULL,
                           tier_desc = c("1" = "Playoff",
                                         "2" = "Very Good",
                                         "3" = "Medium",
                                         "4" = "Bad",
                                         "5" = "What are they doing?",
                                         "6" = "",
                                         "7" = ""),
                           presort = FALSE,
                           alpha = 0.8,
                           width = 0.075,
                           no_line_below_tier = NULL,
                           devel = FALSE){

  rlang::check_installed("sjmisc", "to build the cfbplotR team tiers.")

  required_vars <- c("tier_no", "team")

  if (!all(required_vars %in% names(data))){
    cli::cli_abort("The data frame {.var data} has to include the variables {.var {required_vars}}!")
  }

  bg <- "#1e1e1e"
  lines <- "#e0e0e0"

  tiers <- sort(unique(data$tier_no))
  tierlines <- tiers[!tiers %in% no_line_below_tier] + 0.5
  tierlines <- c(min(tiers) - 0.5, tierlines)

  if (isTRUE(presort)){
    data <- data %>%
      dplyr::group_by(.data$tier_no) %>%
      dplyr::arrange(.data$team) %>%
      dplyr::mutate(tier_rank = 1:dplyr::n()) %>%
      dplyr::ungroup()
  }

  if (!"tier_rank" %in% names(data)){
    data <- data %>%
      dplyr::group_by(.data$tier_no) %>%
      dplyr::mutate(tier_rank = 1:dplyr::n()) %>%
      dplyr::ungroup()
  }

  data$team <- cfbplotR::clean_school_names(as.character(data$team))

  p <- ggplot2::ggplot(data, ggplot2::aes(y = .data$tier_no, x = .data$tier_rank)) +
    ggplot2::geom_hline(yintercept = tierlines, color = lines)

  if(isFALSE(devel)) p <- p + cfbplotR::geom_cfb_logos(ggplot2::aes(team = .data$team), width = width, alpha = alpha)
  if(isTRUE(devel))p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$team), color = "white")

  p <- p +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = 0.1),
      limits = rev(c(min(tiers) - 0.5, max(tiers) + 0.5)),
      breaks = rev(tiers),
      labels = function(x) sjmisc::word_wrap(tier_desc[x], 15),
      trans = "reverse"
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_minimal(base_size = 11.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(color = "white", face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "#8e8e93"),
      plot.caption = ggplot2::element_text(color = "#8e8e93", hjust = 1),
      plot.title.position = "plot",
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = "white", face = "bold", size = ggplot2::rel(1.1)),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = bg, color = bg),
      panel.background = ggplot2::element_rect(fill = bg, color = bg)
    ) +
    NULL

  p
}
