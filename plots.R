# TODO: move to `pacta.multi.loanbook.plot` and export
plot_match_success_rate <- function(data,
                                    aggregate = c(TRUE, FALSE),
                                    metric_type = c("absolute", "relative"),
                                    match_success_type = c("n", "outstanding", "credit_limit"),
                                    currency) {
  # validate inputs
  if (!inherits(data, "data.frame")) {
    stop("Argument data must be of class data.frame. Please check your inputs.")
  }
  expected_cols <- c(
    "group_id",
    "sector",
    "matched",
    "match_success_type",
    "match_success_rate",
    "metric_type"
  )
  if (!all(names(data) %in% expected_cols)) {
    stop(
      glue::glue(
        "Input data does not contain all expected columns. The following columns
        are missing: {paste(setdiff(expected_cols, data), collapse = ", ")}."
      )
    )
  }
  if (!length(currency) == 1) {
    stop("Argument currency must be of length 1. Please check your inputs.")
  }
  if (!inherits(currency, "character")) {
    stop("Argument currency must be of class character. Please check your inputs.")
  }

  # prepare data
  if (aggregate) {
    data <- data %>%
      dplyr::filter(.data$group_id == "meta_loanbook")
  } else {
    data <- data %>%
      dplyr::filter(.data$group_id != "meta_loanbook")
  }

  data <- data %>%
    dplyr::filter(.data$sector != "not in scope") %>%
    dplyr::filter(.data$metric_type == .env$metric_type) %>%
    dplyr::filter(.data$match_success_type == .env$match_success_type)

  # plot design
  fill_scale <- c(
    "Matched" = "#00c082",
    "Not Matched" = "#a63d57"
  )

  theme_match_success <- ggplot2::theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

  # plot description

  title <- r2dii.plot::to_title(glue::glue("{metric_type} Match Success Rate"))

  if (match_success_type == "n") {
    subtitle <- "number of loans by sector"
  } else if (match_success_type == "outstanding") {
    subtitle <- "loan size outstanding by sector"
  } else {
    subtitle <- "credit limit by sector"
  }

  if (aggregate) {
    subtitle <- r2dii.plot::to_title(glue::glue("aggregate {subtitle}"))
  } else {
    subtitle <- r2dii.plot::to_title(glue::glue("{subtitle} and loan book"))
  }

  if (match_success_type == "n") {
    y_label <- "Match success rate (n)"
  } else if (match_success_type == "outstanding") {
    y_label <- "Match success rate: loan size outstanding"
  } else {
    y_label <- "Match success rate: credit limit"
  }

  if (metric_type == "absolute") {
    y_label <- glue::glue("{y_label} (in {currency})")
  } else {
    y_label <- glue::glue("{y_label} (share of total)")
  }

  # plot
  plot <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = r2dii.plot::to_title(sector),
        y = match_success_rate,
        fill = matched
      )
    ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(values = fill_scale) +
    ggplot2::labs(
      x = "Sector",
      y = y_label,
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_bw() +
    theme_match_success

  if (!aggregate) {
    plot <- plot +
      ggplot2::facet_wrap(
        ~ group_id
      )
  }
  plot
}
