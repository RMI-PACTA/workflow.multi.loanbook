run_calculate_match_success_rate <- function() {
  # load config----
  config_dir <- config::get("directories")

  dir_raw <- config_dir$dir_raw
  dir_matched <- config_dir$dir_matched

  config_matching <- config::get("matching")

  matching_use_own_sector_classification <- config_matching$own_sector_classification$use_own_sector_classification
  if (matching_use_own_sector_classification) {
    dir_own_sector_classification <- config_matching$own_sector_classification$dir_own_sector_classification
    filename_own_sector_classification <- config_matching$own_sector_classification$filename_own_sector_classification
    path_own_sector_classification <- file.path(dir_own_sector_classification, filename_own_sector_classification)
  }

  config_match_success_rate <- config::get("match_success_rate")

  match_success_rate_plot_width <- config_match_success_rate$plot_width
  match_success_rate_plot_height <- config_match_success_rate$plot_height
  match_success_rate_plot_units <- config_match_success_rate$plot_units
  match_success_rate_plot_resolution <- config_match_success_rate$plot_resolution

  # validate config values----
  stop_if_not_length(dir_raw, 1L)
  stop_if_not_inherits(dir_raw, "character")
  stop_if_dir_not_found(dir_raw, desc = "Raw loanbook")

  stop_if_not_length(dir_matched, 1L)
  stop_if_not_inherits(dir_matched, "character")
  stop_if_dir_not_found(dir_matched, desc = "Matched loanbook")

  stop_if_not_length(matching_use_own_sector_classification, 1L)
  stop_if_not_inherits(matching_use_own_sector_classification, "logical")

  # path to own sector classification only required if boolean TRUE
  if (matching_use_own_sector_classification) {
    stop_if_not_length(path_own_sector_classification, 1L)
    stop_if_not_inherits(path_own_sector_classification, "character")
    stop_if_file_not_found(path_own_sector_classification, desc = "Manual sector classification")
  }

  stop_if_not_length(match_success_rate_plot_width, 1L)
  stop_if_not_inherits(match_success_rate_plot_width, "integer")

  stop_if_not_length(match_success_rate_plot_height, 1L)
  stop_if_not_inherits(match_success_rate_plot_height, "integer")

  stop_if_not_length(match_success_rate_plot_units, 1L)
  stop_if_not_inherits(match_success_rate_plot_units, "character")

  stop_if_not_length(match_success_rate_plot_resolution, 1L)
  stop_if_not_inherits(match_success_rate_plot_resolution, "integer")

  # load data----

  ## load raw loan books----
  list_raw <- list.files(dir_raw)[grepl("csv$", list.files(dir_raw))]

  if (length(list_raw) == 0) {
    stop(glue::glue("No raw loan book csvs found in {dir_raw}. Please check your project setup!"))
  }

  raw_lbk <- readr::read_csv(
    file = file.path(dir_raw, list_raw),
    col_types = col_types_raw,
    id = "group_id"
  ) %>%
    dplyr::mutate(
      group_id = gsub(glue::glue("{dir_raw}/"), "", .data$group_id),
      group_id = gsub(".csv", "", .data$group_id)
    )

  ## load matched prioritized loan books----
  list_matched_prioritized <- list.files(dir_matched)[grepl("^matched_prio_.*csv$", list.files(dir_matched))]

  if (length(list_matched_prioritized) == 0) {
    stop(glue::glue("No matched prioritized loan book csvs found in {dir_matched}. Please check your project setup!"))
  }

  matched_prioritized <- readr::read_csv(
    file = file.path(dir_matched, list_matched_prioritized),
    col_types = col_types_matched_prioritized,
    col_select = dplyr::all_of(col_select_matched_prioritized)
  )

  ## load classification system----
  if (matching_use_own_sector_classification) {
    sector_classification_system <- readr::read_csv(
      file = path_own_sector_classification,
      col_types = col_types_sector_classification,
      col_select = dplyr::all_of(col_select_sector_classification)
    )
  } else {
    sector_classifications_used <- unique(raw_lbk$sector_classification_system)

    if (length(sector_classifications_used) != 1) {
      stop(
        glue::glue(
          "Number of sector classification systems across all loan books must be 1.
          Your raw loan books use {length(sector_classifications_used)} different
          types of sector classifications. Please choose one!"
        )
      )
    }

    sector_classification_system <- r2dii.data::sector_classifications %>%
      dplyr::filter(.data$code_system == .env$sector_classifications_used)
  }

  # combine data----
  # add sectors to raw loan books
  raw_lbk_with_sectors <- raw_lbk %>%
    dplyr::left_join(
      sector_classification_system,
      by = c(
        "sector_classification_system" = "code_system",
        "sector_classification_direct_loantaker" = "code"
      )
    ) %>%
    dplyr::mutate(
      sector = dplyr::if_else(
        is.na(.data$sector),
        "not in scope",
        .data$sector
      ),
      borderline = dplyr::if_else(
        is.na(.data$sector),
        FALSE,
        .data$borderline
      )
    )

  # join raw and matched loan books, matching on all common columns, but using the
  # financial sector from the raw loan book to match the production sector.
  # this simulates matching with the option by_sector = TRUE
  matched_prioritized <- matched_prioritized %>%
    dplyr::select(-"sector")

  lbk_match_success <- raw_lbk_with_sectors %>%
    dplyr::left_join(
      matched_prioritized,
      by = c(
        "id_direct_loantaker",
        "name_direct_loantaker",
        "id_ultimate_parent",
        "name_ultimate_parent",
        "loan_size_outstanding",
        "loan_size_outstanding_currency",
        "loan_size_credit_limit",
        "loan_size_credit_limit_currency",
        "sector_classification_system",
        "sector_classification_direct_loantaker",
        "lei_direct_loantaker",
        "isin_direct_loantaker",
        "id_loan",
        "group_id",
        "sector" = "sector_abcd",
        "borderline"
      )
    ) %>%
    dplyr::mutate(
      matched = dplyr::case_when(
        .data$score == 1 ~ "Matched",
        is.na(.data$score) ~ "Not Matched",
        TRUE ~ "Not Matched"
      ),
      # unmatched borderline loans are considered not in scope, as they would
      # otherwise increase the potential exposure wrongly and artificially without
      # there being a realistic way to match that exposure
      sector = dplyr::case_when(
        .data$borderline == TRUE & .data$matched == "Not Matched" ~ "not in scope",
        TRUE ~ .data$sector
      )
    )

  ## remove misclassified loans----
  # optional: manually exclude loans from the match success calculation
  # this is intended to allow excluding loans that are misclassified as in scope,
  # but research shows that the company is not actually in scope
  if (file.exists(file.path(dir_matched, "loans_to_remove.csv"))) {
    loans_to_remove <- readr::read_csv(
      file = file.path(dir_matched, "loans_to_remove.csv"),
      col_types = readr::cols_only(
        id_loan = "c",
        group_id = "c"
      )
    )

    lbk_match_success <- lbk_match_success %>%
      dplyr::anti_join(
        loans_to_remove,
        by = c("id_loan", "group_id")
      )
  }

  # add meta loan book
  # TODO: unify use of meta loan book across repo
  lbk_match_success_meta <- lbk_match_success %>%
    dplyr::mutate(
      id_loan = paste0(.data$id_loan, "_", .data$group_id),
      group_id = "meta_loanbook",
    )

  lbk_match_success <- lbk_match_success %>%
    dplyr::bind_rows(lbk_match_success_meta)

  # calculate match success rate----
  lbk_match_success_rate <- lbk_match_success %>%
    dplyr::mutate(
      total_n = dplyr::n(),
      total_outstanding = sum(.data[["loan_size_outstanding"]], na.rm = TRUE),
      total_credit_limit = sum(.data[["loan_size_credit_limit"]], na.rm = TRUE),
      .by = c("group_id", "sector")
    ) %>%
    dplyr::summarise(
      match_n = dplyr::n(),
      match_outstanding = sum(.data[["loan_size_outstanding"]], na.rm = TRUE),
      match_credit_limit = sum(.data[["loan_size_credit_limit"]], na.rm = TRUE),
      .by = c("group_id", "sector", "matched", "total_n", "total_outstanding", "total_credit_limit")
    ) %>%
    dplyr::mutate(
      match_success_rate_rel = .data$match_n / .data$total_n,
      match_success_outstanding_rel = .data$match_outstanding / .data$total_outstanding,
      match_success_credit_limit_rel = .data$match_credit_limit / .data$total_credit_limit
    ) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "group_id",
          "sector",
          "matched",
          "match_n",
          "total_n",
          "match_success_rate_rel",
          "match_outstanding",
          "total_outstanding",
          "match_success_outstanding_rel",
          "match_credit_limit",
          "total_credit_limit",
          "match_success_credit_limit_rel"
        )
      )
    ) %>%
    dplyr::arrange(
      .data$group_id,
      .data$sector,
      .data$matched
    )

  # write to csv
  lbk_match_success_rate %>%
    readr::write_csv(
      file = file.path(dir_matched, "lbk_match_success_rate.csv")
    )

  # prepare match success data for plotting----
  data_lbk_match_success_rate <- lbk_match_success_rate %>%
    dplyr::select(
      -dplyr::starts_with("total")
    ) %>%
    tidyr::pivot_longer(
      cols = -c(
        "group_id",
        "sector",
        "matched"
      ),
      names_to = "match_success_type",
      values_to = "match_success_rate"
    ) %>%
    dplyr::mutate(
      metric_type = dplyr::if_else(
        grepl("_rel$", .data$match_success_type),
        "relative",
        "absolute"
      ),
      match_success_type = dplyr::case_when(
        grepl("outstanding", .data$match_success_type) ~ "outstanding",
        grepl("credit_limit", .data$match_success_type) ~ "credit_limit",
        TRUE ~ "n"
      )
    )

  # plot match success rate----
  plot_match_success_currency <- unique(raw_lbk$loan_size_outstanding_currency)

  ## plot relative match success rates for individual loan books----
  plot_match_success_rate_rel_n_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = FALSE,
      metric_type = "relative",
      match_success_type = "n",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_rel_n_individual.png"),
    plot = plot_match_success_rate_rel_n_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_rel_outstanding_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = FALSE,
      metric_type = "relative",
      match_success_type = "outstanding",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_rel_outstanding_individual.png"),
    plot = plot_match_success_rate_rel_outstanding_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_rel_credit_limit_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = FALSE,
      metric_type = "relative",
      match_success_type = "credit_limit",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_rel_credit_limit_individual.png"),
    plot = plot_match_success_rate_rel_credit_limit_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  ## plot relative match success rates for the aggregate loan book----
  plot_match_success_rate_rel_n_agg <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = TRUE,
      metric_type = "relative",
      match_success_type = "n",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_rel_n_aggregate.png"),
    plot = plot_match_success_rate_rel_n_agg,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_rel_outstanding_agg <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = TRUE,
      metric_type = "relative",
      match_success_type = "outstanding",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_rel_outstanding_aggregate.png"),
    plot = plot_match_success_rate_rel_outstanding_agg,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_rel_credit_limit_agg <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = TRUE,
      metric_type = "relative",
      match_success_type = "credit_limit",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_rel_credit_limit_aggregate.png"),
    plot = plot_match_success_rate_rel_credit_limit_agg,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  ## plot absolute match success rates for individual loan books----
  plot_match_success_rate_abs_n_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = FALSE,
      metric_type = "absolute",
      match_success_type = "n",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_abs_n_individual.png"),
    plot = plot_match_success_rate_abs_n_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_abs_outstanding_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = FALSE,
      metric_type = "absolute",
      match_success_type = "outstanding",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_abs_outstanding_individual.png"),
    plot = plot_match_success_rate_abs_outstanding_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_abs_credit_limit_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = FALSE,
      metric_type = "absolute",
      match_success_type = "credit_limit",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_abs_credit_limit_individual.png"),
    plot = plot_match_success_rate_abs_credit_limit_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  ## plot absolute match success rates for the aggregate loan book----
  plot_match_success_rate_abs_n_agg <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = TRUE,
      metric_type = "absolute",
      match_success_type = "n",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_abs_n_aggregate.png"),
    plot = plot_match_success_rate_abs_n_agg,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_abs_outstanding_agg <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = TRUE,
      metric_type = "absolute",
      match_success_type = "outstanding",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_abs_outstanding_aggregate.png"),
    plot = plot_match_success_rate_abs_outstanding_agg,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_abs_credit_limit_agg <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      aggregate = TRUE,
      metric_type = "absolute",
      match_success_type = "credit_limit",
      currency = plot_match_success_currency
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, "plot_match_success_rate_abs_credit_limit_aggregate.png"),
    plot = plot_match_success_rate_abs_credit_limit_agg,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )
}
