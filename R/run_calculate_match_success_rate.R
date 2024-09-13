run_calculate_match_success_rate <- function(config) {
  config <- load_config(config)

  dir_raw <- get_raw_dir(config)
  dir_matched <- get_matched_dir(config)

  matching_use_own_sector_classification <- get_use_maunal_sector_classification(config)
  if (matching_use_own_sector_classification) {
    path_own_sector_classification <- get_manual_sector_classification_path(config)
  }

  by_group <- get_aggregate_alignment_metric_by_group(config)
  by_group <- check_and_prepare_by_group(by_group)
  by_group_ext <- if (is.null(by_group)) { "_meta" } else { paste0("_", by_group) }

  match_success_rate_plot_width <- get_match_plot_width(config)
  match_success_rate_plot_height <- get_match_plot_height(config)
  match_success_rate_plot_units <- get_match_plot_units(config)
  match_success_rate_plot_resolution <- get_match_plot_resolution(config)

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
  stop_if_no_files_found(list_raw, dir_raw, "dir_raw", "raw loan book CSVs")

  if (is.null(by_group) || by_group != "group_id") {
    raw_lbk <- readr::read_csv(
      file = file.path(dir_raw, list_raw),
      col_types = col_types_raw,
      col_select = dplyr::all_of(c(by_group, col_select_raw)),
      id = "group_id"
    )
  } else {
    raw_lbk <- readr::read_csv(
      file = file.path(dir_raw, list_raw),
      col_types = col_types_raw,
      col_select = dplyr::all_of(c(col_select_raw)),
      id = "group_id"
    )
  }

  raw_lbk <- raw_lbk %>%
    dplyr::mutate(
      group_id = gsub(glue::glue("{dir_raw}/"), "", .data$group_id),
      group_id = gsub(".csv", "", .data$group_id)
    )

  ## load matched prioritized loan books----
  list_matched_prioritized <- list.files(path = dir_matched, pattern = "^matched_prio_.*csv$")
  stop_if_no_files_found(list_matched_prioritized, dir_matched, "dir_matched", "matched prioritized loan book CSVs")

  matched_prioritized <- readr::read_csv(
    file = file.path(dir_matched, list_matched_prioritized),
    col_types = col_types_matched_prioritized,
    col_select = dplyr::all_of(c(by_group, col_select_matched_prioritized))
  )

  # add helper column to facilitate calculation of meta results----
  # TODO: decide if this should be removed from outputs
  if (is.null(by_group)) {
    by_group <- "meta"
    raw_lbk <- raw_lbk %>%
      dplyr::mutate(meta = "meta")
    matched_prioritized <- matched_prioritized %>%
      dplyr::mutate(meta = "meta")
  }

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
      cli::cli_abort(
        message = c(
          "x" = "Number of sector classification systems across all loan books is > 1.",
          "i" = "You can only use one sector classification at the same time.",
          "i" = "Your raw loan books use {length(sector_classifications_used)} different types of sector classifications."
        )
      )
    }

    sector_classification_system <- r2dii.data::sector_classifications %>%
      dplyr::filter(.data$code_system == .env$sector_classifications_used)
  }

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
  } else {
    loans_to_remove <- NULL
  }

  lbk_match_success_rate <- calculate_match_success_rate(
    raw_lbk = raw_lbk,
    matched_prioritized = matched_prioritized,
    sector_classification_system = sector_classification_system,
    misclassfied_loans = loans_to_remove,
    by_group = by_group
  )

  # write to csv
  lbk_match_success_rate %>%
    readr::write_csv(
      file = file.path(dir_matched, paste0("lbk_match_success_rate", by_group_ext, ".csv")),
      na = ""
    )

  # prepare match success data for plotting----
  data_lbk_match_success_rate <- prep_match_success_rate(
    data = lbk_match_success_rate,
    by_group = by_group
  )

  # plot match success rate----
  plot_match_success_currency <- unique(raw_lbk$loan_size_outstanding_currency)

  ## plot relative match success rates for individual loan books----
  plot_match_success_rate_rel_n_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "relative",
      match_success_type = "n",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, paste0("plot_match_success_rate_rel_n", by_group_ext, ".png")),
    plot = plot_match_success_rate_rel_n_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_rel_outstanding_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "relative",
      match_success_type = "outstanding",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, paste0("plot_match_success_rate_rel_outstanding", by_group_ext, ".png")),
    plot = plot_match_success_rate_rel_outstanding_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_rel_credit_limit_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "relative",
      match_success_type = "credit_limit",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, paste0("plot_match_success_rate_rel_credit_limit", by_group_ext, ".png")),
    plot = plot_match_success_rate_rel_credit_limit_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  ## plot absolute match success rates for individual loan books----
  plot_match_success_rate_abs_n_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "absolute",
      match_success_type = "n",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, paste0("plot_match_success_rate_abs_n", by_group_ext, ".png")),
    plot = plot_match_success_rate_abs_n_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_abs_outstanding_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "absolute",
      match_success_type = "outstanding",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, paste0("plot_match_success_rate_abs_outstanding", by_group_ext, ".png")),
    plot = plot_match_success_rate_abs_outstanding_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )

  plot_match_success_rate_abs_credit_limit_ind <- data_lbk_match_success_rate %>%
    plot_match_success_rate(
      metric_type = "absolute",
      match_success_type = "credit_limit",
      currency = plot_match_success_currency,
      by_group = by_group
    )

  ggplot2::ggsave(
    filename = file.path(dir_matched, paste0("plot_match_success_rate_abs_credit_limit", by_group_ext, ".png")),
    plot = plot_match_success_rate_abs_credit_limit_ind,
    width = match_success_rate_plot_width,
    height = match_success_rate_plot_height,
    units = match_success_rate_plot_units,
    dpi = match_success_rate_plot_resolution
  )
}

calculate_match_success_rate <- function(raw_lbk,
                                         matched_prioritized,
                                         sector_classification_system,
                                         misclassfied_loans = NULL,
                                         by_group) {
  # combine data----
  # add sectors to raw loan books
  raw_lbk_with_sectors <- add_sectors_to_raw_lbk(
    raw_lbk = raw_lbk,
    sector_classification_system = sector_classification_system
  )

  lbk_match_success <- combine_raw_and_matched_loan_books(
    raw_lbk_with_sectors = raw_lbk_with_sectors,
    matched_prioritized = matched_prioritized,
    by_group = by_group
  )

  ## remove misclassified loans----
  # optional: manually exclude loans from the match success calculation
  # this is intended to allow excluding loans that are misclassified as in scope,
  # but research shows that the company is not actually in scope
  if (!is.null(misclassfied_loans)) {
    lbk_match_success <- dplyr::anti_join(
      x = lbk_match_success,
      y = misclassfied_loans,
      by = c("id_loan", "group_id")
    )
  }

  # calculate match success rate----
  lbk_match_success_rate <- add_match_success_rate(
    lbk_match_success,
    by_group = by_group
  )
}

add_sectors_to_raw_lbk <- function(raw_lbk, sector_classification_system) {

  raw_lbk_with_sectors <- dplyr::left_join(
    x = raw_lbk,
    y = sector_classification_system,
    by = c(
      "sector_classification_system" = "code_system",
      "sector_classification_direct_loantaker" = "code"
    )
  )

  raw_lbk_with_sectors <- raw_lbk_with_sectors %>%
    dplyr::mutate(
      sector = dplyr::if_else(
        is.na(.data[["sector"]]),
        "not in scope",
        .data[["sector"]]
      ),
      borderline = dplyr::if_else(
        is.na(.data[["sector"]]),
        FALSE,
        .data[["borderline"]]
      )
    )
}

combine_raw_and_matched_loan_books <- function(raw_lbk_with_sectors,
                                               matched_prioritized,
                                               by_group) {
  # join raw and matched loan books, matching on all common columns, but using the
  # financial sector from the raw loan book to match the production sector.
  # this simulates matching with the option by_sector = TRUE
  all_sectors <- unique(r2dii.data::sector_classifications$sector)

  # recreate initial id_loan format for joining
  matched_prioritized <- matched_prioritized %>%
    dplyr::select(-"sector") %>%
    dplyr::mutate(
      id_loan = gsub(paste0("_", .data[["group_id"]], collapse = "|"), "", .data[["id_loan"]])
    ) %>%
    dplyr::mutate(
      id_loan = gsub(paste0("_", .env$all_sectors, collapse="|"), "", .data[["id_loan"]])
    )

  if (by_group != "group_id") {
    matched_prioritized <- matched_prioritized %>%
      dplyr::select(-"group_id")
  }

  # use left_join so that unmatched loans are properly accounted for
  lbk_match_success <- dplyr::left_join(
    x = raw_lbk_with_sectors,
    y = matched_prioritized,
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
      by_group,
      "sector" = "sector_abcd",
      "borderline"
    )
  )

  lbk_match_success <- lbk_match_success %>%
    dplyr::mutate(
      matched = dplyr::case_when(
        .data[["score"]] == 1 ~ "Matched",
        is.na(.data[["score"]]) ~ "Not matched",
        TRUE ~ "Not matched"
      ),
      # unmatched borderline loans are considered not in scope, as they would
      # otherwise increase the potential exposure wrongly and artificially without
      # there being a realistic way to match that exposure
      sector = dplyr::case_when(
        .data[["borderline"]] & .data[["matched"]] == "Not matched" ~ "not in scope",
        TRUE ~ .data[["sector"]]
      )
    )
}

add_match_success_rate <- function(data,
                                   by_group) {
  data <- data %>%
    dplyr::mutate(
      total_n = dplyr::n(),
      total_outstanding = sum(.data[["loan_size_outstanding"]], na.rm = TRUE),
      total_credit_limit = sum(.data[["loan_size_credit_limit"]], na.rm = TRUE),
      .by = dplyr::all_of(c(by_group, "sector"))
    ) %>%
    dplyr::summarise(
      match_n = dplyr::n(),
      match_outstanding = sum(.data[["loan_size_outstanding"]], na.rm = TRUE),
      match_credit_limit = sum(.data[["loan_size_credit_limit"]], na.rm = TRUE),
      .by = dplyr::all_of(c(by_group, "sector", "matched", "total_n", "total_outstanding", "total_credit_limit"))
    ) %>%
    dplyr::mutate(
      match_success_rate_rel = .data[["match_n"]] / .data[["total_n"]],
      match_success_outstanding_rel = .data[["match_outstanding"]] / .data[["total_outstanding"]],
      match_success_credit_limit_rel = .data[["match_credit_limit"]] / .data[["total_credit_limit"]]
    )

  data <- data %>%
    dplyr::select(
      dplyr::all_of(
        c(
          by_group,
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
      .data[[by_group]],
      .data[["sector"]],
      .data[["matched"]]
    )
}

prep_match_success_rate <- function(data,
                                    by_group) {
  # prepare match success data for plotting----
  data <- data %>%
    dplyr::select(
      -dplyr::starts_with("total")
    ) %>%
    tidyr::pivot_longer(
      cols = -c(
        by_group,
        "sector",
        "matched"
      ),
      names_to = "match_success_type",
      values_to = "match_success_rate"
    ) %>%
    dplyr::mutate(
      metric_type = dplyr::if_else(
        grepl("_rel$", .data[["match_success_type"]]),
        "relative",
        "absolute"
      ),
      match_success_type = dplyr::case_when(
        grepl("outstanding", .data[["match_success_type"]]) ~ "outstanding",
        grepl("credit_limit", .data[["match_success_type"]]) ~ "credit_limit",
        TRUE ~ "n"
      )
    )
}

