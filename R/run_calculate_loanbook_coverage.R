run_calculate_loanbook_coverage <- function(config) {
  config <- load_config(config)

  output_prepare_dir <- get_output_prepare_dir(config)
  output_prio_diagnostics_dir <- get_output_prio_diagnostics_dir(config)

  scenario_source_input <- get_scenario_source(config)
  start_year <- get_start_year(config)

  by_group <- get_by_group(config)
  by_group <- check_and_prepare_by_group(by_group)
  by_group_ext <- if (is.null(by_group)) { "_meta" } else { paste0("_", by_group) }

  # validate config values----
  stop_if_not_length(output_prio_diagnostics_dir, 1L)
  stop_if_not_inherits(output_prio_diagnostics_dir, "character")
  stop_if_dir_not_found(output_prio_diagnostics_dir, desc = "Output - Matched loanbooks")

  stop_if_not_length(scenario_source_input, 1L)
  stop_if_not_inherits(scenario_source_input, "character")

  stop_if_not_length(start_year, 1L)
  stop_if_not_inherits(start_year, "integer")

  # load data ----
  ## read abcd data----
  abcd <- readr::read_csv(
    file.path(output_prepare_dir, "abcd_final.csv"),
    col_select = dplyr::all_of(cols_abcd),
    col_types = col_types_abcd_final
  )
  # replace potential NA values with 0 in production
  abcd["production"][is.na(abcd["production"])] <- 0

  # filter to start year as we calculate coverage in start year
  abcd <- dplyr::filter(abcd, .data[["year"]] == .env[["start_year"]])

  ## read matched prioritized loan books----
  list_matched_prioritized <- list.files(path = output_prio_diagnostics_dir, pattern = "^matched_prio_.*csv$")
  stop_if_no_files_found(list_matched_prioritized, output_prio_diagnostics_dir, "output_prio_diagnostics_dir", "matched prioritized loan book CSVs")

  matched_prioritized <- readr::read_csv(
    file = file.path(output_prio_diagnostics_dir, list_matched_prioritized),
    col_types = col_types_matched_prioritized,
    col_select = dplyr::all_of(c(by_group, col_select_matched_prioritized))
  )

  # add helper column to facilitate calculation of meta results----
  # TODO: decide if this should be removed from outputs
  if (is.null(by_group)) {
    by_group <- "meta"
    matched_prioritized <- dplyr::mutate(.data = matched_prioritized, meta = "meta")
  }

  matched_companies <- matched_prioritized %>%
    dplyr::distinct(
      .data[[by_group]],
      .data[["name_abcd"]],
      .data[["sector_abcd"]],
      .data[["loan_size_outstanding"]],
      .data[["loan_size_outstanding_currency"]],
      .data[["score"]]
    )

  ## get required countries for region_select----
  region_isos_complete <- r2dii.data::region_isos

  region_isos_select <- region_isos_complete %>%
    dplyr::filter(
      .data[["source"]] == .env[["scenario_source_input"]]
    )

  # create summary of loan book coverage----
  # coverage of production by companies in loan books compared to total production
  # calculate summary stats for each available region

  available_regions <- unique(region_isos_select[["region"]])

  production_coverage_summary <- NULL

  for (region_i in available_regions) {
    countries_select_i <- region_isos_select %>%
      dplyr::filter(.data[["region"]] == .env[["region_i"]]) %>%
      dplyr::pull(.data[["isos"]]) %>%
      toupper()

    # get total production and average emission intensity for each relevant region for all companies
    production_coverage_summary_i <- abcd %>%
      dplyr::filter(.data[["plant_location"]] %in% .env[["countries_select_i"]]) %>%
      dplyr::summarise(
        emission_factor = stats::weighted.mean(
          x = .data[["emission_factor"]],
          w = .data[["production"]],
          na.rm = TRUE
        ),
        production = sum(.data[["production"]], na.rm = TRUE),
        .by = c(
          "company_id",
          "name_company",
          "lei",
          "is_ultimate_owner",
          "sector",
          "technology",
          "year",
          "production_unit",
          "emission_factor_unit"
        )
      )

    # add information on matched companies and corresponding exposures across all analyzed loan books
    production_coverage_summary_i <- production_coverage_summary_i %>%
      dplyr::left_join(
        matched_companies,
        by = c(
          "name_company" = "name_abcd",
          "sector" = "sector_abcd"
        ),
        relationship = "many-to-many"
      )

    # calculate summary statistics
    production_coverage_summary_i <- production_coverage_summary_i %>%
      dplyr::mutate(
        financed_production = dplyr::if_else(.data[["score"]] == 1, .data[["production"]], 0),
        matched_company = dplyr::if_else(.data[["score"]] == 1, .data[["name_company"]], NA_character_)
      ) %>%
      dplyr::mutate(
        matched_rows_company_sector = sum(.data[["score"]], na.rm = TRUE),
        .by = dplyr::all_of(
          c(
            by_group,
            "name_company",
            "sector"
          )
        )
      ) %>%
      dplyr::mutate(
        n_companies_total = dplyr::n_distinct(.data[["name_company"]], na.rm = TRUE),
        production_total = sum(.data[["production"]], na.rm = TRUE),
        .by = c("sector")
      ) %>%
      dplyr::summarise(
        total_exposure = sum(.data[["loan_size_outstanding"]] / .data[["matched_rows_company_sector"]], na.rm = TRUE),
        n_companies_matched = dplyr::n_distinct(.data[["matched_company"]], na.rm = TRUE),
        production_financed = sum(.data[["financed_production"]], na.rm = TRUE),
        .by = dplyr::all_of(
          c(
            by_group,
            "sector",
            "n_companies_total",
            "production_total"
          )
        )
      ) %>%
      dplyr::mutate(
        share_companies_matched = .data[["n_companies_matched"]] / .data[["n_companies_total"]],
        share_production_financed = .data[["production_financed"]] / .data[["production_total"]],
        region = .env[["region_i"]]
      )

    # remove entries that were not matched to any loan book AFTER calculating
    # summary statistics, so that totals are calculated correctly
    production_coverage_summary_i <- dplyr::filter(production_coverage_summary_i, !is.na(.data[[by_group]]))

    production_coverage_summary <- production_coverage_summary %>%
      dplyr::bind_rows(production_coverage_summary_i)

  }

  # format----
  production_coverage_summary <- production_coverage_summary %>%
    dplyr::select(
      dplyr::all_of(
        c(
          by_group,
          "region",
          "sector",
          "total_exposure",
          "n_companies_matched",
          "n_companies_total",
          "share_companies_matched",
          "production_financed",
          "production_total",
          "share_production_financed"
        )
      )
    )

  # save output to matched directory----
  production_coverage_summary %>%
    readr::write_csv(
      file.path(output_prio_diagnostics_dir, paste0("summary_statistics_loanbook_coverage", by_group_ext, ".csv")),
      na = ""
    )
}
