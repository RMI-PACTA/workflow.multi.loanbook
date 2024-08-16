run_calculate_loanbook_coverage <- function(config) {
  config <- load_config(config)

  abcd_dir <- get_abcd_dir(config)
  dir_matched <- get_matched_dir(config)
  dir_output <- dir_matched
  dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

  scenario_source_input <- get_scenario_source(config)
  start_year <- get_start_year(config)

  # validate config values----
  stop_if_not_length(dir_matched, 1L)
  stop_if_not_inherits(dir_matched, "character")
  stop_if_dir_not_found(dir_matched, desc = "Matched loanbooks")

  stop_if_not_length(scenario_source_input, 1L)
  stop_if_not_inherits(scenario_source_input, "character")

  stop_if_not_length(start_year, 1L)
  stop_if_not_inherits(start_year, "integer")

  # load data ----
  ## read abcd data----
  abcd <- readr::read_csv(
    file.path(abcd_dir, "abcd_final.csv"),
    col_select = dplyr::all_of(cols_abcd),
    col_types = col_types_abcd_final
  )
  # replace potential NA values with 0 in production
  abcd["production"][is.na(abcd["production"])] <- 0

  # filter to start year as we calculate coverage in start year
  abcd <- dplyr::filter(abcd, .data$year == .env$start_year)

  ## read matched prioritized loan books----
  list_matched_prioritized <- list.files(path = dir_matched, pattern = "^matched_prio_.*csv$")

  if (length(list_matched_prioritized) == 0) {
    stop(glue::glue("No matched prioritized loan book csvs found in {dir_matched}. Please check your project setup!"))
  }

  matched_prioritized <- readr::read_csv(
    file = file.path(dir_matched, list_matched_prioritized),
    col_types = col_types_matched_prioritized,
    col_select = dplyr::all_of(col_select_matched_prioritized)
  )

  matched_companies <- matched_prioritized %>%
    dplyr::distinct(
      .data$group_id,
      .data$name_abcd,
      .data$sector_abcd,
      .data$loan_size_outstanding,
      .data$loan_size_outstanding_currency,
      .data$score
    )

  ## get required countries for region_select----
  region_isos_complete <- r2dii.data::region_isos

  region_isos_select <- region_isos_complete %>%
    dplyr::filter(
      .data$source == .env$scenario_source_input
    )

  # create summary of loan book coverage----
  # coverage of production by companies in loan books compared to total production
  # calculate summary stats for each available region

  available_regions <- unique(region_isos_select$region)

  production_coverage_summary <- NULL

  for (region_i in available_regions) {
    countries_select_i <- region_isos_select %>%
      dplyr::filter(.data$region == .env$region_i) %>%
      dplyr::pull(.data$isos) %>%
      toupper()

    # get total production and average emission intensity for each relevant region for all companies
    production_coverage_summary_i <- abcd %>%
      dplyr::filter(.data$plant_location %in% .env$countries_select_i) %>%
      dplyr::summarise(
        emission_factor = stats::weighted.mean(
          x = .data$emission_factor,
          w = .data$production,
          na.rm = TRUE
        ),
        production = sum(.data$production, na.rm = TRUE),
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
        financed_production = dplyr::if_else(.data$score == 1, .data$production, 0),
        matched_company = dplyr::if_else(.data$score == 1, .data$name_company, NA_character_)
      ) %>%
      dplyr::mutate(
        matched_rows_company_sector = sum(.data$score, na.rm = TRUE),
        .by =c("group_id", "name_company", "sector")
      ) %>%
      dplyr::mutate(
        n_companies_total = dplyr::n_distinct(.data$name_company, na.rm = TRUE),
        production_total = sum(.data$production, na.rm = TRUE),
        .by = c("sector")
      ) %>%
      dplyr::summarise(
        total_exposure = sum(.data$loan_size_outstanding / .data$matched_rows_company_sector, na.rm = TRUE),
        n_companies_matched = dplyr::n_distinct(.data$matched_company, na.rm = TRUE),
        production_financed = sum(.data$financed_production, na.rm = TRUE),
        .by = c("group_id", "sector", "n_companies_total", "production_total")
      ) %>%
      dplyr::mutate(
        share_companies_matched = .data$n_companies_matched / .data$n_companies_total,
        share_production_financed = .data$production_financed / .data$production_total,
        region = .env$region_i
      )

    # remove entries that were not matched to any loan book AFTER calculating
    # summary statistics, so that totals are calculated correctly
    production_coverage_summary_i <- dplyr::filter(production_coverage_summary_i, !is.na(.data$group_id))

    production_coverage_summary <- production_coverage_summary %>%
      dplyr::bind_rows(production_coverage_summary_i)

  }

  # format----
  production_coverage_summary <- production_coverage_summary %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "group_id",
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
      file.path(dir_output, "summary_statistics_loanbook_coverage.csv"),
      na = ""
    )
}
