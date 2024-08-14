run_aggregate_alignment_metric <- function() {
  # set up project and load packages----
  library(dplyr)
  library(glue)
  library(pacta.multi.loanbook.analysis)
  library(r2dii.analysis)
  library(r2dii.data)

  # source helpers----
  source("expected_columns.R")

  # load config----
  config_dir <- config::get("directories")
  config_files <- config::get("file_names")
  config_project_parameters <- config::get("project_parameters")
  config_prepare_sector_split <- config::get("sector_split")
  config_aggregate_alignment_metric <- config::get("aggregate_alignment_metric")

  dir_abcd <- config_dir$dir_abcd
  dir_matched <- config_dir$dir_matched
  dir_output <- config_dir$dir_output
  dir_output_aggregated <- file.path(dir_output, "aggregated")
  apply_sector_split <- config_prepare_sector_split$apply_sector_split

  if (apply_sector_split) {
    dir_output_aggregated <- file.path(
      dir_output,
      config_prepare_sector_split$sector_split_type,
      "aggregated"
    )
  }

  dir.create(dir_output_aggregated, recursive = TRUE)

  path_scenario_tms <- file.path(config_dir$dir_scenario, config_files$filename_scenario_tms)
  path_scenario_sda <- file.path(config_dir$dir_scenario, config_files$filename_scenario_sda)

  scenario_source_input <- config_project_parameters$scenario_source
  scenario_select <- config_project_parameters$scenario_select
  region_select <- config_project_parameters$region_select
  start_year <- config_project_parameters$start_year
  time_frame <- config_project_parameters$time_frame

  by_group <- config_aggregate_alignment_metric$by_group
  if (by_group == "NULL") {by_group <- NULL}
  if (length(by_group) >= 1) {
    by_group <- gsub(" ", "", unlist(strsplit(by_group, split = ",")))
  }

  # load input data----
  region_isos_select <- r2dii.data::region_isos %>%
    dplyr::filter(
      .data$source == .env$scenario_source_input,
      .data$region %in% .env$region_select
    )

  scenario_input_tms <- readr::read_csv(
    path_scenario_tms,
    col_types = col_types_scenario_tms,
    col_select = dplyr::all_of(col_select_scenario_tms)
  )
  scenario_input_sda <- readr::read_csv(
    path_scenario_sda,
    col_types = col_types_scenario_sda,
    col_select = dplyr::all_of(col_select_scenario_sda)
  )

  abcd <- readr::read_csv(
    file.path(dir_abcd, "abcd_final.csv"),
    col_types = col_types_abcd_final,
    col_select = dplyr::all_of(cols_abcd)
  )

  # read matched and prioritized loan book----
  list_matched_prio <- list.files(dir_matched)[grepl("matched_prio_", list.files(dir_matched))]

  matched_prioritized <- NULL

  # combine all matched loan books into one object to loop over
  for (i in list_matched_prio) {
    matched_prioritized_i <- readr::read_csv(
      file.path(dir_matched, i),
      col_types = col_types_matched_prioritized
    )

    matched_prioritized <- matched_prioritized %>%
      dplyr::bind_rows(matched_prioritized_i)
  }

  # aggregate P4B alignment----

  ## set specifications----

  # for the calculation of the aggregate company alignment metric, we do not force companies
  # to enter a new market to build out hydro power or nuclear power, as this may
  # not be feasible for political and/or geographic reasons.
  # in the power sector, only renewables continues to follow the SMSP logic
  increasing_or_decreasing_aggregate_alignment <- r2dii.data::increasing_or_decreasing %>%
    dplyr::mutate(
      increasing_or_decreasing = dplyr::if_else(
        .data$technology %in% c("hydrocap", "nuclearcap"),
        "decreasing",
        .data$increasing_or_decreasing
      )
    )

  # define if technologies should be treated as build out or phase down in the
  # aggregation
  technology_direction <- scenario_input_tms %>%
    dplyr::filter(.data$year %in% c(.env$start_year, .env$start_year + .env$time_frame)) %>%
    dplyr::distinct(
      .data$scenario_source,
      .data$scenario,
      .data$sector,
      .data$technology,
      .data$region
    ) %>%
    dplyr::inner_join(r2dii.data::increasing_or_decreasing, by = c("sector", "technology")) %>%
    dplyr::mutate(
      directional_dummy = dplyr::if_else(
        .data$increasing_or_decreasing == "increasing",
        1,
        -1
      )
    ) %>%
    dplyr::select(-"increasing_or_decreasing")

  # remove non standard columns from matched_prioritzed when calling r2dii.analysis
  # TODO: check if this needs to be adjusted to remove other by_group columns
  matched_prio_non_standard_cols <- names(matched_prioritized)[!names(matched_prioritized) %in% col_standard_matched_prioritized]

  ## prepare TMS company level P4B results for aggregation----
  tms_result_for_aggregation <- r2dii.analysis::target_market_share(
    data = matched_prioritized %>%
      dplyr::select(-dplyr::all_of(matched_prio_non_standard_cols)),
    abcd = abcd,
    scenario = scenario_input_tms,
    region_isos = region_isos_select,
    by_company = TRUE,
    weight_production = FALSE,
    increasing_or_decreasing = increasing_or_decreasing_aggregate_alignment
  )

  ## aggregate TMS P4B results to company level alignment metric----
  # calculate aggregation for the loan book

  company_technology_deviation_tms <- tms_result_for_aggregation %>%
    pacta.multi.loanbook.analysis::calculate_company_tech_deviation(
      technology_direction = technology_direction,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      # bridge_tech = "gascap",
      time_frame = time_frame
    )

  company_technology_deviation_tms %>%
    readr::write_csv(
      file.path(dir_output_aggregated, "company_technology_deviation_tms.csv"),
      na = ""
    )

  company_alignment_net_tms <- company_technology_deviation_tms %>%
    pacta.multi.loanbook.analysis::calculate_company_aggregate_alignment_tms(
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      level = "net"
    )

  company_alignment_net_tms %>%
    readr::write_csv(
      file.path(dir_output_aggregated, "company_alignment_net_tms.csv"),
      na = ""
    )

  company_alignment_bo_po_tms <- company_technology_deviation_tms %>%
    pacta.multi.loanbook.analysis::calculate_company_aggregate_alignment_tms(
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      level = "bo_po"
    )

  company_alignment_bo_po_tms %>%
    readr::write_csv(
      file.path(dir_output_aggregated, "company_alignment_bo_po_tms.csv"),
      na = ""
    )

  ## prepare SDA company level P4B results for aggregation----
  sda_result_for_aggregation <- r2dii.analysis::target_sda(
    data = matched_prioritized %>%
      dplyr::select(-dplyr::all_of(matched_prio_non_standard_cols)),
    abcd = abcd,
    co2_intensity_scenario = scenario_input_sda,
    by_company = TRUE,
    region_isos = region_isos_select
  )

  sda_result_for_aggregation <- sda_result_for_aggregation %>%
    dplyr::filter(.data$year >= .env$start_year)

  ## aggregate SDA P4B results to company level alignment metric----
  company_alignment_net_sda <- sda_result_for_aggregation %>%
    pacta.multi.loanbook.analysis::calculate_company_aggregate_alignment_sda(
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      time_frame = time_frame
    )

  company_alignment_net_sda %>%
    readr::write_csv(
      file.path(dir_output_aggregated, "company_alignment_net_sda.csv"),
      na = ""
    )

  ## calculate sector and loan book level aggregate alignment based on company exposures in loan book----

  # the company level aggregate alignment metrics are then joined with the matched
  # loan book to derive some high level summary statistics on the loan book level
  company_alignment_net <- dplyr::bind_rows(company_alignment_net_tms, company_alignment_net_sda)

  # show exposures (n companies and loan size) by alignment with given scenario
  write_alignment_metric_to_csv <- function(data,
                                            output_dir,
                                            level,
                                            .by = NULL) {
    if (is.null(.by)) {
      by_group <- ""
    } else {
      by_group <- glue::glue("_by_{paste(.by, collapse = \"_\")}")
    }

    data$company %>%
      readr::write_csv(
        file = file.path(output_dir, glue::glue("company_exposure_{level}_aggregate_alignment{by_group}.csv")),
        na = ""
      )

    data$aggregate %>%
      readr::write_csv(
        file = file.path(output_dir, glue::glue("loanbook_exposure_{level}_aggregate_alignment{by_group}.csv")),
        na = ""
      )

  }

  # net
  aggregated_alignment_net <- company_alignment_net %>%
    pacta.multi.loanbook.analysis::aggregate_alignment_loanbook_exposure(
      matched = matched_prioritized,
      level = "net",
      .by = by_group
    )

  write_alignment_metric_to_csv(
    data = aggregated_alignment_net,
    output_dir = dir_output_aggregated,
    level = "net",
    .by = by_group
  )

  # buildout / phaseout
  aggregated_alignment_bo_po <- company_alignment_bo_po_tms %>%
    pacta.multi.loanbook.analysis::aggregate_alignment_loanbook_exposure(
      matched = matched_prioritized,
      level = "bo_po",
      .by = by_group
    )

  write_alignment_metric_to_csv(
    data = aggregated_alignment_bo_po,
    output_dir = dir_output_aggregated,
    level = "bo_po",
    .by = by_group
  )
}
