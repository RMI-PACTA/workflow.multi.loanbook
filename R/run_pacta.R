run_pacta <- function() {
  # load packages----
  library(dplyr)
  library(pacta.multi.loanbook.analysis)
  library(r2dii.analysis)
  library(r2dii.data)
  library(r2dii.plot)


  # source helpers----
  source("expected_columns.R")
  source("plots.R")

  # load config----
  config <- config::get()

  input_path_scenario <- config$directories$dir_scenario
  input_dir_abcd <- config$directories$dir_abcd
  input_path_matched <- config$directories$dir_matched

  input_path_scenario_tms <- file.path(input_path_scenario, config$file_names$filename_scenario_tms)
  input_path_scenario_sda <- file.path(input_path_scenario, config$file_names$filename_scenario_sda)

  input_path_abcd <- file.path(input_dir_abcd, "abcd_final.csv")

  output_path <- config$directories$dir_output
  output_path_standard <- file.path(output_path, "standard")

  scenario_source_input <- config$project_parameters$scenario_source
  scenario_select <- config$project_parameters$scenario_select
  region_select <- config$project_parameters$region_select
  start_year <- config$project_parameters$start_year
  time_frame_select <- config$project_parameters$time_frame
  apply_sector_split <- config$sector_split$apply_sector_split
  if (is.null(apply_sector_split)) { apply_sector_split <- FALSE }
  sector_split_type_select <- config$sector_split$sector_split_type
  remove_inactive_companies <- config$prepare_abcd$remove_inactive_companies
  if (is.null(remove_inactive_companies)) { remove_inactive_companies <- FALSE }

  # if a sector split is applied, write results into a directory that states the type
  if (apply_sector_split) {
    output_path_standard <- file.path(output_path, sector_split_type_select, "standard")
  }

  dir.create(output_path_standard, recursive = TRUE)

  # TODO: add check if all files exist, resort to test files if not

  # load input data----
  region_isos_select <- r2dii.data::region_isos %>%
    dplyr::filter(
      .data$source == .env$scenario_source_input,
      .data$region %in% .env$region_select
    )

  scenario_input_tms <- readr::read_csv(
    input_path_scenario_tms,
    col_types = col_types_scenario_tms,
    col_select = dplyr::all_of(col_select_scenario_tms)
  )

  scenario_input_sda <- readr::read_csv(
    input_path_scenario_sda,
    col_types = col_types_scenario_sda,
    col_select = dplyr::all_of(col_select_scenario_sda)
  )

  abcd <- readr::read_csv(
    input_path_abcd,
    col_select = dplyr::all_of(cols_abcd),
    col_types = col_types_abcd_final
  )

  # read matched and prioritized loan book----
  list_matched_prio <- list.files(input_path_matched)[grepl("matched_prio_", list.files(input_path_matched))]

  matched_prioritized <- NULL

  # combine all matched loan books into one object to loop over
  for (i in list_matched_prio) {
    matched_prioritized_i <- readr::read_csv(
      file.path(input_path_matched, i),
      col_types = col_types_matched_prioritized,
      col_select = dplyr::all_of(col_select_matched_prioritized)
    )

    matched_prioritized <- matched_prioritized %>%
      dplyr::bind_rows(matched_prioritized_i)
  }

  # meta loan book----
  # aggregate all individual loan books into one meta loan book and add that to
  # the full list of loan books
  matched_prioritized_meta <- matched_prioritized %>%
    dplyr::mutate(
      id_loan = paste0(.data$id_loan, "_", .data$group_id),
      group_id = "meta_loanbook"
    )

  matched_prioritized <- matched_prioritized %>%
    dplyr::bind_rows(matched_prioritized_meta)

  # generate all P4B outputs----
  unique_loanbooks_matched <- unique(matched_prioritized$group_id)

  ## generate SDA outputs----
  results_sda_total <- NULL

  # generate SDA results for each individual loan book, including the meta loan book
  for (i in unique_loanbooks_matched) {
    matched_i <- matched_prioritized %>%
      dplyr::filter(.data$group_id == i) %>%
      dplyr::select(-"group_id")

    results_sda_i <- matched_i %>%
      r2dii.analysis::target_sda(
        abcd = abcd,
        co2_intensity_scenario = scenario_input_sda,
        region_isos = region_isos_select
      ) %>%
      dplyr::mutate(group_id = .env$i)

    results_sda_total <- results_sda_total %>%
      dplyr::bind_rows(results_sda_i)
  }

  # write SDA results to csv
  results_sda_total %>%
    readr::write_csv(
      file.path(output_path_standard, "sda_results_all_groups.csv"),
      na = ""
    )


  ## generate TMS outputs----

  results_tms_total <- NULL

  # generate TMS results for each individual loan book, including the meta loan book
  for (i in unique_loanbooks_matched) {
    matched_i <- matched_prioritized %>%
      dplyr::filter(.data$group_id == i) %>%
      dplyr::select(-"group_id")

    results_tms_i <- matched_i %>%
      r2dii.analysis::target_market_share(
        abcd = abcd,
        scenario = scenario_input_tms,
        region_isos = region_isos_select
      ) %>%
      dplyr::mutate(group_id = .env$i)

    results_tms_total <- results_tms_total %>%
      dplyr::bind_rows(results_tms_i)
  }

  # write TMS results to csv
  results_tms_total %>%
    readr::write_csv(
      file.path(output_path_standard, "tms_results_all_groups.csv"),
      na = ""
    )

  # generate P4B plots----

  ## retrieve set of unique groups to loop over----
  unique_groups_tms <- unique(results_tms_total$group_id)
  unique_groups_sda <- unique(results_sda_total$group_id)

  ## run automatic result generation ----------

  ### automotive----
  sector_select <- "automotive"
  for (tms_i in unique_groups_tms) {
    available_rows <- results_tms_total %>%
      dplyr::filter(
        .data[["group_id"]] == .env[["tms_i"]],
        .data[["scenario_source"]] == .env[["scenario_source_input"]],
        grepl(.env[["scenario_select"]], .data$metric),
        .data[["region"]] == .env[["region_select"]],
        .data[["sector"]] == .env[["sector_select"]]
      ) %>%
      nrow()
    if (available_rows > 0) {
      generate_individual_outputs(
        data = results_tms_total,
        matched_prioritized = matched_prioritized,
        output_directory = output_path_standard,
        target_type = "tms",
        group_id = tms_i,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_select,
        sector = sector_select,
        start_year = start_year,
        time_horizon = time_frame_select
      )
    } else {
      next()
    }
  }
  ### coal----
  sector_select <- "coal"
  for (tms_i in unique_groups_tms) {
    available_rows <- results_tms_total %>%
      dplyr::filter(
        .data[["group_id"]] == .env[["tms_i"]],
        .data[["scenario_source"]] == .env[["scenario_source_input"]],
        grepl(.env[["scenario_select"]], .data$metric),
        .data[["region"]] == .env[["region_select"]],
        .data[["sector"]] == .env[["sector_select"]]
      ) %>%
      nrow()
    if (available_rows > 0) {
      generate_individual_outputs(
        data = results_tms_total,
        matched_prioritized = matched_prioritized,
        output_directory = output_path_standard,
        target_type = "tms",
        group_id = tms_i,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_select,
        sector = sector_select,
        start_year = start_year,
        time_horizon = time_frame_select
      )
    } else {
      next()
    }
  }
  ### oil and gas----
  sector_select <- "oil and gas"
  for (tms_i in unique_groups_tms) {
    available_rows <- results_tms_total %>%
      dplyr::filter(
        .data[["group_id"]] == .env[["tms_i"]],
        .data[["scenario_source"]] == .env[["scenario_source_input"]],
        grepl(.env[["scenario_select"]], .data$metric),
        .data[["region"]] == .env[["region_select"]],
        .data[["sector"]] == .env[["sector_select"]]
      ) %>%
      nrow()
    if (available_rows > 0) {
      generate_individual_outputs(
        data = results_tms_total,
        matched_prioritized = matched_prioritized,
        output_directory = output_path_standard,
        target_type = "tms",
        group_id = tms_i,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_select,
        sector = sector_select,
        start_year = start_year,
        time_horizon = time_frame_select
      )
    } else {
      next()
    }
  }
  ### power----
  sector_select <- "power"
  for (tms_i in unique_groups_tms) {
    available_rows <- results_tms_total %>%
      dplyr::filter(
        .data[["group_id"]] == .env[["tms_i"]],
        .data[["scenario_source"]] == .env[["scenario_source_input"]],
        grepl(.env[["scenario_select"]], .data$metric),
        .data[["region"]] == .env[["region_select"]],
        .data[["sector"]] == .env[["sector_select"]]
      ) %>%
      nrow()
    if (available_rows > 0) {
      generate_individual_outputs(
        data = results_tms_total,
        matched_prioritized = matched_prioritized,
        output_directory = output_path_standard,
        target_type = "tms",
        group_id = tms_i,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_select,
        sector = sector_select,
        start_year = start_year,
        time_horizon = time_frame_select
      )
    } else {
      next()
    }
  }

  ### aviation----
  sector_select <- "aviation"
  for (sda_i in unique_groups_sda) {
    available_rows <- results_sda_total %>%
      dplyr::filter(
        .data[["group_id"]] == .env[["tms_i"]],
        .data[["scenario_source"]] == .env[["scenario_source_input"]],
        grepl(.env[["scenario_select"]], .data$emission_factor_metric),
        .data[["region"]] == .env[["region_select"]],
        .data[["sector"]] == .env[["sector_select"]]
      ) %>%
      nrow()
    if (available_rows > 0) {
      generate_individual_outputs(
        data = results_sda_total,
        matched_prioritized = matched_prioritized,
        output_directory = output_path_standard,
        target_type = "sda",
        group_id = sda_i,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_select,
        sector = sector_select,
        start_year = start_year,
        time_horizon = time_frame_select
      )
    } else {
      next()
    }
  }
  ### cement----
  sector_select <- "cement"
  for (sda_i in unique_groups_sda) {
    available_rows <- results_sda_total %>%
      dplyr::filter(
        .data[["group_id"]] == .env[["tms_i"]],
        .data[["scenario_source"]] == .env[["scenario_source_input"]],
        grepl(.env[["scenario_select"]], .data$emission_factor_metric),
        .data[["region"]] == .env[["region_select"]],
        .data[["sector"]] == .env[["sector_select"]]
      ) %>%
      nrow()
    if (available_rows > 0) {
      generate_individual_outputs(
        data = results_sda_total,
        matched_prioritized = matched_prioritized,
        output_directory = output_path_standard,
        target_type = "sda",
        group_id = sda_i,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_select,
        sector = sector_select,
        start_year = start_year,
        time_horizon = time_frame_select
      )
    } else {
      next()
    }
  }
  ### steel----
  sector_select <- "steel"
  for (sda_i in unique_groups_sda) {
    available_rows <- results_sda_total %>%
      dplyr::filter(
        .data[["group_id"]] == .env[["tms_i"]],
        .data[["scenario_source"]] == .env[["scenario_source_input"]],
        grepl(.env[["scenario_select"]], .data$emission_factor_metric),
        .data[["region"]] == .env[["region_select"]],
        .data[["sector"]] == .env[["sector_select"]]
      ) %>%
      nrow()
    if (available_rows > 0) {
      generate_individual_outputs(
        data = results_sda_total,
        matched_prioritized = matched_prioritized,
        output_directory = output_path_standard,
        target_type = "sda",
        group_id = sda_i,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_select,
        sector = sector_select,
        start_year = start_year,
        time_horizon = time_frame_select
      )
    } else {
      next()
    }
  }
}
