run_pacta <- function(config) {
  config <- load_config(config)

  input_path_scenario <- get_scenario_dir(config)
  input_dir_abcd <- get_abcd_dir(config)
  dir_matched <- get_matched_dir(config)

  input_path_scenario_tms <- get_scenario_tms_path(config)
  input_path_scenario_sda <- get_scenario_sda_path(config)

  input_path_abcd <- file.path(input_dir_abcd, "abcd_final.csv")

  output_path <- get_output_dir(config)
  output_path_standard <- file.path(output_path, "standard")

  scenario_source_input <- get_scenario_source(config)
  scenario_select <- get_scenario_select(config)
  region_select <- get_region_select(config)
  start_year <- get_start_year(config)
  time_frame_select <- get_time_frame(config)
  apply_sector_split <- get_apply_sector_split(config)
  if (is.null(apply_sector_split)) { apply_sector_split <- FALSE }
  sector_split_type_select <- get_sector_split_type(config)
  remove_inactive_companies <- get_remove_inactive_companies(config)
  if (is.null(remove_inactive_companies)) { remove_inactive_companies <- FALSE }

  # if a sector split is applied, write results into a directory that states the type
  if (apply_sector_split) {
    output_path_standard <- file.path(output_path, sector_split_type_select, "standard")
  }

  dir.create(output_path_standard, recursive = TRUE, showWarnings = FALSE)

  by_group <- get_aggregate_alignment_metric_by_group(config)
  by_group <- check_and_prepare_by_group(by_group)
  by_group_ext <- if (is.null(by_group)) { "_meta" } else {paste0("_", by_group, collapse = "_")}

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
    matched_prioritized <- matched_prioritized %>%
      dplyr::mutate(meta = "meta")
  }

  # remove non standard columns from matched_prioritzed when calling r2dii.analysis
  matched_prio_non_standard_cols <- names(matched_prioritized)[!names(matched_prioritized) %in% col_standard_matched_prioritized]

  # generate all P4B outputs----
  unique_loanbook_groups_matched <- unique(matched_prioritized[[by_group]])

  ## generate SDA outputs----
  results_sda_total <- NULL

  # generate SDA results for each individual loan book, including the meta loan book
  for (i in unique_loanbook_groups_matched) {
    matched_i <- matched_prioritized %>%
      dplyr::filter(.data[[by_group]] == i) %>%
      dplyr::select(-dplyr::all_of(matched_prio_non_standard_cols))

    results_sda_i <- matched_i %>%
      r2dii.analysis::target_sda(
        abcd = abcd,
        co2_intensity_scenario = scenario_input_sda,
        region_isos = region_isos_select
      ) %>%
      dplyr::mutate(!!rlang::sym(by_group) := .env$i)

    results_sda_total <- results_sda_total %>%
      dplyr::bind_rows(results_sda_i)
  }

  # write SDA results to csv
  results_sda_total %>%
    readr::write_csv(
      file.path(output_path_standard, paste0("sda_results", by_group_ext, ".csv")),
      na = ""
    )


  ## generate TMS outputs----

  results_tms_total <- NULL

  # generate TMS results for each individual loan book, including the meta loan book
  for (i in unique_loanbook_groups_matched) {
    matched_i <- matched_prioritized %>%
      dplyr::filter(.data[[by_group]] == i) %>%
      dplyr::select(-dplyr::all_of(matched_prio_non_standard_cols))

    results_tms_i <- matched_i %>%
      r2dii.analysis::target_market_share(
        abcd = abcd,
        scenario = scenario_input_tms,
        region_isos = region_isos_select
      ) %>%
      dplyr::mutate(!!rlang::sym(by_group) := .env$i)

    results_tms_total <- results_tms_total %>%
      dplyr::bind_rows(results_tms_i)
  }

  # write TMS results to csv
  results_tms_total %>%
    readr::write_csv(
      file.path(output_path_standard, paste0("tms_results", by_group_ext, ".csv")),
      na = ""
    )

  # generate P4B plots----

  ## retrieve set of unique groups to loop over----
  unique_groups_tms <- unique(results_tms_total[[by_group]])
  unique_groups_sda <- unique(results_sda_total[[by_group]])


  ## run automatic result generation ----------

  ### automotive----
  sector_select <- "automotive"
  for (tms_i in unique_groups_tms) {
    available_rows <- results_tms_total %>%
      dplyr::filter(
        .data[[by_group]] == .env[["tms_i"]],
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
        by_group = by_group,
        by_group_value = tms_i,
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
        .data[[by_group]] == .env[["tms_i"]],
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
        by_group = by_group,
        by_group_value = tms_i,
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
        .data[[by_group]] == .env[["tms_i"]],
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
        by_group = by_group,
        by_group_value = tms_i,
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
        .data[[by_group]] == .env[["tms_i"]],
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
        by_group = by_group,
        by_group_value = tms_i,
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
        .data[[by_group]] == .env[["sda_i"]],
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
        by_group = by_group,
        by_group_value = sda_i,
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
        .data[[by_group]] == .env[["sda_i"]],
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
        by_group = by_group,
        by_group_value = sda_i,
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
        .data[[by_group]] == .env[["sda_i"]],
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
        by_group = by_group,
        by_group_value = sda_i,
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
