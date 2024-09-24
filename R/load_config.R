load_config <- function(config) {
  if (is.character(config)) return(config::get(file = config))
  config
}

get_input_dir <- function(params) {
  params[["directories"]][["dir_input"]]
}

get_loanbook_dir <- function(params) {
  file.path(params[["directories"]][["dir_input"]], "loanbooks")
}

# get_abcd_dir <- function(params) {
#   params[["directories"]][["dir_abcd"]]
# }

get_abcd_filename <- function(params) {
  params[["file_names"]][["filename_abcd"]]
}

get_abcd_sheet <- function(params) {
  params[["file_names"]][["sheet_abcd"]]
}

# get_matched_dir <- function(params) {
#   params[["directories"]][["dir_matched"]]
# }
#
# get_raw_dir <- function(params) {
#   params[["directories"]][["dir_raw"]]
# }
#
# get_scenario_dir <- function(params) {
#   params[["directories"]][["dir_scenario"]]
# }

get_output_dir <- function(params) {
  params[["directories"]][["dir_output"]]
}

get_output_prepare_dir <- function(params) {
  file.path(params[["directories"]][["dir_output"]], "prepare_abcd")
}

get_output_matched_loanbooks_dir <- function(params) {
  file.path(params[["directories"]][["dir_output"]], "matched_loanbooks")
}

get_output_prio_diagnostics_dir <- function(params) {
  file.path(params[["directories"]][["dir_output"]], "prioritized_loanbooks_and_diagnostics")
}

get_output_analysis_dir <- function(params) {
  file.path(params[["directories"]][["dir_output"]], "analysis")
}

get_scenario_tms_filename <- function(params) {
  params[["file_names"]][["filename_scenario_tms"]]
}

get_scenario_sda_filename <- function(params) {
  params[["file_names"]][["filename_scenario_sda"]]
}

get_start_year <- function(params) {
  params[["project_parameters"]][["start_year"]]
}

get_time_frame <- function(params) {
  params[["project_parameters"]][["time_frame"]]
}

get_remove_inactive_companies <- function(params) {
  params[["prepare_abcd"]][["remove_inactive_companies"]]
}

get_scenario_source <- function(params) {
  params[["project_parameters"]][["scenario_source"]]
}

get_scenario_select <- function(params) {
  params[["project_parameters"]][["scenario_select"]]
}

get_region_select <- function(params) {
  params[["project_parameters"]][["region_select"]]
}

get_by_group <- function(params) {
  params[["project_parameters"]][["by_group"]]
}

get_match_priority <- function(params) {
  params[["match_prioritize"]][["priority"]]
}

get_match_by_sector <- function(params) {
  params[["matching"]][["params_match_name"]][["by_sector"]]
}

get_match_min_score <- function(params) {
  params[["matching"]][["params_match_name"]][["min_score"]]
}

get_match_method <- function(params) {
  params[["matching"]][["params_match_name"]][["method"]]
}

get_match_p <- function(params) {
  params[["matching"]][["params_match_name"]][["p"]]
}

get_match_overwrite <- function(params) {
  params[["matching"]][["params_match_name"]][["overwrite"]]
}

get_match_join_id <- function(params) {
  params[["matching"]][["params_match_name"]][["join_id"]]
}

get_match_plot_width <- function(params) {
  params[["match_success_rate"]][["plot_width"]]
}

get_match_plot_height <- function(params) {
  params[["match_success_rate"]][["plot_height"]]
}

get_match_plot_units <- function(params) {
  params[["match_success_rate"]][["plot_units"]]
}

get_match_plot_resolution <- function(params) {
  params[["match_success_rate"]][["plot_resolution"]]
}

get_apply_sector_split <- function(params) {
  params[["sector_split"]][["apply_sector_split"]]
}

get_sector_split_type <- function(params) {
  params[["sector_split"]][["sector_split_type"]]
}

get_use_manual_sector_classification <- function(params) {
  params[["matching"]][["manual_sector_classification"]][["use_manual_sector_classification"]]
}

# get_manual_sector_classification_dir <- function(params) {
#   params[["matching"]][["manual_sector_classification"]][["dir_manual_sector_classification"]]
# }

get_manual_sector_classification_filename <- function(params) {
  params[["matching"]][["manual_sector_classification"]][["filename_manual_sector_classification"]]
}

get_manual_sector_classification_path <- function(params) {
  file.path(
    get_input_dir(config),
    get_manual_sector_classification_filename(params)
  )
}

get_abcd_path <- function(config) {
  file.path(
    get_input_dir(config),
    get_abcd_filename(config)
  )
}

get_sector_split_path <- function(config) {
  file.path(
    get_input_dir(config),
    config[["sector_split"]][["filename_split_company_id"]]
  )
}

get_advanced_company_indicators_path <- function(config) {
  file.path(
    get_input_dir(config),
    config[["sector_split"]][["filename_advanced_company_indicators"]]
  )
}

get_advanced_company_indicators_sheet <- function(config) {
  config[["sector_split"]][["sheet_advanced_company_indicators"]]
}

get_scenario_tms_path <- function(config) {
  file.path(
    get_input_dir(config),
    get_scenario_tms_filename(config)
  )
}

get_scenario_sda_path <- function(config) {
  file.path(
    get_input_dir(config),
    get_scenario_sda_filename(config)
  )
}
