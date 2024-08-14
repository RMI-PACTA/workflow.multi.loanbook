# set up project and load packages----
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(readxl)
library(tidyr)

# source helpers----
source("expected_columns.R")

# load config----
config_dir <- config::get("directories")
config_files <- config::get("file_names")

path_abcd <- file.path(config_dir$dir_abcd, config_files$filename_abcd)
sheet_abcd <- config_files$sheet_abcd

config_prepare_abcd <- config::get("prepare_abcd")

prepare_abcd_rm_inactive_companies <- config_prepare_abcd$remove_inactive_companies

config_project_parameters <- config::get("project_parameters")

project_parameters_start_year <- config_project_parameters$start_year
project_parameters_time_frame <- config_project_parameters$time_frame

# validate config values----
if (!length(path_abcd) == 1) {
  stop("Argument path_abcd must be of length 1. Please check your input.")
}
if (!inherits(path_abcd, "character")) {
  stop("Argument path_abcd must be of class character. Please check your input.")
}
if (!length(sheet_abcd) == 1) {
  stop("Argument sheet_abcd must be of length 1. Please check your input.")
}
if (!inherits(sheet_abcd, "character")) {
  stop("Argument sheet_abcd must be of class character. Please check your input.")
}
if (!is.null(prepare_abcd_rm_inactive_companies)) {
  if (!length(prepare_abcd_rm_inactive_companies) == 1) {
    stop("Argument prepare_abcd_rm_inactive_companies must be of length 1. Please check your input.")
  }
  if (!inherits(prepare_abcd_rm_inactive_companies, "logical")) {
    stop("Argument prepare_abcd_rm_inactive_companies must be of class logical. Please check your input.")
  }
}
if (!length(project_parameters_start_year) == 1) {
  stop("Argument project_parameters_start_year must be of length 1. Please check your input.")
}
if (!inherits(project_parameters_start_year, "integer")) {
  stop("Argument project_parameters_start_year must be of class integer Please check your input.")
}
if (!length(project_parameters_time_frame) == 1) {
  stop("Argument project_parameters_time_frame must be of length 1. Please check your input.")
}
if (!inherits(project_parameters_time_frame, "integer")) {
  stop("Argument project_parameters_time_frame must be of class integer Please check your input.")
}


# load data----
if (!file.exists(path_abcd)) {
  stop(glue::glue("No ABCD file found at path {path_abcd}. Please check your project setup!"))
}

abcd <- readxl::read_xlsx(
  path = file.path(path_abcd),
  sheet = sheet_abcd
) %>% dplyr::select(
  dplyr::all_of(cols_abcd)
) %>%
  dplyr::mutate(
    company_id = as.numeric(.data$company_id),
    name_company = as.character(.data$name_company),
    lei = as.character(.data$lei),
    is_ultimate_owner = as.logical(.data$is_ultimate_owner),
    sector = as.character(.data$sector),
    technology = as.character(.data$technology),
    plant_location = as.character(.data$plant_location),
    year = as.integer(.data$year),
    production = as.numeric(.data$production),
    production_unit = as.character(.data$production_unit),
    emission_factor = as.numeric(.data$emission_factor),
    emission_factor_unit = as.character(.data$emission_factor_unit)
  )
if (!all(cols_abcd %in% names(abcd))) {
  stop("Columns in abcd do not match expected input names. Please check your input.")
}

# optional: remove inactive companies----

# (1) remove company-sector combinations where production in t5 = 0 when
# it was greater than 0 in t0.
# (2) remove company-sector combinations where production is 0 for the entire
# time frame from t0 to t5.
rm_inactive_companies <- function(data,
                                  start_year,
                                  time_frame) {
  comp_sec_no_prod_t5 <- data %>%
    dplyr::filter(
      .data[["year"]] %in% c(.env$start_year, .env$start_year + .env$time_frame)
    ) %>%
    dplyr::summarise(
      sum_production = sum(.data$production, na.rm = TRUE),
      .by = c("name_company", "sector", "year")
    ) %>%
    tidyr::pivot_wider(
      names_from = "year",
      names_prefix = "prod_",
      values_from = "sum_production"
    ) %>%
    dplyr::filter(
      .data[[paste0("prod_", start_year)]] > 0,
      .data[[paste0("prod_", start_year + time_frame)]] == 0
    ) %>%
    dplyr::distinct(
      .data$name_company,
      .data$sector
    )

  comp_sec_no_prod_t0_to_t5 <- data %>%
    dplyr::filter(
      .data[["year"]] %in% c(.env$start_year, .env$start_year + .env$time_frame)
    ) %>%
    dplyr::summarise(
      sum_production = sum(.data$production, na.rm = TRUE),
      .by = c("name_company", "sector")
    ) %>%
    dplyr::filter(
      .data$sum_production == 0
    ) %>%
    dplyr::distinct(
      .data$name_company,
      .data$sector
    )

  data <- data %>%
    dplyr::anti_join(
      comp_sec_no_prod_t5,
      by = c("name_company", "sector")
    ) %>%
    dplyr::anti_join(
      comp_sec_no_prod_t0_to_t5,
      by = c("name_company", "sector")
    )

  return(data)
}

if (prepare_abcd_rm_inactive_companies) {
  abcd_keep <- abcd %>%
    rm_inactive_companies(
      start_year = project_parameters_start_year,
      time_frame = project_parameters_time_frame
    )

  abcd_removed <- abcd %>%
    dplyr::anti_join(
      abcd_keep,
      by = c("company_id", "sector")
    )

  # write removed inactive companies to file for inspection
  abcd_removed %>%
    readr::write_csv(
      file.path(config_dir$dir_abcd, "abcd_removed_inactive_companies.csv"),
      na = ""
    )

  abcd <- abcd_keep

  rm(abcd_keep)
}

# write final version of abcd to file for use PACTA analysis
abcd %>%
  readr::write_csv(
    file.path(config_dir$dir_abcd, "abcd_final.csv"),
    na = ""
  )
