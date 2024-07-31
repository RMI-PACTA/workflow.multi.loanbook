# set up project and load packages----
library(dplyr, warn.conflicts = FALSE)
library(r2dii.data)
library(r2dii.match)
library(readr)
library(readxl)
library(withr)

# source helpers----
source("expected_columns.R")

# load config----
# TODO: all params to function signature
config_dir <- config::get("directories")
config_files <- config::get("file_names")

dir_raw <- config_dir$dir_raw
path_abcd <- file.path(config_dir$dir_abcd, config_files$filename_abcd)
sheet_abcd <- config_files$sheet_abcd
dir_matched <- config_dir$dir_matched

config_matching <- config::get("matching")

matching_by_sector <- config_matching$params_match_name$by_sector
matching_min_score <- config_matching$params_match_name$min_score
matching_method <- config_matching$params_match_name$method
matching_p <- config_matching$params_match_name$p
matching_overwrite <- config_matching$params_match_name$overwrite
matching_join_id <- config_matching$params_match_name$join_id

matching_use_own_sector_classification <- config_matching$own_sector_classification$use_own_sector_classification
if (matching_use_own_sector_classification) {
  dir_own_sector_classification <- config_matching$own_sector_classification$dir_own_sector_classification
  filename_own_sector_classification <- config_matching$own_sector_classification$filename_own_sector_classification
  path_own_sector_classification <- file.path(dir_own_sector_classification, filename_own_sector_classification)
}

# validate config values----
if (!length(dir_raw) == 1) {
  stop("Argument dir_raw must be of length 1. Please check your input.")
}
if (!inherits(dir_raw, "character")) {
  stop("Argument dir_raw must be of class character. Please check your input.")
}
if (!length(path_abcd) == 1) {
  stop("Argument path_abcd must be of length 1. Please check your input.")
}
if (!inherits(path_abcd, "character")) {
  stop("Argument path_abcd must be of class character. Please check your input.")
}
if (!length(dir_matched) == 1) {
  stop("Argument dir_matched must be of length 1. Please check your input.")
}
if (!inherits(dir_matched, "character")) {
  stop("Argument dir_matched must be of class character. Please check your input.")
}
if (!length(matching_by_sector) == 1) {
  stop("Argument matching_by_sector must be of length 1. Please check your input.")
}
if (!inherits(matching_by_sector, "logical")) {
  stop("Argument matching_by_sector must be of class logical. Please check your input.")
}
if (!length(matching_min_score) == 1) {
  stop("Argument matching_min_score must be of length 1. Please check your input.")
}
if (!inherits(matching_min_score, "numeric")) {
  stop("Argument matching_min_score must be of class numeric. Please check your input.")
}
if (!length(matching_method) == 1) {
  stop("Argument matching_method must be of length 1. Please check your input.")
}
if (!inherits(matching_method, "character")) {
  stop("Argument matching_method must be of class character Please check your input.")
}
if (!length(matching_p) == 1) {
  stop("Argument matching_p must be of length 1. Please check your input.")
}
if (!inherits(matching_p, "numeric")) {
  stop("Argument matching_p must be of class numeric. Please check your input.")
}
# TODO: check for data.frame
# if (!length(matching_overwrite) == 1) {
#   stop("Argument matching_overwrite must be of length 1. Please check your input.")
# }
# if (!inherits(matching_overwrite, "numeric")) {
#   stop("Argument matching_overwrite must be of class numeric. Please check your input.")
# }
# TODO: check for join_object
# if (!length(matching_join_id) == 1) {
#   stop("Argument matching_join_id must be of length 1. Please check your input.")
# }
# if (!inherits(matching_join_id, "numeric")) {
#   stop("Argument matching_join_id must be of class numeric. Please check your input.")
# }
if (!length(matching_use_own_sector_classification) == 1) {
  stop("Argument matching_use_own_sector_classification must be of length 1. Please check your input.")
}
if (!inherits(matching_use_own_sector_classification, "logical")) {
  stop("Argument matching_use_own_sector_classification must be of class logical. Please check your input.")
}
# path to own sector classification only required if boolean TRUE
if (matching_use_own_sector_classification) {
  if (!length(path_own_sector_classification) == 1) {
    stop("When matching_use_own_sector_classification == TRUE, argument path_own_sector_classification must be of length 1. Please check your input.")
  }
  if (!inherits(path_own_sector_classification, "character")) {
    stop("When matching_use_own_sector_classification == TRUE, argument path_own_sector_classification must be of class character. Please check your input.")
  }
}

# load data----

## load abcd----
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

## optionally load own classification system----
if (matching_use_own_sector_classification) {
  if (!file.exists(path_own_sector_classification)) {
    stop(glue::glue("No sector classification file found at path {path_own_sector_classification}. Please check your project setup!"))
  }

  sector_classification_system <- readr::read_csv(
    file = path_own_sector_classification,
    col_types = col_types_sector_classification,
    col_select = dplyr::all_of(col_select_sector_classification)
  )
}

## load raw loan books----
list_raw <- list.files(path = dir_raw, pattern = "[.]csv$")

if (length(list_raw) == 0) {
  stop(glue::glue("No raw loan book csvs found in {dir_raw}. Please check your project setup!"))
}

raw_lbk <- vroom::vroom(
  file = file.path(dir_raw, list_raw),
  col_types = col_types_raw,
  id = "group_id"
) %>%
  dplyr::mutate(group_id = tools::file_path_sans_ext(basename(.data$group_id))) %>%
  dplyr::group_split(.data$group_id)

# match and save loan books----
for (i in 1:length(raw_lbk)) {
  group_name <- unique(raw_lbk[[i]]$group_id)

  ## match data----
  if (matching_use_own_sector_classification) {
    withr::with_options(
      new = list(r2dii.match.sector_classifications = sector_classification_system),
      code = {
        getOption("r2dii.match.sector_classifications")
        matched_lbk_i <- r2dii.match::match_name(
          loanbook = raw_lbk[[i]],
          abcd = abcd,
          by_sector = matching_by_sector,
          min_score = matching_min_score,
          method = matching_method,
          p = matching_p,
          overwrite = matching_overwrite,
          join_id = matching_join_id
          # TODO: allow surfacing the other match_name args
        )
      }
    )
  } else {
    matched_lbk_i <- r2dii.match::match_name(
      loanbook = raw_lbk[[i]],
      abcd = abcd,
      by_sector = matching_by_sector,
      min_score = matching_min_score,
      method = matching_method,
      p = matching_p,
      overwrite = matching_overwrite,
      join_id = matching_join_id
    )
  }

  ## write matched data to file----
  matched_lbk_i %>%
    readr::write_csv(
      file = file.path(dir_matched, glue::glue("matched_lbk_{group_name}.csv")),
      na = ""
    )
}
