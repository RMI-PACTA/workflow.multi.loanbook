# set up project and load packages----
library(dplyr, warn.conflicts = FALSE)
library(r2dii.data)
library(r2dii.match)
library(readr)
library(readxl)
library(withr)

# source helpers----
source("expected_columns.R")
source("helper_functions.R")

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
stop_if_not_length(dir_raw, 1L)
stop_if_not_inherits(dir_raw, "character")
stop_if_dir_not_found(dir_raw, desc = "Raw loanbooks")

stop_if_not_length(path_abcd, 1L)
stop_if_not_inherits(path_abcd, "character")
stop_if_file_not_found(path_abcd, desc = "ABCD")

stop_if_not_length(dir_matched, 1L)
stop_if_not_inherits(dir_matched, "character")
stop_if_dir_not_found(dir_matched, desc = "Matched loanbooks")

stop_if_not_length(matching_by_sector, 1L)
stop_if_not_inherits(matching_by_sector, "logical")

stop_if_not_length(matching_min_score, 1L)
stop_if_not_inherits(matching_min_score, "numeric")

stop_if_not_length(matching_method, 1L)
stop_if_not_inherits(matching_method, "character")

stop_if_not_length(matching_p, 1L)
stop_if_not_inherits(matching_p, "numeric")

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

stop_if_not_length(matching_use_own_sector_classification, 1L)
stop_if_not_inherits(matching_use_own_sector_classification, "logical")

# path to own sector classification only required if boolean TRUE
if (matching_use_own_sector_classification) {
  stop_if_not_length(path_own_sector_classification, 1L)
  stop_if_not_inherits(path_own_sector_classification, "character")
  stop_if_file_not_found(path_own_sector_classification, desc = "Own sector classification")
}

# load data----

## load abcd----

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

stop_if_not_expected_columns(abcd, cols_abcd, "ABCD")

## optionally load own classification system----
if (matching_use_own_sector_classification) {
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

raw_lbk <- readr::read_csv(
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
