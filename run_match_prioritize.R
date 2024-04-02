# set up project and load packages----
library(dplyr, warn.conflicts = FALSE)
library(r2dii.match)
library(readr)
library(vroom)

# source helpers----
source("expected_columns.R")

# load config----
config_dir <- config::get("directories")
dir_matched <- config_dir$dir_matched

config_match_prio <- config::get("match_prioritize")
match_prio_priority <- config_matching$match_prioritize$priority

# validate config values----
if (!length(dir_matched) == 1) {
  stop("Argument dir_matched must be of length 1. Please check your input.")
}
if (!inherits(dir_matched, "character")) {
  stop("Argument dir_matched must be of class character. Please check your input.")
}
# TODO: check for appropriate expectations
# if (!is.null(match_prio_priority)) {
#   if (!length(match_prio_priority) == 1) {
#     stop("Argument match_prio_priority must be of length 1. Please check your input.")
#   }
#   if (!inherits(match_prio_priority, "character")) {
#     stop("Argument match_prio_priority must be of class character. Please check your input.")
#   }
# }

# laod manually matched files----
list_matched_manual <- list.files(dir_matched)[grepl("^matched_lbk_.*_manual.csv$", list.files(dir_matched))]

if (length(list_matched_manual) == 0) {
  stop(glue::glue("No manually matched loan book csvs found in {dir_matched}. Please check your project setup!"))
}

matched_lbk_manual <- vroom::vroom(
  file = file.path(dir_matched, list_matched_manual),
  col_types = col_types_raw,
  col_select = dplyr::all_of(col_select_matched_manual)
) %>%
  dplyr::group_split(.data$group_id)

# prioritize and save files----
for (i in 1:length(matched_lbk_manual)) {
  group_name <- unique(matched_lbk_manual[[i]]$group_id)

  ## prioritize matched loan book----
  matched_prio_i <- matched_lbk_manual[[i]] %>%
    r2dii.match::prioritize(priority = match_prio_priority) %>%
    dplyr::mutate(group_id = .env$group_name)

  ## write matched prioritized loan book to file----
  matched_prio_i %>%
    readr::write_csv(
      file = file.path(dir_matched, glue::glue("matched_prio_{group_name}.csv")),
      na = ""
    )
}
