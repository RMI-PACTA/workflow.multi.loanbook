# load packages----
library(dplyr, warn.conflicts = FALSE)
library(r2dii.data)
library(readr)
library(rlang)
library(tidyr)
library(vroom)

# source helpers----
source("expected_columns.R")

# load config----
config_dir <- config::get("directories")
dir_matched <- config_dir$dir_matched
dir_output <- dir_matched

config_project_parameters <- config::get("project_parameters")
scenario_source_input <- config_project_parameters$scenario_source
start_year <- config_project_parameters$start_year

config_sector_split <- config::get("sector_split")
apply_sector_split <- config_sector_split$apply_sector_split

if (apply_sector_split) {
  sector_split_type <- config_sector_split$sector_split_type
  dir_output <- file.path(dir_output, sector_split_type)
}

dir.create(dir_output, recursive = TRUE)

# description production data ----
# read abcd
abcd <- readr::read_csv(
  file.path(config_dir$dir_abcd, "abcd_final.csv"),
  # col_types = col_types_abcd,
  col_select = dplyr::all_of(cols_abcd)
)
# # replace potential NA values with 0 in production
# abcd["production"][is.na(abcd["production"])] <- 0

# filter to start year as we calculate coverage in start year
abcd <- abcd %>%
  dplyr::filter(.data$year == .env$start_year)

# coverage of production by companies in loan books compared to total production----

## load matched prioritized loan books----
list_matched_prioritized <- list.files(dir_matched)[grepl("^matched_prio_.*csv$", list.files(dir_matched))]

if (length(list_matched_prioritized) == 0) {
  stop(glue::glue("No matched prioritized loan book csvs found in {dir_matched}. Please check your project setup!"))
}

matched_prioritized <- vroom::vroom(
  file = file.path(dir_matched, list_matched_prioritized),
  col_types = col_types_matched_prioritized,
  col_select = dplyr::all_of(col_select_matched_prioritized)
)

# optional: apply sector split----
# if (apply_sector_split & sector_split_type_select %in% c("equal_weights", "worst_case")) {
#   if (sector_split_type_select == "equal_weights") {
#     companies_sector_split <- readr::read_csv(
#       file.path(input_path_matched, "companies_sector_split.csv"),
#       col_types = col_types_companies_sector_split,
#       col_select = dplyr::all_of(col_select_companies_sector_split)
#     )
#   } else {
#     companies_sector_split <- readr::read_csv(
#       file.path(input_path_matched, "companies_sector_split_worst_case.csv"),
#       col_types = col_types_companies_sector_split_worst_case,
#       col_select = dplyr::all_of(col_select_companies_sector_split_worst_case)
#     )
#   }
#
#   matched_prioritized <- matched_prioritized %>%
#     apply_sector_split_to_loans(
#       abcd = abcd,
#       companies_sector_split = companies_sector_split,
#       sector_split_type = sector_split_type_select,
#       input_path_matched = input_path_matched
#     )
# }

# create summary of loan book coverage----

matched_companies <- matched_prioritized %>%
  dplyr::distinct(
    .data$name_abcd,
    .data$sector_abcd,
    .data$loan_size_outstanding,
    .data$loan_size_outstanding_currency,
    .data$score
  )

# get required countries for region_select----
region_isos_complete <- r2dii.data::region_isos

region_isos_select <- region_isos_complete %>%
  dplyr::filter(
    .data$source == .env$scenario_source_input
  )

# available_regions
available_regions <- unique(region_isos_select$region)

# calculate summary stats for each available region----
production_coverage_summary <- NULL

for (region_i in available_regions) {
  countries_select_i <- region_isos_select %>%
    dplyr::filter(.data$region == .env$region_i) %>%
    dplyr::pull(.data$isos) %>%
    toupper()

  # summarise abcd by relevant region
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

  # add information on matched companies across analysed loan books
  production_coverage_summary_i <- production_coverage_summary_i %>%
    dplyr::left_join(
      matched_companies,
      by = c(
        "name_company" = "name_abcd",
        "sector" = "sector_abcd"
      )
    )

  # calculate summary statistics
  production_coverage_summary_i <- production_coverage_summary_i %>%
    dplyr::mutate(
      financed_production = dplyr::if_else(.data$score == 1, .data$production, 0),
      matched_company = dplyr::if_else(.data$score == 1, .data$name_company, NA_character_)
    ) %>%
    dplyr::mutate(
      matched_rows_company_sector = sum(.data$score, na.rm = TRUE),
      .by =c("name_company", "sector")
    ) %>%
    dplyr::summarise(
      total_exposure = sum(.data$loan_size_outstanding / .data$matched_rows_company_sector, na.rm = TRUE),
      n_companies_matched = dplyr::n_distinct(.data$matched_company, na.rm = TRUE),
      n_companies_total = dplyr::n_distinct(.data$name_company, na.rm = TRUE),
      production_financed = sum(.data$financed_production, na.rm = TRUE),
      production_total = sum(.data$production, na.rm = TRUE),
      .by = c("sector")
    ) %>%
    dplyr::mutate(
      share_companies_matched = .data$n_companies_matched / .data$n_companies_total,
      share_production_financed = .data$production_financed / .data$production_total,
      region = .env$region_i
    )

  production_coverage_summary <- production_coverage_summary %>%
    dplyr::bind_rows(production_coverage_summary_i)

}

# format
production_coverage_summary <- production_coverage_summary %>%
  dplyr::select(
    dplyr::all_of(
      c(
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

# save to matched directory
production_coverage_summary %>%
  readr::write_csv(
    file.path(dir_output, "summary_statistics_loanbook_coverage.csv"),
    na = ""
  )
