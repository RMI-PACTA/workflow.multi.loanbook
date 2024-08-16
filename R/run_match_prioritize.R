run_match_prioritize <- function(config) {
  config <- load_config(config)

  dir_matched <- get_matched_dir(config)
  path_abcd <- get_abcd_path(config)
  sheet_abcd <- get_abcd_sheet(config)

  match_prio_priority <- get_match_priority(config)

  apply_sector_split <- get_apply_sector_split(config)
  sector_split_type_select <- get_sector_split_type(config)

  # validate config values----
  stop_if_not_length(dir_matched, 1L)
  stop_if_not_inherits(dir_matched, "character")
  stop_if_dir_not_found(dir_matched, desc = "Matched loanbook")

  if (!is.null(match_prio_priority)) {
    if (
      !inherits(match_prio_priority, "character") &
      !inherits(match_prio_priority, "formula") &
      !inherits(match_prio_priority, "function")
    ) {
      stop(
        glue::glue(
          "Argument match_prio_priority must be of one of: a character vector, a
          function, or a quosure-style lambda function. Your input is of class
          {class(match_prio_priority)}. Please check your input."
        )
      )
    }
  }

  # load data----
  ## load manually matched files----
  list_matched_manual <- list.files(path = dir_matched, pattern = "^matched_lbk_.*_manual[.]csv$")

  if (length(list_matched_manual) == 0) {
    stop(glue::glue("No manually matched loan book csvs found in {dir_matched}. Please check your project setup!"))
  }

  matched_lbk_manual <- readr::read_csv(
    file = file.path(dir_matched, list_matched_manual),
    col_types = col_types_matched_manual
  ) %>%
    dplyr::group_split(.data$group_id)

  ## optional: load sector split----
  if (apply_sector_split & sector_split_type_select == "equal_weights") {
    companies_sector_split <- readr::read_csv(
      file.path(dir_matched, "companies_sector_split.csv"),
      col_types = col_types_companies_sector_split,
      col_select = dplyr::all_of(col_select_companies_sector_split)
    )

    # TODO: better use prepared abcd?
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

    stop_if_not_expected_columns(abcd, cols_abcd, desc = "ABCD")
  }

  # prioritize and save files----
  for (i in seq_along(matched_lbk_manual)) {
    group_name <- unique(matched_lbk_manual[[i]]$group_id)

    ## prioritize matched loan book----
    matched_prio_i <- matched_lbk_manual[[i]] %>%
      r2dii.match::prioritize(priority = match_prio_priority) %>%
      dplyr::mutate(group_id = .env$group_name)

    # optional: apply sector split----
    if (apply_sector_split & sector_split_type_select == "equal_weights") {
      matched_prio_i <- matched_prio_i %>%
        apply_sector_split_to_loans(
          abcd = abcd,
          companies_sector_split = companies_sector_split
        )
    }

    ## ensure that id_loan is unique across all loan books----
    matched_prio_i <- matched_prio_i %>%
      dplyr::mutate(
        id_loan = paste(.data$id_loan, i, sep = "_")
      )

    ## write matched prioritized loan book to file----
    matched_prio_i %>%
      readr::write_csv(
        file = file.path(dir_matched, glue::glue("matched_prio_{group_name}.csv")),
        na = ""
      )
  }

  # optional: apply sector split----
  if (apply_sector_split & sector_split_type_select == "equal_weights") {
    lost_companies_sector_split <- lost_companies_sector_split(
      abcd = abcd,
      companies_sector_split = companies_sector_split
    )

    lost_companies_sector_split %>%
      readr::write_csv(
        file = file.path(dir_matched, glue::glue("lost_companies_sector_split.csv.csv")),
        na = ""
      )
  }
}
