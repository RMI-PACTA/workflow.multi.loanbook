run_match_prioritize <- function(config) {
  config <- load_config(config)

  dir_matched <- get_matched_dir(config)
  abcd_dir <- get_abcd_dir(config)

  match_prio_priority <- get_match_priority(config)

  apply_sector_split <- get_apply_sector_split(config)
  sector_split_type_select <- get_sector_split_type(config)

  # validate config values----
  stop_if_not_length(abcd_dir, 1L)
  stop_if_not_inherits(abcd_dir, "character")
  stop_if_dir_not_found(abcd_dir, desc = "ABCD data")
  stop_if_file_not_found(file.path(abcd_dir, "abcd_final.csv"), desc = "ABCD final")
  stop_if_not_length(dir_matched, 1L)
  stop_if_not_inherits(dir_matched, "character")
  stop_if_dir_not_found(dir_matched, desc = "Matched loanbook")

  if (!is.null(match_prio_priority)) {
    if (
      !inherits(match_prio_priority, "character") &
      !inherits(match_prio_priority, "formula") &
      !inherits(match_prio_priority, "function")
    ) {
      valid_types <- c(
        "a character vector",
        "a function",
        "a quosure-style lambda function"
      )
      cli::cli_abort(c(
        "x" = paste0(
          "Argument {.arg match_prio_priority} must be of one of: {.or {valid_types}}, ",
          "not {.cls {class(match_prio_priority)}}."
        ),
        "i" = "Check the {.val match_prioritize:priority} parameter set in your {.file config.yml}."
      ))
    }
  }

  # load data----
  ## load manually matched files----
  list_matched_manual <- list.files(path = dir_matched, pattern = "^matched_lbk_.*_manual[.]csv$")

  if (length(list_matched_manual) == 0) {
    cli::cli_abort(c(
      "x" = "No manually matched loan book csvs were found.",
      "i" = "No files matching the pattern {.code ^matched_lbk_.*_manual[.]csv$} were found in {.path {dir_matched}}. Have you done the manual matching process and named the edited CSVs properly?",
      "i" = "If {.path {dir_matched}} is not the correct directory, check the {.val dir_matched} parameter set in your {.file config.yml}."
    ))
  }

  matched_lbk_manual <- readr::read_csv(
    file = file.path(dir_matched, list_matched_manual),
    col_types = col_types_matched_manual
  ) %>%
    dplyr::group_split(.data[["group_id"]])

  ## optional: load sector split----
  if (apply_sector_split & sector_split_type_select == "equal_weights") {
    companies_sector_split <- readr::read_csv(
      file.path(dir_matched, "companies_sector_split.csv"),
      col_types = col_types_companies_sector_split,
      col_select = dplyr::all_of(col_select_companies_sector_split)
    )

    abcd <- readr::read_csv(
      file.path(abcd_dir, "abcd_final.csv"),
      col_select = dplyr::all_of(cols_abcd),
      col_types = col_types_abcd_final
    )
  }

  # prioritize and save files----
  for (i in seq_along(matched_lbk_manual)) {
    group_name <- unique(matched_lbk_manual[[i]][["group_id"]])

    ## prioritize matched loan book----
    matched_prio_i <- matched_lbk_manual[[i]] %>%
      r2dii.match::prioritize(priority = match_prio_priority) %>%
      dplyr::mutate(group_id = .env[["group_name"]])

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
        id_loan = paste(.data[["id_loan"]], .data[["group_id"]], sep = "_")
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
    lost_companies_sector_split(
      abcd = abcd,
      companies_sector_split = companies_sector_split
    ) %>%
      readr::write_csv(
        file = file.path(dir_matched, glue::glue("lost_companies_sector_split.csv.csv")),
        na = ""
      )
  }
}
