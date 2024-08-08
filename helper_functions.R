lost_companies_sector_split <- function(abcd,
                                        companies_sector_split) {
  abcd_id <- abcd %>%
    dplyr::distinct(.data$company_id, .data$name_company)

  # identify lost_companies_sector_split and write to csv for inspection
  lost_companies_sector_split <- companies_sector_split %>%
    dplyr::anti_join(
      abcd_id,
      by = c("company_id")
    )

  return(lost_companies_sector_split)
}

apply_sector_split_to_loans <- function(data,
                                        abcd,
                                        companies_sector_split) {
  unique_companies_pre_split <- data %>%
    distinct(name_abcd)

  abcd_id <- abcd %>%
    dplyr::distinct(.data$company_id, .data$name_company)

  companies_sector_split <- companies_sector_split %>%
    dplyr::left_join(
      abcd_id,
      by = c("company_id")
    ) %>%
    dplyr::select(-"company_id")

  data <- data %>%
    dplyr::inner_join(
      companies_sector_split,
      by = c("name_abcd" = "name_company", "sector_abcd" = "sector")
    ) %>%
    dplyr::mutate(
      # renaming the loan_id is not conditional to avoid any chance of accidentally
      # renaming a split loan to a loan_id that already exists elsewhere
      id_loan = paste(.data$id_loan, .data$sector_abcd, sep = "_"),
      loan_size_outstanding = dplyr::if_else(
        is.na(.data$sector_split),
        .data$loan_size_outstanding,
        .data$loan_size_outstanding * .data$sector_split
      ),
      loan_size_credit_limit = dplyr::if_else(
        is.na(.data$sector_split),
        .data$loan_size_credit_limit,
        .data$loan_size_credit_limit * .data$sector_split
      )
    ) %>%
    dplyr::select(-"sector_split")

  unique_companies_post_split <- data %>%
    distinct(name_abcd)

  if (nrow(unique_companies_pre_split) != nrow(unique_companies_post_split)) {
    n_pre <- nrow(unique_companies_pre_split)
    n_post <- nrow(unique_companies_post_split)
    cli::cli_warn(c(
      "!" = "Applying the sector split has lead to changes in the number of unique companies covered in the analysis.",
      "i" = "Prior to the split, there {?was/were} {.strong {n_pre}} unique compan{?y/ies}.",
      "i" = "After the split, there {?is/are} {.strong {n_post}} unique compan{?y/ies}."
    ))
  }

  return(data)
}

stop_if_not_inherits <- function(x, cls) {
  if (isFALSE(inherits(x, cls))) {
    arg <- deparse(substitute(x))
    cli::cli_abort(
      message = paste0(
        "x" = "Argument {.arg {arg}} must inherit class {.cls {cls}}, ",
        "not {.cls {class(x)}}."
      ),
      call = rlang::caller_env()
    )
  }
}

stop_if_not_length <- function(x, len) {
  if (length(x) != len) {
    arg <- deparse(substitute(x))
    cli::cli_abort(
      message = paste0(
        "x" = "Argument {.arg {arg}} must be of length {.strong {len}}, ",
        " not {.strong {length(x)}}."
      ),
      call = rlang::caller_env()
    )
  }
}

stop_if_dir_not_found <- function(x, desc = NULL) {
  if (isFALSE(dir.exists(x))) {
    if (is.null(desc)) {
      msg <- "Directory not found at path:"
    } else {
      msg <- "{desc} directory not found at path:"
    }
    cli::cli_abort(
      message = c(
        "x" = msg,
        " " = "{.path {x}}",
        "i" = "Check the path set in your {.file config.yml}."
      ),
      call = rlang::caller_env()
    )
  }
}

stop_if_file_not_found <- function(x, desc = NULL) {
  if (isFALSE(file.exists(x))) {
    if (is.null(desc)) {
      msg <- "File not found at path:"
    } else {
      msg <- "{desc} file not found at path:"
    }
    cli::cli_abort(
      message = c(
        "x" = msg,
        " " = "{.file {x}}",
        "i" = "Check the path and filename set in your {.file config.yml}."
      ),
      call = rlang::caller_env()
    )
  }
}

stop_if_sheet_not_found <- function(sheet, path) {
  if (isFALSE(sheet %in% readxl::excel_sheets(path))) {
    cli::cli_abort(
      message = c(
        "x" = "Sheet {.val {sheet}} is not found in file:",
        " " = "{.file {path}}",
        "i" = "Check the sheet name set in your {.file config.yml}."
      ),
      call = rlang::caller_env()
    )
  }
}

stop_if_not_expected_columns <- function(data, cols, desc = NULL) {
  if (isFALSE(all(cols %in% names(data)))) {
    if (is.null(desc)) {
      msg <- "Data does not contain all of the expected columns."
    } else {
      msg <- "{desc} data does not contain all of the expected columns."
    }
    missing_cols <- setdiff(cols, names(data))
    cli::cli_abort(
      message = c(
        "x" = msg,
        "i" = "missing columns: {.var {missing_cols}}."
      ),
      call = rlang::caller_env()
    )
  }
}
