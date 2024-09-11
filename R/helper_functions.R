lost_companies_sector_split <- function(abcd, companies_sector_split) {
  abcd_id <-
    dplyr::distinct(
      .data = abcd,
      .data[["company_id"]],
      .data[["name_company"]]
    )

  dplyr::anti_join(
    x = companies_sector_split,
    y = abcd_id,
    by = c("company_id")
  )
}

apply_sector_split_to_loans <- function(data,
                                        abcd,
                                        companies_sector_split) {
  unique_companies_pre_split <- dplyr::distinct(.data = data, "name_abcd")

  abcd_id <-
    dplyr::distinct(
      .data = abcd,
      .data[["company_id"]],
      .data[["name_company"]]
    )

  companies_sector_split <-
    dplyr::left_join(
      x = companies_sector_split,
      y = abcd_id,
      by = c("company_id")
    ) %>%
    dplyr::select(-"company_id")

  data <-
    dplyr::inner_join(
      x = data,
      y = companies_sector_split,
      by = c("name_abcd" = "name_company", "sector_abcd" = "sector")
    ) %>%
    dplyr::mutate(
      # renaming the loan_id is not conditional to avoid any chance of accidentally
      # renaming a split loan to a loan_id that already exists elsewhere
      id_loan = paste(.data[["id_loan"]], .data[["sector_abcd"]], sep = "_"),
      loan_size_outstanding = dplyr::if_else(
        is.na(.data[["sector_split"]]),
        .data[["loan_size_outstanding"]],
        .data[["loan_size_outstanding"]] * .data[["sector_split"]]
      ),
      loan_size_credit_limit = dplyr::if_else(
        is.na(.data[["sector_split"]]),
        .data[["loan_size_credit_limit"]],
        .data[["loan_size_credit_limit"]] * .data[["sector_split"]]
      )
    ) %>%
    dplyr::select(-"sector_split")

  unique_companies_post_split <-
    dplyr::distinct(
      .data = data,
      .data[["name_abcd"]]
    )

  if (nrow(unique_companies_pre_split) != nrow(unique_companies_post_split)) {
    n_pre <- nrow(unique_companies_pre_split)
    n_post <- nrow(unique_companies_post_split)
    cli::cli_warn(c(
      "!" = "Applying the sector split has lead to changes in the number of unique companies covered in the analysis.",
      "i" = "Prior to the split, there {?was/were} {.strong {n_pre}} unique compan{?y/ies}.",
      "i" = "After the split, there {?is/are} {.strong {n_post}} unique compan{?y/ies}."
    ))
  }

  data
}

check_and_prepare_by_group <- function(by_group) {
  if (!is.null(by_group)) {
    stop_if_not_inherits(by_group, "character")
    stop_if_not_length(by_group, 1)
    if (by_group == "NULL") {
      by_group <- NULL
    } else {
      by_group
    }
  }
}
