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
    warning(
      glue::glue(
        "Applying the sector split has lead to changes in the number of unique
        companies covered in the analysis. Prior to the split, there were
        {nrow(unique_companies_pre_split)} unique companies. After the split,
        there are {nrow(unique_companies_post_split)} unique companies."
      )
    )
  }

  return(data)
}
