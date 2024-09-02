# combine_raw_and_matched_loan_books
test_that("combine_raw_and_matched_loan_books identifies correct matched and unmatched loans", {
  test_raw <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test")

  possible_matches_direct <- test_raw %>%
    dplyr::distinct(.data[["id_loan"]], .data[["name_direct_loantaker"]]) %>%
    dplyr::semi_join(r2dii.data::abcd_demo, by = c("name_direct_loantaker" = "name_company"))

  non_matches_direct <- test_raw %>%
    dplyr::distinct(.data[["id_loan"]], .data[["name_direct_loantaker"]]) %>%
    dplyr::anti_join(r2dii.data::abcd_demo, by = c("name_direct_loantaker" = "name_company"))

  test_raw <- test_raw %>%
    dplyr::filter(
      .data[["id_loan"]] %in% possible_matches_direct[["id_loan"]][1] |
        .data[["id_loan"]] %in% non_matches_direct[["id_loan"]][1]
    )

  nace_sectors <- r2dii.data::sector_classifications %>%
    dplyr::filter(.data$code_system == "NACE")

  test_raw_lbk_with_sectors <- add_sectors_to_raw_lbk(
    raw_lbk = test_raw,
    sector_classification_system = nace_sectors
  )

  test_matched_prio <- test_raw %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1)

  test_lbk_match_success <- combine_raw_and_matched_loan_books(
    raw_lbk_with_sectors = test_raw_lbk_with_sectors,
    matched_prioritized = test_matched_prio
  )

  matched <- test_lbk_match_success %>%
    dplyr::filter(.data[["matched"]] == "Matched") %>%
    dplyr::distinct(.data[["id_loan"]], .data[["name_direct_loantaker"]])

  testthat::expect_equal(matched, possible_matches_direct[1, ])

  not_matched <- test_lbk_match_success %>%
    dplyr::filter(.data[["matched"]] == "Not matched") %>%
    dplyr::distinct(.data[["id_loan"]], .data[["name_direct_loantaker"]])

  testthat::expect_equal(not_matched, non_matches_direct[1, ])
})

test_that("combine_raw_and_matched_loan_books removes group_id from id_loan where needed", {
  test_raw <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test")

  nace_sectors <- r2dii.data::sector_classifications %>%
    dplyr::filter(.data$code_system == "NACE")

  test_raw_lbk_with_sectors <- add_sectors_to_raw_lbk(
    raw_lbk = test_raw,
    sector_classification_system = nace_sectors
  )

  test_matched_prio <- test_raw %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::mutate(id_loan = paste(id_loan, group_id, sep = "_"))

  test_lbk_match_success <- combine_raw_and_matched_loan_books(
    raw_lbk_with_sectors = test_raw_lbk_with_sectors,
    matched_prioritized = test_matched_prio
  )

  testthat::expect_equal(test_raw$id_loan, unique(test_lbk_match_success$id_loan))

  testthat::expect_in(
    gsub("_test", "", test_matched_prio$id_loan),
    test_raw$id_loan
  )
})

test_that("combine_raw_and_matched_loan_books removes sector_abcd from id_loan where needed", {
  test_raw <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test")

  nace_sectors <- r2dii.data::sector_classifications %>%
    dplyr::filter(.data$code_system == "NACE")

  test_raw_lbk_with_sectors <- add_sectors_to_raw_lbk(
    raw_lbk = test_raw,
    sector_classification_system = nace_sectors
  )

  test_matched_prio <- test_raw %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::mutate(id_loan = paste(id_loan, sector_abcd, sep = "_"))

  test_lbk_match_success <- combine_raw_and_matched_loan_books(
    raw_lbk_with_sectors = test_raw_lbk_with_sectors,
    matched_prioritized = test_matched_prio
  )

  testthat::expect_equal(
    test_raw$id_loan,
    unique(test_lbk_match_success$id_loan)
  )

  testthat::expect_in(
    gsub(paste0(test_raw$id_loan, "_", collapse = "|"), "", test_matched_prio$id_loan),
    unique(r2dii.data::sector_classifications$sector)
  )
})
