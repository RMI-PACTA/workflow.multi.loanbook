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
    dplyr::filter(.data[["code_system"]] == "NACE")

  test_raw_lbk_with_sectors <- add_sectors_to_raw_lbk(
    raw_lbk = test_raw,
    sector_classification_system = nace_sectors
  )

  test_matched_prio <- test_raw %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1)

  test_lbk_match_success <- combine_raw_and_matched_loan_books(
    raw_lbk_with_sectors = test_raw_lbk_with_sectors,
    matched_prioritized = test_matched_prio,
    by_group = "group_id"
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

test_that("combine_raw_and_matched_loan_books removes group_id from id_loan where needed (GH #90)", {
  test_raw <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test")

  nace_sectors <- r2dii.data::sector_classifications %>%
    dplyr::filter(.data[["code_system"]] == "NACE")

  test_raw_lbk_with_sectors <- add_sectors_to_raw_lbk(
    raw_lbk = test_raw,
    sector_classification_system = nace_sectors
  )

  test_matched_prio <- test_raw %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::mutate(id_loan = paste(id_loan, group_id, sep = "_"))

  test_lbk_match_success <- combine_raw_and_matched_loan_books(
    raw_lbk_with_sectors = test_raw_lbk_with_sectors,
    matched_prioritized = test_matched_prio,
    by_group = "group_id"
  )

  testthat::expect_equal(test_raw$id_loan, unique(test_lbk_match_success$id_loan))

  testthat::expect_in(
    gsub("_test", "", test_matched_prio$id_loan),
    test_raw$id_loan
  )
})

test_that("combine_raw_and_matched_loan_books removes sector_abcd from id_loan where needed (GH #90)", {
  test_raw <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test")

  nace_sectors <- r2dii.data::sector_classifications %>%
    dplyr::filter(.data[["code_system"]] == "NACE")

  test_raw_lbk_with_sectors <- add_sectors_to_raw_lbk(
    raw_lbk = test_raw,
    sector_classification_system = nace_sectors
  )

  test_matched_prio <- test_raw %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::mutate(id_loan = paste(id_loan, sector_abcd, sep = "_"))

  test_lbk_match_success <- combine_raw_and_matched_loan_books(
    raw_lbk_with_sectors = test_raw_lbk_with_sectors,
    matched_prioritized = test_matched_prio,
    by_group = "group_id"
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

# calculate_match_success_rate
test_that("calculate_match_success_rate returns results for each group of variable by_group available in matched_prioritized", {
  test_raw_meta <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test") %>%
    dplyr::mutate(meta = "meta")

  test_raw_group_id <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test")

  test_raw_foo <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test") %>%
    dplyr::mutate(
      foo = dplyr::if_else(
        as.numeric(row.names(.)) %% 2 == 0, "bar", "baz"
      )
    )

  test_matched_prio_meta <- test_raw_meta %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::filter(.data[["level"]] == "direct_loantaker")

  test_matched_prio_group_id <- test_raw_group_id %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::filter(.data[["level"]] == "direct_loantaker")

  test_matched_prio_foo <- test_raw_foo %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::filter(.data[["level"]] == "direct_loantaker")

  nace_sectors <- r2dii.data::sector_classifications %>%
    dplyr::filter(.data[["code_system"]] == "NACE")

  lbk_match_success_rate_meta <- calculate_match_success_rate(
    raw_lbk = test_raw_meta,
    matched_prioritized = test_matched_prio_meta,
    sector_classification_system = nace_sectors,
    misclassfied_loans = NULL,
    by_group = "meta"
  )

  lbk_match_success_rate_group_id <- calculate_match_success_rate(
    raw_lbk = test_raw_group_id,
    matched_prioritized = test_matched_prio_group_id,
    sector_classification_system = nace_sectors,
    misclassfied_loans = NULL,
    by_group = "group_id"
  )

  lbk_match_success_rate_foo <- calculate_match_success_rate(
    raw_lbk = test_raw_foo,
    matched_prioritized = test_matched_prio_foo,
    sector_classification_system = nace_sectors,
    misclassfied_loans = NULL,
    by_group = "foo"
  )

  testthat::expect_contains(names(lbk_match_success_rate_meta), "meta")
  testthat::expect_contains(names(lbk_match_success_rate_group_id), "group_id")
  testthat::expect_contains(names(lbk_match_success_rate_foo), "foo")

  testthat::expect_contains(unique(lbk_match_success_rate_meta[["meta"]]), unique(test_matched_prio_meta[["meta"]]))
  testthat::expect_contains(unique(lbk_match_success_rate_group_id[["group_id"]]), unique(test_matched_prio_group_id[["group_id"]]))
  testthat::expect_contains(unique(lbk_match_success_rate_foo[["foo"]]), unique(test_matched_prio_foo[["foo"]]))
})

test_that("calculate_match_success_rate can only be calculated for by_groups that are available the raw and the matched priortized loan books", {
  test_raw_meta <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test") %>%
    dplyr::mutate(meta = "meta")

  test_raw_foo <- r2dii.data::loanbook_demo %>%
    dplyr::mutate(group_id = "test") %>%
    dplyr::mutate(
      foo = dplyr::if_else(
        as.numeric(row.names(.)) %% 2 == 0, "bar", "baz"
      )
    )

  test_matched_prio_meta <- test_raw_meta %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::filter(.data[["level"]] == "direct_loantaker")

  test_matched_prio_foo <- test_raw_foo %>%
    r2dii.match::match_name(abcd = r2dii.data::abcd_demo, by_sector = TRUE, min_score = 1) %>%
    dplyr::filter(.data[["level"]] == "direct_loantaker")

  nace_sectors <- r2dii.data::sector_classifications %>%
    dplyr::filter(.data[["code_system"]] == "NACE")

  testthat::expect_no_condition(
    calculate_match_success_rate(
      raw_lbk = test_raw_meta,
      matched_prioritized = test_matched_prio_meta,
      sector_classification_system = nace_sectors,
      misclassfied_loans = NULL,
      by_group = "meta"
    )
  )

  testthat::expect_no_condition(
    calculate_match_success_rate(
      raw_lbk = test_raw_foo,
      matched_prioritized = test_matched_prio_foo,
      sector_classification_system = nace_sectors,
      misclassfied_loans = NULL,
      by_group = "foo"
    )
  )

  testthat::expect_condition(
    calculate_match_success_rate(
      raw_lbk = test_raw_foo,
      matched_prioritized = test_matched_prio_meta,
      sector_classification_system = nace_sectors,
      misclassfied_loans = NULL,
      by_group = "foo"
    )
  )

  testthat::expect_condition(
    calculate_match_success_rate(
      raw_lbk = test_raw_foo,
      matched_prioritized = test_matched_prio_meta,
      sector_classification_system = nace_sectors,
      misclassfied_loans = NULL,
      by_group = "meta"
    )
  )

  testthat::expect_condition(
    calculate_match_success_rate(
      raw_lbk = test_raw_meta,
      matched_prioritized = test_matched_prio_meta,
      sector_classification_system = nace_sectors,
      misclassfied_loans = NULL,
      by_group = "foo"
    )
  )

  testthat::expect_condition(
    calculate_match_success_rate(
      raw_lbk = test_raw_foo,
      matched_prioritized = test_matched_prio_foo,
      sector_classification_system = nace_sectors,
      misclassfied_loans = NULL,
      by_group = "meta"
    )
  )
})
