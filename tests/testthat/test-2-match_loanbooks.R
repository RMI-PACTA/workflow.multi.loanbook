test_that("", {
  config <-
    list(
      directories = list(
        dir_input = test_path("test-data", "input"),
        dir_output = test_path("test-data", "output")
      ),
      matching = list(
        params_match_name = list(
          by_sector = TRUE,
          min_score = 0.9,
          method = "jw",
          p = 0.1,
          overwrite = NULL,
          join_id = NULL
        ),
        manual_sector_classification = list(
          use_manual_sector_classification = FALSE
        )
      )
    )

  expect_no_error(suppressMessages(match_loanbooks(config)))
})
