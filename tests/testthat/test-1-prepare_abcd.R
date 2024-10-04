test_that("", {
  config <-
    list(
      directories = list(
        dir_input = test_path("test-data", "input"),
        dir_output = test_path("test-data", "output")
      ),
      file_names = list(
        filename_abcd = "test-banks-data.xlsx",
        sheet_abcd = "Company Indicators - PACTA Comp"
      ),
      project_parameters = list(
        start_year = 2022L,
        time_frame = 5L
      ),
      sector_split = list(
        apply_sector_split = TRUE,
        sector_split_type = "equal_weights",
        filename_split_company_id = "split_company_ids.csv",
        filename_advanced_company_indicators = "test-cmpy-indicators.xlsx",
        sheet_advanced_company_indicators = "Company Activities"
      ),
      prepare_abcd = list(
        remove_inactive_companies = TRUE
      )
    )
  
  expect_no_condition(prepare_abcd(config))
})
