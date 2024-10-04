test_that("", {
  config <-
    list(
      directories = list(
        dir_input = test_path("test-data", "input"),
        dir_output = test_path("test-data", "output")
      ),
      project_parameters = list(
        scenario_source = "weo_2022",
        scenario_select = "nze_2050",
        region_select = "global",
        start_year = 2022L,
        time_frame = 5L,
        by_group = "group_id"
      ),
      sector_split = list(
        apply_sector_split = TRUE,
        sector_split_type = "equal_weights",
        filename_split_company_id = "split_company_ids.csv",
        filename_advanced_company_indicators = "test-cmpy-indicators.xlsx",
        sheet_advanced_company_indicators = "Company Activities"
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
      ),
      match_prioritize = list(
        priority =  NULL
      ),
      match_success_rate = list(
        plot_width = 12L,
        plot_height = 8L,
        plot_units = "in",
        plot_resolution = 300L
      )
    )
  
  expect_no_error(suppressWarnings(prioritise_and_diagnose(config)))
})
