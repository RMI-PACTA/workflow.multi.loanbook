prepare_abcd <- function(config) {
  config <- load_config(config)

  abcd_dir <- get_abcd_dir(config)
  path_abcd <- get_abcd_path(config)
  sheet_abcd <- get_abcd_sheet(config)

  remove_inactive_companies <- get_remove_inactive_companies(config)
  start_year <- get_start_year(config)
  time_frame <- get_time_frame(config)

  # validate config values----
  stop_if_not_length(path_abcd, 1L)
  stop_if_not_inherits(path_abcd, "character")
  stop_if_file_not_found(path_abcd, desc = "ABCD")

  stop_if_not_length(sheet_abcd, 1L)
  stop_if_not_inherits(sheet_abcd, "character")
  stop_if_sheet_not_found(sheet_abcd, path_abcd)

  if (!is.null(remove_inactive_companies)) {
    stop_if_not_length(remove_inactive_companies, 1L)
    stop_if_not_inherits(remove_inactive_companies, "logical")
  }

  stop_if_not_length(start_year, 1L)
  stop_if_not_inherits(start_year, "integer")

  stop_if_not_length(time_frame, 1L)
  stop_if_not_inherits(time_frame, "integer")


  # load data----
  abcd <- read_abcd_raw(path_abcd, sheet_abcd)
  stop_if_not_expected_columns(abcd, cols_abcd, desc = "ABCD")

  # optional: remove inactive companies----

  # (1) remove company-sector combinations where production in t5 = 0 when
  # it was greater than 0 in t0.
  # (2) remove company-sector combinations where production is 0 for the entire
  # time frame from t0 to t5.
  rm_inactive_companies <- function(data,
                                    start_year,
                                    time_frame) {
    comp_sec_no_prod_t5 <- data %>%
      dplyr::filter(
        .data[["year"]] %in% c(.env[["start_year"]], .env[["start_year"]] + .env[["time_frame"]])
      ) %>%
      dplyr::summarise(
        sum_production = sum(.data[["production"]], na.rm = TRUE),
        .by = c("name_company", "sector", "year")
      ) %>%
      tidyr::pivot_wider(
        names_from = "year",
        names_prefix = "prod_",
        values_from = "sum_production"
      ) %>%
      dplyr::filter(
        .data[[paste0("prod_", start_year)]] > 0,
        .data[[paste0("prod_", start_year + time_frame)]] == 0
      ) %>%
      dplyr::distinct(
        .data[["name_company"]],
        .data[["sector"]]
      )

    comp_sec_no_prod_t0_to_t5 <- data %>%
      dplyr::filter(
        .data[["year"]] %in% c(.env[["start_year"]], .env[["start_year"]] + .env[["time_frame"]])
      ) %>%
      dplyr::summarise(
        sum_production = sum(.data[["production"]], na.rm = TRUE),
        .by = c("name_company", "sector")
      ) %>%
      dplyr::filter(
        .data[["sum_production"]] == 0
      ) %>%
      dplyr::distinct(
        .data[["name_company"]],
        .data[["sector"]]
      )

    data <- data %>%
      dplyr::anti_join(
        comp_sec_no_prod_t5,
        by = c("name_company", "sector")
      ) %>%
      dplyr::anti_join(
        comp_sec_no_prod_t0_to_t5,
        by = c("name_company", "sector")
      )

    return(data)
  }

  if (remove_inactive_companies) {
    abcd_keep <- abcd %>%
      rm_inactive_companies(
        start_year = start_year,
        time_frame = time_frame
      )

    abcd_removed <- abcd %>%
      dplyr::anti_join(
        abcd_keep,
        by = c("company_id", "sector")
      )

    # write removed inactive companies to file for inspection
    abcd_removed %>%
      readr::write_csv(
        file.path(abcd_dir, "abcd_removed_inactive_companies.csv"),
        na = ""
      )

    abcd <- abcd_keep

    rm(abcd_keep)
  }

  # write final version of abcd to file for use PACTA analysis
  abcd %>%
    readr::write_csv(
      file.path(abcd_dir, "abcd_final.csv"),
      na = ""
    )
}
