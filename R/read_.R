read_abcd_raw <- function(path, sheet) {
  abcd <-
    readxl::read_xlsx(
      path = file.path(path),
      sheet = sheet,
      col_types = c(
        company_id = "numeric",
        name_company = "text",
        lei = "text",
        is_ultimate_owner = "logical",
        sector = "text",
        technology = "text",
        plant_location = "text",
        year = "numeric",
        production = "numeric",
        production_unit = "text",
        emission_factor = "numeric",
        emission_factor_unit = "text",
        ald_timestamp = "skip"
      )
    )

  dplyr::mutate(abcd, year = as.integer(.data[["year"]]))
}
