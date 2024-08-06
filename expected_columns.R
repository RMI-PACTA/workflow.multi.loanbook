# expected columns region isos file
col_types_region_isos <- readr::cols_only(
  region = "c",
  isos = "c",
  source = "c"
)
col_select_region_isos <- names(col_types_region_isos[["cols"]])

# expected columns tms scenario file
col_types_scenario_tms <- readr::cols_only(
  scenario_source = "c",
  region = "c",
  scenario = "c",
  sector = "c",
  technology = "c",
  year = "i",
  smsp = "n",
  tmsr = "n"
)
col_select_scenario_tms <- names(col_types_scenario_tms[["cols"]])

# expected columns sda scenario file
col_types_scenario_sda <- readr::cols_only(
  scenario_source = "c",
  region = "c",
  scenario = "c",
  sector = "c",
  year = "i",
  emission_factor = "n",
  emission_factor_unit = "c"
)
col_select_scenario_sda <- names(col_types_scenario_sda[["cols"]])

# expected columns abcd file (raw and final)
cols_abcd <- c(
  "company_id",
  "name_company",
  "lei",
  "is_ultimate_owner",
  "sector",
  "technology",
  "plant_location",
  "year",
  "production",
  "production_unit",
  "emission_factor",
  "emission_factor_unit"
)

# expected column types final abcd file
col_types_abcd_final <- c(
  company_id = "i",
  name_company = "c",
  lei = "c",
  is_ultimate_owner = "l",
  sector = "c",
  technology = "c",
  plant_location = "c",
  year = "i",
  production = "n",
  production_unit = "c",
  emission_factor = "n",
  emission_factor_unit = "c"
)

# expected columns matched_prioritized_all_groups file
col_types_matched_prioritized <- readr::cols_only(
  group_id = "c",
  id_loan = "c",
  id_direct_loantaker = "c",
  name_direct_loantaker = "c",
  id_ultimate_parent = "c",
  name_ultimate_parent = "c",
  loan_size_outstanding = "n",
  loan_size_outstanding_currency = "c",
  loan_size_credit_limit = "n",
  loan_size_credit_limit_currency = "c",
  sector_classification_system = "c",
  sector_classification_direct_loantaker = "c",
  lei_direct_loantaker = "c",
  isin_direct_loantaker = "c",
  id_2dii = "c",
  level = "c",
  sector = "c",
  sector_abcd = "c",
  name = "c",
  name_abcd = "c",
  score = "n",
  source = "c",
  borderline = "l"
)
col_select_matched_prioritized <- names(col_types_matched_prioritized[["cols"]])

# expected columns matched_all_groups file
# col_types_matched_manual <- readr::cols_only(
col_types_matched_manual <- readr::cols(
  group_id = "c",
  id_loan = "c",
  id_direct_loantaker = "c",
  name_direct_loantaker = "c",
  id_ultimate_parent = "c",
  name_ultimate_parent = "c",
  loan_size_outstanding = "n",
  loan_size_outstanding_currency = "c",
  loan_size_credit_limit = "n",
  loan_size_credit_limit_currency = "c",
  sector_classification_system = "c",
  sector_classification_direct_loantaker = "c",
  lei_direct_loantaker = "c",
  isin_direct_loantaker = "c",
  id_2dii = "c",
  level = "c",
  sector = "c",
  sector_abcd = "c",
  name = "c",
  name_abcd = "c",
  score = "n",
  source = "c",
  borderline = "l"
)
col_select_matched_manual <- names(col_types_matched_manual[["cols"]])

# expected columns raw loan book file
col_types_raw <- readr::cols(
  id_loan = "c",
  id_direct_loantaker = "c",
  name_direct_loantaker = "c",
  id_ultimate_parent = "c",
  name_ultimate_parent = "c",
  loan_size_outstanding = "n",
  loan_size_outstanding_currency = "c",
  loan_size_credit_limit = "n",
  loan_size_credit_limit_currency = "c",
  sector_classification_system = "c",
  sector_classification_direct_loantaker = "c",
  lei_direct_loantaker = "c",
  isin_direct_loantaker = "c"
)

# expected columns companies_sector_split file
col_types_companies_sector_split <- readr::cols_only(
  company_id = "i",
  sector = "c",
  sector_split = "n"
)
col_select_companies_sector_split <- names(col_types_companies_sector_split[["cols"]])

# expected columns companies_sector_split_worst_case file
col_types_companies_sector_split_worst_case <- readr::cols_only(
  name_company = "c",
  sector = "c",
  sector_split = "n"
)
col_select_companies_sector_split_worst_case <- names(col_types_companies_sector_split_worst_case[["cols"]])

# expected columns sector classifications file
col_types_sector_classification <- readr::cols_only(
  code_system = "c",
  code = "c",
  sector = "c",
  borderline = "l"
)
col_select_sector_classification <- names(r2dii.data::sector_classifications)
