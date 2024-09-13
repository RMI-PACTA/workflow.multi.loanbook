# Physical energy content and primary energy efficiency.
# Power generation based on heat/combustion causes a loss of a share of primary
# energy. To back calculate the primary energy content based on electricity
# metrics, such as power generation, therefore requires dividing the power
# capacity by this efficiency factor to derive the physical energy input.
# We apply such factors for fossil fuel based power generation only, since we
# are interested in approximating the exposure to fossil fuels.
# Values for efficiency of electricity production are taken from IEA "Energy
# Efficiency Indicators for Public Electricity Prodcution from Fossil Fuels" at
# https://iea.blob.core.windows.net/assets/acaecb98-4430-4395-a4fa-d1a4d5ccb3d3/EnergyEfficiencyIndicatorsforPublicElectricityProductionfromFossilFuels.pdf
# last accessed on 15 March, 2023.

primary_energy_efficiency <- dplyr::tribble(
  ~region,  ~sector,     ~technology, ~primary_energy_efficiency_factor,
  "global", "power",       "coalcap",                             0.343,
  "global", "power",        "gascap",                             0.395,
  "global", "power",        "oilcap",                             0.365,
  "global", "power",      "hydrocap",                                 1,
  "global", "power",    "nuclearcap",                                 1,
  "global", "power", "renewablescap",                                 1
)


# unit conversions are taken from: http://wds.iea.org/wds/pdf/WORLDBAL_Documentation.pdf
# last accessed on 27 Feb 2023

unit_conversion <- dplyr::tribble(
  ~sector,       ~unit,             ~value_in_mtoe,
  "coal",        "t coal",          7e-07,
  "oil and gas", "GJ",              2.3885e-08,
  "power",       "MWh",             8.598e-08
)


# activity units

library(dplyr)

sda_sectors <- c("aviation", "cement", "steel")

activity_units <-
  r2dii.data::abcd_demo %>%
  dplyr::distinct(
    .data[["sector"]],
    .data[["technology"]],
    .data[["production_unit"]],
    .data[["emission_factor_unit"]]
  ) %>%
  dplyr::rename(activity_unit = "production_unit") %>%
  dplyr::mutate(
    activity_unit = dplyr::if_else(
      .data[["sector"]] %in% .env[["sda_sectors"]],
      .data[["emission_factor_unit"]],
      .data[["activity_unit"]]
    )
  ) %>%
  dplyr::select(-"emission_factor_unit")

usethis::use_data(activity_units, internal = TRUE, overwrite = TRUE)

usethis::use_data(
  activity_units, primary_energy_efficiency, unit_conversion,
  overwrite = TRUE,
  internal = TRUE
)
