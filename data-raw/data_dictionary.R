# Data dictionary for the output table company_technology_deviation_tms
dd_company_technology_deviation_tms <- dplyr::tribble(
  ~dataset,  ~column,     ~typeof, ~definition, ~value,
  "company_technology_deviation_tms", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "company_technology_deviation_tms", "technology", "character", "The technology", "One of the in-scope PACTA technologies that belong to the sector indicated in 'sector'",
  "company_technology_deviation_tms", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_technology_deviation_tms", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_technology_deviation_tms", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_technology_deviation_tms", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_technology_deviation_tms", "projected", "double", "The projected activity level of the technology in the year for the given company", "Numerical value greater or equal to 0",
  "company_technology_deviation_tms", "target_<scenario>", "double", "The target activity level of the technology in the year for the given company", "Numerical value greater or equal to 0",
  "company_technology_deviation_tms", "total_tech_deviation", "double", "The total difference between 'target_<scenario>' and 'projected'", "Numerical value. Can be negative or positive",
  "company_technology_deviation_tms", "direction", "character", "The direction of the technology. Follows long term scenario requirements", "One of the following: 'buildout', 'phaseout'",
  "company_technology_deviation_tms", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power"
)

data_dictionary <- dplyr::bind_rows(dd_company_technology_deviation_tms)

usethis::use_data(data_dictionary, overwrite = TRUE)
