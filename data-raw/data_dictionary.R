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
  "company_technology_deviation_tms", "direction", "character", "The direction the technology follows long term in the scenario", "One of the following: 'buildout', 'phaseout'",
  "company_technology_deviation_tms", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power"
)

dd_company_alignment_net_tms <- dplyr::tribble(
  ~dataset,  ~column,     ~typeof, ~definition, ~value,
  "company_alignment_net_tms", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_alignment_net_tms", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "company_alignment_net_tms", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power",
  "company_alignment_net_tms", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_alignment_net_tms", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_alignment_net_tms", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "company_alignment_net_tms", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_alignment_net_tms", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'net'",
  "company_alignment_net_tms", "total_deviation", "double", "Net aggregate deviation of all underlying technologies, accounting for directionality. A more positive number means that the combination of underlying technologies is more aligned, a more negative one implies higher misalignment", "Numerical value. Can be negative or positive",
  "company_alignment_net_tms", "alignment_metric", "double", "Net aggregate alignment value by sector. A ratio that is calculated as the total deviation divided by the net scenario value (accounting for directionality). A positive value shows the company plans are ahead of target. A negative value means they are behind target", "Numerical value. Can be negative or positive"
)

dd_company_alignment_bo_po_tms <- dplyr::tribble(
  ~dataset,  ~column,     ~typeof, ~definition, ~value,
  "company_alignment_bo_po_tms", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_alignment_bo_po_tms", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "company_alignment_bo_po_tms", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power",
  "company_alignment_bo_po_tms", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_alignment_bo_po_tms", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_alignment_bo_po_tms", "scenario", "character", "The name of the scenario against which alignment is measured", "Must be available in the input scenario data. Must be a scenario provided in the indicated 'scenario_source'",
  "company_alignment_bo_po_tms", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_alignment_bo_po_tms", "direction", "character", "At the sector level, 'direction' indicates if the alignment value is aggregated across buildout or phaseout technologies, or if it is the net aggregate of the sector.", "In this case, must be: 'buildout' or 'phaseout'",
  "company_alignment_bo_po_tms", "total_deviation", "double", "Aggregate deviation of all underlying technologies of the same directionality. A more positive number means that the combination of underlying technologies is more aligned, a more negative one implies higher misalignment", "Numerical value. Can be negative or positive",
  "company_alignment_bo_po_tms", "alignment_metric", "double", "Net aggregate alignment value by sector, disaggregated by direction of the underlying technlogies. A ratio that is calculated as the total deviation divided by the net scenario value (accounting for directionality). A positive value shows the company plans are ahead of target. A negative value means they are behind target", "Numerical value. Can be negative or positive"
)

# TODO
dd_company_alignment_net_sda <- dplyr::tribble(
  ~dataset,  ~column,     ~typeof, ~definition, ~value,
  "company_alignment_net_sda", "sector", "character", "The sector of the technology", "One of the following: 'power', 'automotive', 'coal', 'oil and gas'",
  "company_alignment_net_sda", "technology", "character", "The technology", "One of the in-scope PACTA technologies that belong to the sector indicated in 'sector'",
  "company_alignment_net_sda", "year", "integer", "The year of the data", "A year between the 'start_year' of the analyis and the 'start_year' plus the 'time_frame'",
  "company_alignment_net_sda", "region", "character", "The region for which the analysis has been run. Indicates which production assets have been considered and which scenario region is used", "Must be a value available in the input scenario data",
  "company_alignment_net_sda", "scenario_source", "character", "The publication the scenario data is based on", "Must be available in the input scenario data. Usually, available sources are: 'weo', 'geco', 'isf'. Usually follows the pattern '<source>_<publication_year>'",
  "company_alignment_net_sda", "name_abcd", "character", "The name of the company", "The name of the company",
  "company_alignment_net_sda", "projected", "double", "The projected activity level of the technology in the year for the given company", "Numerical value greater or equal to 0",
  "company_alignment_net_sda", "target_<scenario>", "double", "The target activity level of the technology in the year for the given company", "Numerical value greater or equal to 0",
  "company_alignment_net_sda", "total_tech_deviation", "double", "The total difference between 'target_<scenario>' and 'projected'", "Numerical value. Can be negative or positive",
  "company_alignment_net_sda", "direction", "character", "The direction of the technology. Follows long term scenario requirements", "One of the following: 'buildout', 'phaseout'",
  "company_alignment_net_sda", "activity_unit", "character", "The unit the activity is measured in for a given sector", "The unit corresponding to the sector. For example 'MW' of capacity for power"
)

# TODO: company_exposure_bo_po_aggregate_alignment<_by_...>
# TODO: company_exposure_net_aggregate_alignment<_by_...>
# TODO: loanbook_exposure_bo_po_aggregate_alignment<_by_...>
# TODO: loanbook_exposure_net_aggregate_alignment<_by_...>

data_dictionary <- dplyr::bind_rows(
  dd_company_technology_deviation_tms,
  dd_company_alignment_net_tms,
  dd_company_alignment_bo_po_tms,
  dd_company_alignment_net_sda
)

usethis::use_data(data_dictionary, overwrite = TRUE)
